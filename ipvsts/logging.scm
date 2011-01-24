#!/usr/bin/guile -s
!#

;;
;; Copyright (c) 2011, Red Hat, Inc.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND RED HAT, INC. DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL RED HAT, INC. BE LIABLE
;; FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

;;
;; Author: Jan Friesse <jfriesse@redhat.com>
;;

(define-module (ipvsts logging))
(use-modules (ipvsts cfg))
(export ipvsts:log)

;; Append log message to test:log-file-name
;; Message and args are same as for simple-format are
;; Every message is prepended by call stack
(define (ipvsts:log message . args)
  (define (strace-procs deep fr str)
    (cond ((not fr) str)
          ((and (frame-procedure? fr) (procedure-name (frame-procedure fr)))
           (if (not deep)
               (strace-procs deep (frame-previous fr)
                             (string-append (symbol->string (procedure-name (frame-procedure fr)))
                                            (if (equal? str "") "" "-> ")
                                            str))
               (symbol->string (procedure-name (frame-procedure fr)))))
          (#t (strace-procs deep (frame-previous fr) str))))

  (let* ((f (open-file (cfg 'test:log-file-name) "a"))
         (proc (strace-procs #t (stack-ref (make-stack #t ipvsts:log) 0) "")))
    (apply simple-format (append (list f (string-append "~A ~A: " message "\n")
                                       (strftime "%F %T" (localtime (current-time))) proc) args))
    (close f)))

(debug-enable 'debug)
