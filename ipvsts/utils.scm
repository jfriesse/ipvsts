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

(define-module (ipvsts utils))
(use-modules (ice-9 rw))

(export port-cat mkdir-safe ipvsts:log)

;; Copy content of source-port to destination port dest-port
(define (port-cat source-port dest-port)
  (let* ((str-len 1024)
         (str (make-string str-len)))
    (do ((readed (read-string!/partial str source-port 0 str-len) (read-string!/partial str source-port 0 str-len)))
        ((not readed))
      (write-string/partial str dest-port 0 readed))))

;; Create directory and all subdirectories if not exists
(define (mkdir-safe path)
  (define (iter str-path rest-path)
    (cond ((null? rest-path) #t)
          (#t
           (let ((new-str-path (string-append str-path "/" (car rest-path))))
             (if (not (access? new-str-path F_OK))
                 (mkdir new-str-path))
             (iter new-str-path (cdr rest-path))))))
  (iter "" (string-split path #\/)))

;; Append log message to test:log-file-name
;; Message and args are same as for simple-format are
;; Every message is prepended by call stack
(define (ipvsts:log message . args)
  (define (strace-procs fr str)
    (cond ((not fr) str)
          ((and (frame-procedure? fr) (procedure-name (frame-procedure fr)))
           (strace-procs (frame-previous fr)
                         (string-append (symbol->string (procedure-name (frame-procedure fr)))
                                        (if (equal? str "") "" "-> ")
                                        str)))
          (#t (strace-procs (frame-previous fr) str))))

  (let* ((f (open-file test:log-file-name "a"))
         (proc (strace-procs (stack-ref (make-stack #t ipvsts:log) 0) "")))
    (apply simple-format (append (list f (string-append "~A: " message "\n") proc) args))
    (close f)))
