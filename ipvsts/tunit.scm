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

(define-module (ipvsts tunit))

(export ipvsts:report-result ipvsts:check ipvsts:check-list-add ipvsts:check-list-pop)

;; Static variable with tests
(define ipvsts:check-list '())

(define (ipvsts:check-list-add item)
  (set! ipvsts:check-list (append ipvsts:check-list (list item))))

(define (ipvsts:check-list-pop)
  (set! ipvsts:check-list (reverse (cdr (reverse ipvsts:check-list)))))

(define (ipvsts:check-list-get)
  ipvsts:check-list)

;; Report result in format [pass|FAIL] ... form to current-output-port
;; return result
(define (ipvsts:report-result result form)
  (simple-format #t "~A ... ~A: ~A\n" (if result "pass" "FAIL") (ipvsts:check-list-get) form)
  result)

;; Macro for doing tests
;; Takes body, which is multiple expressions and call "and" function
;; on every ipvsts:report-result with expr 'expr parameters.
(define-macro (ipvsts:check test . body)
  `(let ()
     (ipvsts:check-list-add ,test)
     (let ((result
            (and
             ,@(map (lambda (x) `(ipvsts:report-result ,x ',x)) body))))
       (ipvsts:check-list-pop)
       result)))
