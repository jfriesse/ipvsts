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

(export ipvsts:report-result ipvsts:check)

;; Report result in format [pass|FAIL] ... form to current-output-port
;; return result
(define (ipvsts:report-result result form)
  (simple-format #t "~A ... ~A\n" (if result "pass" "FAIL") form)
  result)

;; Macro for doing tests
;; Takes body, which is multiple expressions and call "and" function
;; on every ipvsts:report-result with expr 'expr parameters.
(define-macro (ipvsts:check . body)
  `(and
    ,@(map (lambda (x) `(ipvsts:report-result ,x ',x)) body)))
