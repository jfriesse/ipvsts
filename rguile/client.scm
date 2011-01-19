#!/usr/bin/guile -s
!#

;;
;; Copyright (c) 2010-2011, Red Hat, Inc.
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

(define-module (rguile client))

(export rguile-client-unsafe rguile-client-stat rguile-client rguile-client-direct)

;; General remote guile client. This is unsafe version which is
;; evaluted directly, not surrounded with (let () ... ). This means
;; that this is only one which is able to define new symbols.
;; host is host where rguile server is running, port is ip_port of
;; rgule server and to-eval is string to eval.
;; Returned value is list in form of:
;; - (ok) - result was unspecified
;; - (ok result)
;; - (error 'error-code)
(define (rguile-client-unsafe host port to-eval)
  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (connect s AF_INET (inet-pton AF_INET host) port)
        (display to-eval s)

        (read s))
      (lambda () (close s)))))

;; Remote guile client with status return. This is safe version which is
;; surrounded with (let () ... ).
;; host is host where rguile server is running, port is ip_port of
;; rgule server and to-eval is string to eval.
;; Returned value is list in form of:
;; - (ok) - result was unspecified
;; - (ok result)
;; - (error 'error-code)
(define (rguile-client-stat host port to-eval)
  (rguile-client-unsafe host port (simple-format #f "(let () ~A)" to-eval)))

;; Reccomended Remote guile client. This is safe version which calls
;; rguile-client-stat, but returned value is directly transformed to
;; ether *unspecified* or return value or misc-error is thrown on error
;; host is host where rguile server is running, port is ip_port of
;; rgule server and to-eval is string or (symbol/list) to eval.
(define (rguile-client-direct host port to-eval)
  (let ((res
         (cond ((string? to-eval) (rguile-client-stat host port to-eval))
               (#t (rguile-client-stat host port (simple-format #f "~S" to-eval))))))
    (cond ((null? (cdr res)) *unspecified*)
          ((equal? (car res) 'ok) (cadr res))
          ((equal? (car res) 'error) (error (cadr res)))
          (#t (error 'unknown-output)))))

;; Function which return rguile client closure. So it's possible to create
;; this object once and then call with expression parameter.
(define (rguile-client host port)
  (lambda (to-eval)
    (rguile-client-direct host port to-eval)))
