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

(use-modules (ice-9 threads))
(use-modules (ice-9 rdelim))

(define (http-server port)
  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET (inet-aton "127.0.0.1") port)
    (listen s 5)

    (let* ((client-connection (accept s))
           (client-details (cdr client-connection))
           (client (car client-connection)))

      (do ((line (read-line client) (read-line client)))
           ((eof-object? line))
         (display line)
         (newline)))
    (close s)))


(define (sig-handler sig)
  (display sig)
  (set 'want-quit? #t))

(sigaction SIGUSR1 sig-handler)

(http-server 8888)

(define (rguile port)
  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET (inet-pton AF_INET "127.0.0.1") port)
    (listen s 5)

    (let* ((client-connection (accept s))
           (client-details (cdr client-connection))
           (client (car client-connection)))
      (do ((ex (read client) (read client))) ((eof-object? ex))
        (eval ex (interaction-environment))))
    (close s)))

(rguile 8888)
