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

(set! %load-path (append %load-path (list ".")))
(use-modules (ice-9 rdelim))
(use-modules (ipvsts netfuncs))
(use-modules (ipvsts utils))
(use-modules (ice-9 rw))

(define port (httpd:init "127.0.0.1" 8888))

(define (http-server cl path)
  (cond ((equal? path "/ipvsts.ks")
         (let ((f (open-file "ks.cfg" "r")))
           (port-cat f cl)))
        (#t #f)))

(while #t
(httpd:accept port http-server))

(close port)
