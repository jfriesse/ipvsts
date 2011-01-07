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

(define (http-server host ip-port remote-path)
  (define (create-socket)
    (let ((s (socket PF_INET SOCK_STREAM 0)))
      (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
      (bind s AF_INET (inet-pton AF_INET host) ip-port)
      (listen s 5)
      s))

  (define (first-line-parse line)
    (let* ((keyword (substring line 0 (string-index line #\ )))
           (path-with-proto (substring line (1+ (string-index line #\ ))))
           (path (substring path-with-proto 0 (string-index path-with-proto #\ ))))
      (list keyword path)))

  (define (finish-http-reading s)
    (do ((line (read-line s) (read-line s)))
        ((or (equal? line "") (equal? line "\r")))

      (display line)(newline)
      #t))

  (define (accept-connection s)
    (let* ((client-connection (accept s))
           (client-details (cdr client-connection))
           (client (car client-connection)))
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let* ((first-line (read-line client))
                 (method-path (first-line-parse first-line)))
            (finish-http-reading client)
            (cond ((and (equal? (car method-path) "GET") (equal? (cadr method-path) "/ipvsts.ks"))
                   (let* ((f (open-file "ks.cfg" "r"))
                         (str-len 1024)
                         (str (make-string str-len)))
                     (do ((readed (read-string!/partial str f 0 str-len) (read-string!/partial str f 0 str-len)))
                         ((not readed))
                       (write-string/partial str client 0 readed))))
                  (#t
                                        ;(display "HTTP/1.1 200 OK\r\n" client)
                                        ;   (display "Content-Type:  application/octet-stream\r\n" client)
                                        ;   (display "\r\n" client)
                   (catch #t
                     (lambda ()
                       (http-get client (simple-format #f "~A~A" remote-path (cadr method-path))))
                     (lambda (key . args)
                       #f))))))
        (lambda () (display 'CLOSE) (close client)))))

  (let ((s (create-socket)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (while #t
          (accept-connection s)))
      (lambda () (close s)))))

(http-server "127.0.0.1" 8888 "http://download/pub/rhel/released/RHEL-6/6.0/Server/i386/os/")

(set! %load-path (append %load-path (list ".")))
(use-modules (ice-9 rdelim))
(use-modules (ipvsts netfuncs))
(use-modules (ice-9 rw))
