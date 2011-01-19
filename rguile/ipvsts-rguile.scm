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

(define (rguile-server-tcp port)
  (define (create-socket)
    (let ((s (socket PF_INET SOCK_STREAM 0)))
      (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
      (bind s AF_INET INADDR_ANY port)
      (listen s 5)
      s))

    (define (process-connection s)
      (catch #t
        (lambda ()
          (let ((res (eval (read s) (interaction-environment))))
            (cond ((eq? res *unspecified*) (simple-format s "(ok)"))
                  (#t (simple-format s "(ok ~S)" res))))

          (do ((readed (read-char s) (read-char s)))
              ((eof-object? readed))))
        (lambda (key . args)
          (simple-format s "(error '~A)" key))))

    (define (accept-connections s)
      (while #t
        (catch #t
          (lambda ()
            (let* ((client-connection (accept s))
                   (client-details (cdr client-connection))
                   (client (car client-connection)))
              (dynamic-wind
                (lambda () #t)
                (lambda () (process-connection client))
                (lambda () (close client)))))
        (lambda (key . args)
          #f))))

    (let ((s (create-socket)))
      (dynamic-wind
        (lambda () #t)
        (lambda () (accept-connections s))
        (lambda () (close s)))))

(define (rguile-server-serial file)
    (define (process-connection s)
      (catch #t
        (lambda ()
          (let ((res (eval (read s) (interaction-environment))))
            (cond ((eq? res *unspecified*) (simple-format s "(ok)"))
                  (#t (simple-format s "(ok ~S)" res)))))
        (lambda (key . args)
          (simple-format s "(error '~A)" key))))

    (define (accept-connections s)
      (while #t
        (catch #t
          (lambda ()
            (dynamic-wind
              (lambda () #t)
              (lambda () (process-connection s))
              (lambda () #t)))
        (lambda (key . args)
          #f))))

    (let ((s (open-file file "r+0")))
      (dynamic-wind
        (lambda () #t)
        (lambda () (accept-connections s))
        (lambda () (close s)))))

(define (usage)
  (simple-format (current-error-port) "usage: ~A [port number | /dev/serial-port]\n" (car (command-line)))
  (exit 1))

(define (sig-handler sig)
  (primitive-exit 0))

(sigaction SIGINT sig-handler SA_RESTART)
(sigaction SIGPIPE SIG_IGN)

(cond ((eq? (cdr (command-line)) '()) (usage))
      ((eq? (cddr (command-line)) '())
       (cond ((string->number (cadr (command-line)))
              (let ((port (string->number (cadr (command-line)))))
                (rguile-server-tcp port)))
             ((= (string-prefix-length (cadr (command-line)) "/dev/") (string-length "/dev/"))
                (rguile-server-serial (cadr (command-line))))
             (#t
              (simple-format (current-error-port) "illegal number or /dev/... -- ~A\n" (cadr (command-line)))
              (usage))))
      (#t (usage)))
