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

(define-module (ipvsts netfuncs))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 rw))
(use-modules (ipvsts utils))

(export uri-parse net-getip net-getport http-get
        httpd:init httpd:accept http-get-file http-serve-string10)

;; Parse uri (protocol://host:port/root_path) string to list in form
;; (protocol host port root_path) or #f if uri is invalid. port may be
;; #f if not entered
(define (uri-parse uri)
  (if (string-index uri #\:)
      (let ((uri2 (substring uri (string-index uri #\:)))
            (proto (substring uri 0 (string-index uri #\:))))
        (if (string-prefix? "://" uri2)
            (let ((uri3 (substring uri2 3)))
              (let ((domain
                     (if (string-index uri3 #\:)
                         (substring uri3 0 (string-index uri3 #\:))
                         (if (string-index uri3 #\/)
                             (substring uri3 0 (string-index uri3 #\/))
                             uri3)))
                    (port
                     (if (not (string-index uri3 #\:))
                         #f
                         (let ((uri4 (substring uri3 (1+ (string-index uri3 #\:)))))
                           (if (string-index uri4 #\/)
                               (substring uri4 0 (string-index uri4 #\/))
                               uri4))))
                    (root_addr
                     (if (not (string-index uri3 #\/))
                         "/"
                         (substring uri3 (string-index uri3 #\/)))))
                (list proto domain port root_addr)))
            #f))
      #f))

;; Return IP value of host.
(define (net-getip host)
  (car (array-ref (gethost host) 4)))

;; Return port in numeric form. Port parameter can be ether
;; number, or string with number or name of service
(define (net-getport port)
  (cond ((number? port) port)
        ((string->number port) (string->number port))
        (#t (array-ref (getserv port "tcp") 2))))

;; Download file from http and display ouput to port
;; Function takes one or three additional parameter(s)
;; If one parameter mode is used, parameter is expected to be valid uri
;; If three parameters mode is used, parameters are host ip_port and root_path
;; Function return #t on success, otherwise #f or exception
(define (http-get port . args)
  (define (get3 host ip_port path)
    (let ((s (socket PF_INET SOCK_STREAM 0)))
      (connect s AF_INET (net-getip host) (net-getport ip_port))
      (simple-format s "GET ~A\r\n" path)

      (let* ((str-len 1024)
             (str (make-string str-len)))
        (do ((readed (read-string!/partial str s 0 str-len) (read-string!/partial str s 0 str-len)))
            ((not readed))
          (write-string/partial str port 0 readed)))
      (close s)
      #t))

  (cond ((null? args) (throw 'wrong-number-of-args))
        ((not (or (= (length args) 1) (= (length args) 3))) (throw 'wrong-number-of-args))
        ((= (length args) 3) (apply get3 args))
        (#t
         (let ((parsed (uri-parse (car args))))
           (cond ((not parsed) #f)
                 ((not (equal? (car parsed) "http")) #f)
                 (#t
                  (let ((host (cadr parsed))
                       (path (cadddr parsed))
                       (ip_port (if (caddr parsed) (caddr parsed) 80)))
                   (get3 host ip_port path))))))))

;; Download file from http with uri and save it to file named path.
(define (http-get-file path uri)
  (let ((f (open-file path "w")))
    (http-get f uri)
    (close f)))

;; Init http daemon. Socket is bound on host:ip-port.
;; Returned value is port which can be used to select on and also for future
;; accepting connection. Port should be closed when not needed.
(define (httpd:init host ip-port)
  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET (inet-pton AF_INET host) ip-port)
    (listen s 5)
    s))

;; Accept connection on port. get-file is procedure which takes two parameters
;; client-port and requested path. get-file should write ouput to client-port and
;; return (not #f) if path is supported, otherwise #t should be returned. Only
;; supported http protocol is 0.9, so only GET command.
(define (httpd:accept port get-file)
  (define (not-found client path)
    (simple-format client "<html><head><title>404 Not Found</title></head>")
    (simple-format client "<body><p>URL ~A was not found.</p></body></html>" path))

  (define (first-line-parse line)
    (let* ((keyword (substring line 0 (string-index line #\ )))
           (path-with-proto (substring line (1+ (string-index line #\ ))))
           (path
            (if (string-index path-with-proto #\ )
                (substring path-with-proto 0 (string-index path-with-proto #\ ))
                path-with-proto)))
      (list keyword path)))

  (define (finish-http-reading s)
    (do ((line (read-line s) (read-line s)))
        ((or (equal? line "") (equal? line "\r")))
      #t))

  (let* ((client-connection (accept port))
         (client-details (cdr client-connection))
         (client (car client-connection)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let* ((first-line (read-line client))
               (method-path (first-line-parse first-line)))
          (finish-http-reading client)
          (cond ((equal? (car method-path) "GET")
                 (if (not (get-file client (cadr method-path)))
                     (not-found client (cadr method-path))))
                (#t (not-found client (cadr method-path))))))
      (lambda () (close client)))))

;; Output str to port with HTTP 1.0 headers
(define (http-serve-string10 port str)
  (display "HTTP/1.0 200 OK\r\n" port)
  (display "Content-Type: text/plain\r\n" port)
  (simple-format port "Content-Length: ~A\r\n\r\n" (string-length str))
  (let ((f (open-input-string str)))
    (port-cat f port)
    (close f)))
