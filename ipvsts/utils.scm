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

(export port-cat mkdir-safe find-file-in-path find-file-in-lpath
        byte->hexstr)

;; Copy content of source-port to destination port dest-port
;; source and dest ports can be any type of port, but if both of them are file ports,
;; faster block read is used. Otherwise *much* slower read-char/write char is used
(define (port-cat source-port dest-port)
  (define (block-cat)
    (let* ((str-len 1024)
           (str (make-string str-len)))
      (do ((readed (read-string!/partial str source-port 0 str-len)
                   (read-string!/partial str source-port 0 str-len)))
          ((not readed))
        (write-string/partial str dest-port 0 readed))))

  (define (char-cat)
    (do ((readed (read-char source-port) (read-char source-port)))
        ((eof-object? readed))
      (write-char readed dest-port)))

  (cond ((and (file-port? source-port) (file-port? dest-port)) (block-cat))
        (#t (char-cat))))

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

;; Tries to find file in list of paths which can be readed. Returned value is path
(define (find-file-in-path path file)
  (define (iter path)
    (cond ((null? path) #f)
          ((access? (string-append (car path) "/" file) R_OK) (car path))
          (#t (iter (cdr path)))))
  (iter path))

;; Wrapper for find-file-in-path where path is %load-path
(define (find-file-in-lpath file)
  (find-file-in-path %load-path file))

;; Convert byte value (number from 0 to 255) to hex string with leading 0
;; If byte is not byte, exception or #f is returned otherwise string is returned
(define (byte->hexstr byte)
  (if (or (< byte 0) (> byte 255))
      #f
      (let ((str (string-upcase (number->string byte 16))))
        (if (= (string-length str) 1)
            (string-append "0" str)
            str))))
