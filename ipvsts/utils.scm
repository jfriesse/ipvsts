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
        byte->hexstr hash-table->list list->hash-table hash-table-clone
        string-list->string left-char-pad ip4addr->hexstr ip6addr->hexstr
        ipport->hexstr fwmark->hexstr)

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

;; Convert hash table to assoc list
(define (hash-table->list ht)
  (hash-map->list
   (lambda (key val)
     (cons key val))
   ht))

;; Create new hash table and push items form assoc list to this new ht
(define (list->hash-table list)
  (let ((ht (make-hash-table)))
    (for-each
     (lambda (item)
       (hash-set! ht (car item) (cdr item)))
     list)
    ht))

;; Clone hash table
(define (hash-table-clone ht)
  (list->hash-table (hash-table->list ht)))

;; Convert list of strings ("test" "test2") to string inserting
;; sep string inside two items (sep = " " -> "test test2")
(define (string-list->string l sep)
  (define (iter l res)
    (cond ((null? l) res)
          ((= (string-length res) 0) (iter (cdr l) (car l)))
          (#t
           (iter (cdr l) (string-append res sep (car l))))))
  (iter l ""))

;; Pad string str with pad-str from leftside but maximal upto len length
;; so (left-char-pad "f1" "0" 4) -> "00f1"
(define (left-char-pad str pad-str len)
  (cond ((>= (string-length str) len) str)
        (#t (left-char-pad (string-append pad-str str) pad-str len))))

;; Convert IPv4 adddress (string) to hexadecimal value of address where all
;; hex values are upper case
(define (ip4addr->hexstr ip4)
  (string-upcase
   (left-char-pad (number->string (inet-pton AF_INET ip4) 16) "0" 8)))

;; Convert IPv6 address (string) to hexadecimal value of address, where
;; each octets are surrounded with : and all hex values are lowercase
(define (ip6addr->hexstr ip6)
  (define (add-colons str)
    (define (iter res i)
      (cond ((= i (string-length str)) res)
            ((and (= (modulo i 4) 0) (not (= i 0)))
             (iter (string-append res ":" (substring str i (+ i 1)))
                   (+ i 1)))
            (#t (iter (string-append res (substring str i (+ i 1)))
                      (+ i 1)))))
    (iter "" 0))

  (add-colons
   (string-downcase
    (left-char-pad (number->string (inet-pton AF_INET6 ip6) 16) "0" (* 4 8)))))

;; Convert IP port (string) to hexadecimal value of port padded to 4 places
(define (ipport->hexstr port)
  (string-upcase
   (left-char-pad
    (if
     (string? port)
     (number->string (string->number port) 16)
     (number->string port 16)) "0" 4)))

;; Convert firewall mark to hex str. Result value is padded to 8 places by 0.
(define (fwmark->hexstr fwmark)
  (string-upcase
   (left-char-pad
    (if
     (string? fwmark)
     (number->string (string->number fwmark) 16)
     (number->string fwmark 16)) "0" 8)))
