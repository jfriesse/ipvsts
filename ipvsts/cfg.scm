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

(define-module (ipvsts cfg))

(export cfg set-cfg!)

(define cfg-ht (make-hash-table))

(define (cfg var)
  (hash-ref cfg-ht var))

(define (set-cfg! var val)
  (hash-set! cfg-ht var val))

(define (set-defaults!)
  (set-cfg! 'ipvsts:qemu-img "/usr/bin/qemu-img")
  (set-cfg! 'ipvsts:qemu "/usr/bin/qemu-kvm")
  (set-cfg! 'ipvsts:vm-dir (string-append (getenv "HOME") "/vms"))
  (set-cfg! 'ipvsts:http-mirror "http://download")
  (set-cfg! 'ipvsts:vm-disk-size "1.5G")
  (set-cfg! 'ipvsts:vm-passwd "password")
  (set-cfg! 'test:arch "unknown")
  (set-cfg! 'test:version "unknown")
  (set-cfg! 'test:name "unknown")
  (set-cfg! 'test:install-url "unknown")
  (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-default.log"))
  (set-cfg! 'test:vm:max-install-time (* 60 60))
  (set-cfg! 'test:vm:mem 128)
  (set-cfg! 'test:vm:net 'user)
  (set-cfg! 'test:vm:vnc-base 10)
  (set-cfg! 'test:vm:rguile-port-base 2300))

(define (load-user-defaults!)
  (if (access? (string-append (getenv "HOME") "/.ipvsts") R_OK)
    (load (string-append (getenv "HOME") "/.ipvsts"))))

(set-defaults!)
(load-user-defaults!)
