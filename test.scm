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

(define ipvsts:qemu-img "/usr/bin/qemu-img")
(define ipvsts:qemu "/usr/bin/qemu-kvm")
(define ipvsts:vm-dir (string-append (getenv "HOME") "/vms"))
(define ipvsts:http-mirror "http://download")
(define ipvsts:vm-disk-size "1.5G")
(define test:arch "i386")
(define test:version "6.0")
(define test:name "rhel-6")
(define test:install-url (string-append ipvsts:http-mirror "/released/RHEL-6-Server/" test:version "/" test:arch "/os"))
(define test:log-file-name (string-append (getenv "HOME") "/ipvsts-" test:name ".log"))

(set! %load-path (append %load-path (list ".")))
(use-modules (ice-9 rdelim))
(use-modules (ipvsts netfuncs))
(use-modules (ipvsts utils))
(use-modules (ice-9 rw))

(define (vminstall:download)
  (let* ((vm-dir (string-append ipvsts:vm-dir "/" test:name))
         (vmlinuz-path (string-append test:install-url "/images/pxeboot/vmlinuz"))
         (initrd-path (string-append test:install-url "/images/pxeboot/initrd.img")))
    (ipvsts:log "creating ~A" vm-dir)
    (mkdir-safe vm-dir)
    (ipvsts:log "download ~A" vmlinuz-path)
    (http-get-file (string-append vm-dir "/vmlinuz") vmlinuz-path)
    (ipvsts:log "download ~A" initrd-path)
    (http-get-file (string-append vm-dir "/initrd.img") initrd-path)))


(define (vminstall:disk-create)
  (let* ((vm-dir (string-append ipvsts:vm-dir "/" test:name))
         (args (list ipvsts:qemu-img "create" "-f" "qcow2" (string-append vm-dir "/c.img") ipvsts:vm-disk-size))
         (stat (apply system* args)))
    (ipvsts:log "creating image ~A return val ~A" args (status:exit-val stat))
    (if (= (status:exit-val stat) 0) #t #f)))

(define (vminstall:run-install)
  (define (http-server cl path)
    (cond ((equal? path "/ipvsts.ks")
           (let ((f (open-file "ks.cfg" "r")))
             (port-cat f cl)))
          (#t #f)))

  (let* ((vm-dir (string-append ipvsts:vm-dir "/" test:name))
         (args (list ipvsts:qemu "-kernel" (string-append vm-dir "/vmlinuz")
                    "-initrd" (string-append vm-dir "/initrd.img")
                    "-hda" (string-append vm-dir "/c.img")
                    "-m" "512" "-net" "nic,model=virtio" "-net" "user"
                    "-append" "ks=http://10.0.2.2:8888/ipvsts.ks"
                    "-vnc" ":11"))
         (pid (primitive-fork)))
    (cond ((= pid 0)
           (let ()
             (ipvsts:log "installing vm ~A" args)
             (apply system* args)
             (exit 0)))
          (#t
           (let ((http-port (httpd:init "127.0.0.1" 8888)))
             (ipvsts:log "waiting for qemu to download kickstart")
             (httpd:accept http-port http-server)
             (ipvsts:log "kickstart downloaded")
             (close http-port)
             (waitpid pid))))))

(vminstall:download)
(vminstall:disk-create)
(vminstall:run-install)
