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

(set! %load-path (append %load-path (list ".")))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 rw))
(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts netfuncs))
(use-modules (ipvsts utils))

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version "6.0")
(set-cfg! 'test:name "rhel-6")
(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-6-Server/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))


(define (vminstall:download)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (vmlinuz-path (string-append (cfg 'test:install-url) "/images/pxeboot/vmlinuz"))
         (initrd-path (string-append (cfg 'test:install-url) "/images/pxeboot/initrd.img")))
    (ipvsts:log "creating ~A" vm-dir)
    (mkdir-safe vm-dir)
    (ipvsts:log "download ~A" vmlinuz-path)
    (http-get-file (string-append vm-dir "/vmlinuz") vmlinuz-path)
    (ipvsts:log "download ~A" initrd-path)
    (http-get-file (string-append vm-dir "/initrd.img") initrd-path)))


(define (vminstall:disk-create)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (args (list (cfg 'ipvsts:qemu-img) "create" "-f" "qcow2" (string-append vm-dir "/c.img") (cfg 'ipvsts:vm-disk-size)))
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
;; (vminstall:run-install)
