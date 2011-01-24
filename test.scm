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

(set! %load-path (append %load-path (list "." "/usr/local/share/ipvsts" "/usr/share/ipvsts")))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 rw))
(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts netfuncs))
(use-modules (ipvsts utils))
(use-modules (ipvsts tunit))
(use-modules (rguile client))

(use-modules (tests vminstall))

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version "6.0")
(set-cfg! 'test:name "rhel-6")
(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-6-Server/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))

(ipvsts:check
 (vminstall:download)
 (vminstall:disk-create)
 (vminstall:run-install))

(define (vm:start name order args)
  (let* ((mem (if (assoc 'mem args) (cdr (assoc 'mem args)) (cfg 'test:vm:mem)))
         (net (if (assoc 'net args) (cdr (assoc 'net args)) (cfg 'test:vm:net)))
         (vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (vm-net-model-args
          (cond ((equal? net 'both)
                 (list
                  "-net"
                  (simple-format #f "nic,vlan=0,model=virtio,macaddr=52:54:00:00:00:~A"
                                 (byte->hexstr (1+ (* order 2))))
                  "-net"
                  (simple-format #f "nic,vlan=1,model=virtio,macaddr=52:54:00:00:00:~A"
                                (byte->hexstr (* order 2)))))
                (#t
                 (list
                  "-net"
                  (simple-format #f "nic,vlan=0,model=virtio,macaddr=52:54:00:00:00:~A"
                                 (byte->hexstr (* order 2)))))))
         (vm-net-type-args
          (cond ((equal? net 'user)
                 (list "-net" "user,vlan=0"))
                ((equal? net 'lan1)
                 (list "-net" "socket,vlan=0,mcast=239.255.0.1:4096"))
                ((equal? net 'lan2)
                 (list "-net" "socket,vlan=0,mcast=239.255.0.1:4097"))
                ((equal? net 'both)
                 (list "-net" "socket,vlan=0,mcast=239.255.0.1:4096"
                       "-net" "socket,vlan=0,mcast=239.255.0.1:4097"))))
         (vm-args (append (list (cfg 'ipvsts:qemu)
                     "-hda" (string-append vm-dir "/" name ".img")
                     "-m" (number->string mem)
                     "-vnc" (string-append ":" (number->string (+ order (cfg 'test:vm:vnc-base))))
                     "-serial"
                     (string-append "tcp:127.0.0.1:"
                                    (number->string (+ order (cfg 'test:vm:rguile-port-base)))
                                    ",server,nowait"))
                     vm-net-type-args vm-net-model-args)))
    vm-args))

(display (vm:start "c1" 2 '()))
;; (apply system* (vm:start "c1" 2 '((net . both))))
