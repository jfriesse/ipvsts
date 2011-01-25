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

;;(ipvsts:check
;; (vminstall:download)
;; (vminstall:disk-create)
;; (vminstall:run-install))

(define (vm:start name order . args)
  (define (get-net-qemu-params order params)
    (define (iter params i res)
      (if (null? params) res
          (let ((nic-params
                 (list "-net" (simple-format #f "nic,vlan=~A,model=virtio,macaddr=~A"
                                             i
                                             (simple-format #f (cfg 'test:vm:macaddr)
                                                            (byte->hexstr order)
                                                            (byte->hexstr i))))))
            (cond  ((equal? (car params) 'user)
                    (iter (cdr params)
                          (+ i 1)
                          (append res nic-params
                                  (list "-net" (simple-format #f "user,vlan=~A" i)))))
                   (#t
                    (iter (cdr params)
                          (+ i 1)
                          (append res nic-params
                                  (list "-net"
                                        (simple-format
                                         #f "socket,vlan=~A,mcast=~A:~A"
                                         i (cfg 'test:vm:mcast-addr)
                                         (+ (cfg 'test:vm:mcast-port-base) (cdar params)))))))))))
    (iter params 0 '()))

  (let* ((mem (get-param-val 'mem 'test:vm:mem args))
         (net (get-param-val 'net 'test:vm:net args))
         (vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (net-args (get-net-qemu-params order net))
         (vm-args (append (list (cfg 'ipvsts:qemu)
                     "-hda" (string-append vm-dir "/" name ".img")
                     "-m" (number->string mem)
                     "-vnc" (string-append ":" (number->string (+ order (cfg 'test:vm:vnc-base))))
                     "-serial"
                     (string-append "tcp:127.0.0.1:"
                                    (number->string (+ order (cfg 'test:vm:rguile-port-base)))
                                    ",server,nowait"))
                     net-args)))
    vm-args))

(display (vm:start "c1" 2))
;;'((net . (user ))))
           (display (vm:start "c1" 2 '((net . (user (net . 1) (net . 2) (net . 3))))))
;; (apply system* (vm:start "c1" 2 '((net . both))))

;(set-cfg! 'test:arch "i386")
;(set-cfg! 'test:version "U5")
;(set-cfg! 'test:name "rhel-5")
;(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-5-Server/"
;                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
;(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))
;(set-cfg! 'test:distro 'el5)

;;(ipvsts:check
; (vminstall:download)
; (vminstall:disk-create)
; (vminstall:run-install))
