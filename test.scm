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

(use-modules (ipvsts vmdisk))
(use-modules (ipvsts vmsh))
(use-modules (ipvsts vminstall))
(use-modules (ipvsts vm))

(use-modules (tests vmcreate))
(use-modules (tests vmprepare))
(use-modules (tests ipvslocal))

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version "6.0")
(set-cfg! 'test:name "rhel-6")
(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-6-Server/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:update-url (string-append (cfg 'ipvsts:http-mirror)
                                          "/nightly/latest-RHEL6.1/6/Server"
                                          "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))

;;(ipvsts:check 'test
;;              (test:vmcreate)
;;              (test:vm-prepare-base-image))

(define (test:ipvslocal:bad-params cl net-id vm-id)
  (define (run-ipvsadm . params)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " "
                           (string-list->string params " ")))))
      (= res 0)))

  (define (add-service ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'add-service
                  (not (run-ipvsadm "-A"))
                  (not (run-ipvsadm "-A" ip4))
                  (not (run-ipvsadm "-A" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-A" "-t"))
                  (not (run-ipvsadm "-A" "-t" (string-append ip4 ":" port) "-O"))
                  (run-ipvsadm "-A" "-t" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-A" "-t" (string-append ip4 ":" port)))
                  (run-ipvsadm "-A" "-t" (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-A" "-t" (string-append "[" ip6 "]:" port)))
                  (not (run-ipvsadm "-A" "-u"))
                  (run-ipvsadm "-A" "-u" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-A" "-u" (string-append ip4 ":" port)))
                  (run-ipvsadm "-A" "-u" (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-A" "-u" (string-append "[" ip6 "]:" port)))
                  (not (run-ipvsadm "-A" "-f"))
                  (run-ipvsadm "-A" "-f" fw-mark)
                  (not (run-ipvsadm "-A" "-f" fw-mark))
                  (not (run-ipvsadm "-A" (string-append ip4 ":" port)))))

  (define (edit-service ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'edit-service
                  (not (run-ipvsadm "-E"))
                  (run-ipvsadm "-E" "-t" (string-append ip4 ":" port))
                  (run-ipvsadm "-E" "-t" (string-append ip4 ":" port) "-s" "rr")
                  (not (run-ipvsadm "-E" "-t" (string-append ip4 ":" port2) "-s" "rr"))
                  (run-ipvsadm "-E" "-t" (string-append "[" ip6 "]:" port) "-s" "rr")
                  (not (run-ipvsadm "-E" "-t" (string-append "[" ip6 "]:" port2) "-s" "rr"))
                  (run-ipvsadm "-E" "-u" (string-append ip4 ":" port))
                  (run-ipvsadm "-E" "-u" (string-append ip4 ":" port) "-s" "rr")
                  (not (run-ipvsadm "-E" "-u" (string-append ip4 ":" port2) "-s" "rr"))
                  (run-ipvsadm "-E" "-u" (string-append "[" ip6 "]:" port) "-s" "rr")
                  (not (run-ipvsadm "-E" "-u" (string-append "[" ip6 "]:" port2) "-s" "rr"))
                  (run-ipvsadm "-E" "-f" fw-mark)
                  (run-ipvsadm "-E" "-f" fw-mark "-s" "rr")))

  (define (del-service ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'del-service
                  (not (run-ipvsadm "-D"))
                  (not (run-ipvsadm "-D" "-t"))
                  (run-ipvsadm "-D" "-t" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-D" "-t" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-D" "-t" (string-append ip4 ":" port2)))
                  (run-ipvsadm "-D" "-t" (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-D" "-t" (string-append "[" ip6 "]:" port)))
                  (run-ipvsadm "-D" "-u" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-D" "-u" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-D" "-u" (string-append ip4 ":" port2)))
                  (run-ipvsadm "-D" "-u" (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-D" "-u" (string-append "[" ip6 "]:" port)))
                  (run-ipvsadm "-D" "-f" fw-mark)))

  (define (add-route ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'add-route
                  (not (run-ipvsadm "-a"))
                  (not (run-ipvsadm "-a" "-t"))
                  (not (run-ipvsadm "-a" "-t" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-a" "-t" (string-append ip4 ":" port) "-r"))
                  (run-ipvsadm "-a" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port))
                  (not (run-ipvsadm "-a" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port)))
                  (run-ipvsadm "-a" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-a" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port)))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-w"))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-w" "test"))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-x"))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-x" "test"))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-y"))
                  (not (run-ipvsadm "-a" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-y" "test"))))

  (define (del-route ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'del-route
                  (not (run-ipvsadm "-d"))
                  (not (run-ipvsadm "-d" "-t"))
                  (not (run-ipvsadm "-d" "-t" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-d" "-t" (string-append ip4 ":" port) "-r"))
                  (run-ipvsadm "-d" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port))
                  (not (run-ipvsadm "-d" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port)))
                  (run-ipvsadm "-d" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-d" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port)))
                  (not (run-ipvsadm "-d" "-u" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-w"))))

  (let ((ip4 (simple-format #f (cfg 'test:vm:ip:addr) net-id vm-id))
        (ip6 "fec1::1")
        (port "80")
        (port2 "81")
        (fw-mark "1"))
    (ipvsts:check 'bad-params
                  (add-service ip4 ip6 port port2 fw-mark)
                  (edit-service ip4 ip6 port port2 fw-mark)
                  (del-service ip4 ip6 port port2 fw-mark)
                  (add-service ip4 ip6 port port2 fw-mark)
                  (add-route ip4 ip6 port port2 fw-mark)
                  (del-route ip4 ip6 port port2 fw-mark)
                  (run-ipvsadm "-C"))))

(test:ipvslocal:bad-params (rguile-client "127.0.0.1" 2301) 1 1)

(define (test:ipvslocal)
  (let* ((vm-id 1)
         (net-id 1)
         (vm-disk-name "lvs1")
         (vm-net (list 'user (cons 'net net-id)))
         (cl (rguile-client "127.0.0.1" (+ (cfg 'test:vm:rguile-port-base) vm-id))))

    (define (vm-start)
      (call-with-cfg
       (list (cons 'test:vm:net vm-net))
       (lambda ()
         (vm:start vm-disk-name vm-id))))

  (ipvsts:check 'ipvslocal
                (vm:disk:create-snapshot vm-disk-name)
                (vm-start)
                (vm:configure-net cl vm-id vm-net)
                (test:ipvslocal:dont-load-module-on-status cl)
                (test:ipvslocal:auto-load-module cl)
                (test:ipvslocal:auto-unload-module cl)
                (test:ipvslocal:man-page-test cl)
                (test:ipvslocal:bad-params cl net-id vm-id))))

(test:ipvslocal)

;; (set-cfg! 'test:arch "i386")
;; (set-cfg! 'test:version "U5")
;; (set-cfg! 'test:name "rhel-5")
;; (set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-5-Server/"
;;                                            (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
;; (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))
;; (set-cfg! 'test:distro 'el5)

