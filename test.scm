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

(define (test:ipvslocal:add-service cl net-id vm-id)
  (define (run-ipvsadm . params)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " "
                           (string-list->string params " ")))))
      (= res 0)))

  (let ((ip4 (simple-format #f (cfg 'test:vm:ip:addr) net-id vm-id))
        (port "80"))
    (ipvsts:check 'add-service
                  (not (run-ipvsadm "-A"))
                  (not (run-ipvsadm "-A" ip4))
                  (not (run-ipvsadm "-A" "-t"))
                  (run-ipvsadm "-A" "-t" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-A" "-u"))
                  (run-ipvsadm "-A" "-u" (string-append ip4 ":" port)))))

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
                (test:ipvslocal:man-page-test cl))))

(test:ipvslocal)

;; (set-cfg! 'test:arch "i386")
;; (set-cfg! 'test:version "U5")
;; (set-cfg! 'test:name "rhel-5")
;; (set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-5-Server/"
;;                                            (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
;; (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))
;; (set-cfg! 'test:distro 'el5)

