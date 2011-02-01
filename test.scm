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

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version "6.0")
(set-cfg! 'test:name "rhel-6")
(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-6-Server/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:update-url (string-append (cfg 'ipvsts:http-mirror)
                                          "/nightly/latest-RHEL6.1/6/Server"
                                          "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))

;(ipvsts:check 'vminstall
; (vminstall:download)
; (vminstall:disk-create)
; (vminstall:run-install))

;;(vm:start "c1" 1 '((mem . 512) (net . (user))))
;;(let ((cl (rguile-client "127.0.0.1" 2301)))
;;  (vm:sh:create-file cl "ahoj" "/tmp/bla"))
;;  (vm:configure-net cl 1 '(user)))

(vm:disk:create-snapshot "c1")
(exit 0)

(ipvsts:check 'AHOJ
(vm:start "base" 1 '((mem . 512) (net . (user))))
(let ((cl (rguile-client "127.0.0.1" 2301)))
  (vm:configure-net cl 1 '(user)))
(vm:sh:delete-yum-repo (rguile-client "127.0.0.1" 2301) "*")
;;(vm:sh:chkconfig (rguile-client "127.0.0.1" 2301) "ipvsadm" "off")
(vm:sh:add-int-yum-repo (rguile-client "127.0.0.1" 2301) "Server")
(vm:sh:add-int-yum-repo (rguile-client "127.0.0.1" 2301) "LoadBalancer")
(vm:sh:add-int-update-yum-repo (rguile-client "127.0.0.1" 2301) "Server")
(vm:sh:add-int-update-yum-repo (rguile-client "127.0.0.1" 2301) "LoadBalancer")
(vm:sh:yum-update (rguile-client "127.0.0.1" 2301))
(vm:sh:yum-install (rguile-client "127.0.0.1" 2301) "ipvsadm")
(vm:sh:shutdown (rguile-client "127.0.0.1" 2301) #t))

;;(display (vm:sh:get-file "/etc/redhat-release"))
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
