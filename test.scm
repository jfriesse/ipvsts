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
(use-modules (tests vm))

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

(display (vm:start "c1" 1 '((mem . 512))))
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
