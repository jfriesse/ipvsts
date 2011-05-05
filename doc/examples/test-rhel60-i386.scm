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

;; Example file for installation of RHEL 6.0 i386 with update to nightly
;; ipvsts:http-mirror must be set to work

(set! %load-path (append %load-path (list "../../" "/usr/local/share/ipvsts" "/usr/share/ipvsts")))
(use-modules (ipvsts cfg))
(use-modules (ipvsts tunit))

(use-modules (tests delimiter))
(use-modules (tests vmcreate))
(use-modules (tests vmprepare))
(use-modules (tests ipvslocal))

(define local-version-major "6")
(define local-version-minor "0")

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version (string-append local-version-major "." local-version-minor))
(set-cfg! 'test:name (string-append "rhel-" local-version-major "-" (cfg 'test:arch)))

(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror)
                                           "/released/RHEL-" local-version-major "-Server/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:update-url (string-append
                            (cfg 'ipvsts:http-mirror)
                            "/nightly/latest-RHEL"
                            local-version-major "."
                            (number->string (+ (string->number local-version-minor) 1))
                            "/" local-version-major "/Server"
                            "/" (cfg 'test:arch) "/os"))

(set-cfg! 'test:yum:int-repos '("Server" "LoadBalancer"))
(set-cfg! 'test:yum:update-int-repos (cfg 'test:yum:int-repos))

(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))

(exit
 (ipvsts:check 'test
               (test:delim:start)
               (test:vmcreate)
               (test:vm-prepare-base-image)
               (test:ipvslocal)
               (test:delim:end)))
