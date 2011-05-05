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

;; Example file for installation of Scientific Linux 6x i386

(set! %load-path (append %load-path (list "../../" "/usr/local/share/ipvsts" "/usr/share/ipvsts")))
(use-modules (ipvsts cfg))
(use-modules (ipvsts tunit))

(use-modules (tests delimiter))
(use-modules (tests vmcreate))
(use-modules (tests vmprepare))
(use-modules (tests ipvslocal))

(set-cfg! 'test:arch "i386")
(set-cfg! 'test:version "6x")
(set-cfg! 'test:name (string-append "sl-6x" "-" (cfg 'test:arch)))
(set-cfg! 'ipvsts:http-mirror "http://ftp.scientificlinux.org")
(set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/linux/scientific/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
(set-cfg! 'test:update-url (string-append (cfg 'ipvsts:http-mirror) "/linux/scientific/"
                                           (cfg 'test:version) "/" (cfg 'test:arch) "/updates"))
(set-cfg! 'test:yum:update-int-repos '("fastbugs" "security"))
(set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))

(exit
 (ipvsts:check 'test
               (test:delim:start)
               (test:vmcreate)
               (test:vm-prepare-base-image)
               (test:ipvslocal)
               (test:delim:end)))
