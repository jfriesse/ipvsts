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

(define-module (tests ipvslocal))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts tunit))
(use-modules (ipvsts vm))
(use-modules (ipvsts vmdisk))
(use-modules (ipvsts vminstall))
(use-modules (ipvsts vmsh))

(use-modules (rguile client))

(export test:ipvslocal:module-loaded test:ipvslocal:dont-load-module-on-status)

;; Test if ip_vs module is loaded
(define (test:ipvslocal:module-loaded cl)
  (vm:sh:is-module-loaded? cl (cfg 'test:vm:sh:module-name:ipvs)))

;; Test, that module is not loaded on start and status if no cfg file exists
(define (test:ipvslocal:dont-load-module-on-status cl)
  (ipvsts:check 'dont-load-module-on-status
                (not (test:ipvslocal:module-loaded cl))
                (not (vm:sh:service cl (cfg 'test:vm:sh:service:ipvsadm) "start"))
                (not (vm:sh:service cl (cfg 'test:vm:sh:service:ipvsadm) "status"))
                (not (test:ipvslocal:module-loaded cl))))
