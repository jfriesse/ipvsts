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

(define-module (tests vmprepare))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts tunit))
(use-modules (ipvsts vm))
(use-modules (ipvsts vmdisk))
(use-modules (ipvsts vminstall))
(use-modules (ipvsts vmsh))

(use-modules (rguile client))

(export test:vm-prepare-base-image)

;; Prepare base image. Install updates + rpm packages and optionally compress base image
(define (test:vm-prepare-base-image)
  (let* ((vm-id 1)
         (vm-net '(user))
         (cl (rguile-client "127.0.0.1" (+ (cfg 'test:vm:rguile-port-base) vm-id))))
    (define (vm-start)
      (call-with-cfg
       (list (cons 'test:vm:mem (cfg 'test:vm:update-mem))
             (cons 'test:vm:net vm-net))
       (lambda ()
         (vm:start (cfg 'test:disk:name) vm-id))))

    (define (vm-add-int-yum-repos repo-list)
      (if repo-list
          (for-each (lambda (item) (vm:sh:add-int-yum-repo cl item)) repo-list)
          #t))

    (define (vm-add-int-update-yum-repos repo-list)
      (if repo-list
          (for-each (lambda (item) (vm:sh:add-int-update-yum-repo cl item)) repo-list)
          #t))

    (define (vm-yum-install packages)
      (if packages
          (vm:sh:yum-install cl packages)
          #t))

    (define (vm-rpm-install packages)
      (if packages
          (vm:sh:rpm-install cl packages)
          #t))

    (define (vm-chkconfig list oper)
      (if list
          (for-each (lambda (item) (vm:sh:chkconfig cl item oper)) list)
          #t))

    (ipvsts:check 'vm-prepare-base-image
                  (vm-start)
                  (vm:configure-net cl vm-id vm-net)
                  (vm:sh:delete-yum-repo cl "*")
                  (vm-add-int-yum-repos (cfg 'test:yum:int-repos))
                  (vm-add-int-update-yum-repos (cfg 'test:yum:update-int-repos))
                  (vm:sh:yum-update cl)
                  (vm-yum-install (cfg 'test:yum:packages-to-install))
                  (vm-rpm-install (cfg 'test:rpm:packages-to-install))
                  (vm-chkconfig (cfg 'test:chkconfig:off) "off")
                  (vm-chkconfig (cfg 'test:chkconfig:on) "on")
                  (vm:sh:shutdown "127.0.0.1" (+ (cfg 'test:vm:rguile-port-base) 1) #t #t)
                  (if (cfg 'test:vm:compress-base-image)
                      (vm:disk:compress)
                      #t))))
