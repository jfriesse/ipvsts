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
(define-module (tests vm))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts utils))
(use-modules (ipvsts netfuncs))

(export vm:disk:create-snapshot vm:disk:compress)

;; Create snapshot from image (name can be in assoc list in 'name key, or test:disk:name) and
;; result image is in new-img
(define (vm:disk:create-snapshot new-img . args)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (get-param-val 'name 'test:disk:name args))
         (system-args (string-append
                       (cfg 'ipvsts:qemu-img) " create -f " (cfg 'vminstall:disk:format) " "
                       "-o" "backing_file=" vm-dir "/" disk-name ".img"
                       " " vm-dir "/" new-img ".img"
                       ">/dev/null"))
         (stat (system system-args)))
    (ipvsts:log "creating snapshot ~A return val ~A" system-args (status:exit-val stat))
    (if (= (status:exit-val stat) 0) #t #f)))

;; Compress image (name can be in assoc list in 'name key, or test:disk:name)
(define (vm:disk:compress . args)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (get-param-val 'name 'test:disk:name args))
         (tmp-disk-name (string-append vm-dir "/" "tmpimg-XXXXXX"))
         (new-tmp-disk-name
          (let ()
            (close (mkstemp! tmp-disk-name))
            tmp-disk-name))
         (system-args (string-append
                       (cfg 'ipvsts:qemu-img) " convert -O " (cfg 'vminstall:disk:format) " -c "
                       " " vm-dir "/" disk-name ".img"
                       " " new-tmp-disk-name
                       ">/dev/null"))
         (stat (system system-args)))
    (ipvsts:log "compressing image ~A return val ~A" system-args (status:exit-val stat))
    (if (= (status:exit-val stat) 0)
        (let ()
          (rename-file new-tmp-disk-name (string-append vm-dir "/" disk-name ".img"))
          #t)
        (let ()
          (delete-file new-tmp-disk-name)
          #f))))
