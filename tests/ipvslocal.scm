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

(use-modules (ice-9 regex))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts tunit))
(use-modules (ipvsts vm))
(use-modules (ipvsts vmdisk))
(use-modules (ipvsts vminstall))
(use-modules (ipvsts vmsh))

(use-modules (rguile client))

(export test:ipvslocal:auto-load-module test:ipvslocal:auto-unload-module
        test:ipvslocal:module-loaded test:ipvslocal:dont-load-module-on-status
        test:ipvslocal:man-page-test)

;; Test that ip_vs module is automatically loaded on first run of ipvsadm
;; (not thru service init script)
(define (test:ipvslocal:auto-load-module cl)
  (let ((res
         (vm:sh:run-command
          cl
          (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " -Ln"))))
    (ipvsts:check 'auto-load-module
                  (= res 0)
                  (test:ipvslocal:module-loaded cl))))

;; Test that init script correctly unloads module on stop of service
(define (test:ipvslocal:auto-unload-module cl)
  (ipvsts:check 'auto-unload-module
                (vm:sh:service cl (cfg 'test:vm:sh:service:ipvsadm) "stop")
                (not (test:ipvslocal:module-loaded cl))))

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

;; Test that man page of ipvsadm exists and also that all options returned by
;; --help are documented. There is one exception (--ipv6) which is not documented
;; at all (but short version -6 is).
(define (test:ipvslocal:man-page-test cl)
  (define (gen-man-page)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:man) " " (cfg 'test:vm:sh:manpage:ipvsadm)
                           " | " (cfg 'test:vm:sh:cmd:col) " -b | "
                           (cfg 'test:vm:sh:cmd:tee) " /tmp/manpage.txt"
                           "; [ $PIPESTATUS -eq 0 ] || false"))))
      (= res 0)))

  (define (is-opt-in-mp? opt)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:grep) " -q -- " opt " /tmp/manpage.txt"))))
      (= res 0)))

  (define (line-iter file)
    (cond ((null? file) #t)
          (#t
           (let* ((line (string-trim-both (car file)))
                  (match (string-match "^(--[^ ]+)[^-]*(-[^ ])* +" line)))
             (cond ((= (string-length line) 0)
                    (line-iter (cdr file)))
                   ((not match) #f)
                   ((not (= (match:count match) 3)) #f)
                   (#t
                    (let ((long-opt (match:substring match 1))
                          (short-opt (match:substring match 2)))
                      (cond ((equal? long-opt "--ipv6")
                             (line-iter (cdr file)))
                            ((and long-opt short-opt)
                             (if (and (is-opt-in-mp? long-opt) (is-opt-in-mp? short-opt))
                                 (line-iter (cdr file))
                                 #f))
                            (#t
                             (if (is-opt-in-mp? long-opt)
                                 (line-iter (cdr file))
                                 #f))))))))))

  (let ((res
         (vm:sh:run-command-out
          cl
          (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " --help | "
                         (cfg 'test:vm:sh:cmd:grep) " '^ *--'")))
        (res-mp (gen-man-page)))
    (cond ((and (= (car res) 0) res-mp)
           (line-iter (string-split (cdr res) #\nl)))
          (#t #f))))
