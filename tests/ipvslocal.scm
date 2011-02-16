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
(use-modules (ipvsts utils))
(use-modules (ipvsts vm))
(use-modules (ipvsts vmdisk))
(use-modules (ipvsts vminstall))
(use-modules (ipvsts vmsh))

(use-modules (rguile client))

(export test:ipvslocal:auto-load-module test:ipvslocal:auto-unload-module
        test:ipvslocal:bad-params test:ipvslocal:module-loaded
        test:ipvslocal:dont-load-module-on-status test:ipvslocal:man-page-test)

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

  (define (edit-route ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'edit-route
                  (not (run-ipvsadm "-e"))
                  (not (run-ipvsadm "-e" "-t"))
                  (not (run-ipvsadm "-e" "-t" (string-append ip4 ":" port)))
                  (not (run-ipvsadm "-e" "-t" (string-append ip4 ":" port) "-r"))
                  (run-ipvsadm "-e" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port))
                  (not (run-ipvsadm "-e" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-w"))
                  (not (run-ipvsadm "-e" "-t" (string-append ip4 ":" port) "-r"
                       (string-append ip4 ":" port) "-w" "test"))
                  (run-ipvsadm "-e" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port))
                  (not (run-ipvsadm "-e" "-t" (string-append "[" ip6 "]:" port) "-r"
                       (string-append "[" ip6 "]:" port) "-x" "test"))))

  (define (zero-command ip4 ip6 port port2 fw-mark)
    (ipvsts:check 'zero-command
                  (run-ipvsadm "-Z")
                  (not (run-ipvsadm "-Z" "-t"))
                  (run-ipvsadm "-Z" "-t" (string-append ip4 ":" port))
                  (not (run-ipvsadm "-Z" "-t" (string-append ip4 ":" port2) "-r"))))

  (define (set-command)
    (ipvsts:check 'set-command
                  (not (run-ipvsadm "--set"))
                  (not (run-ipvsadm "--set" "0"))
                  (not (run-ipvsadm "--set" "0" "0"))
                  (not (run-ipvsadm "--set" "0" "0" "test"))
                  (not (run-ipvsadm "--set" "0" "0" "0" "0"))
                  (run-ipvsadm "--set" "0" "0" "0")))

  (define (daemon)
    (ipvsts:check 'daemon
                  (not (run-ipvsadm "--start-daemon"))
                  (not (run-ipvsadm "--start-daemon" "test"))
                  (run-ipvsadm "--start-daemon" "master")
                  (not (run-ipvsadm "--start-daemon" "master"))
                  (run-ipvsadm "--start-daemon" "backup")
                  (not (run-ipvsadm "--start-daemon" "backup"))
                  (run-ipvsadm "--stop-daemon" "master")
                  (not (run-ipvsadm "--stop-daemon" "master"))
                  (run-ipvsadm "--stop-daemon" "backup")
                  (not (run-ipvsadm "--stop-daemon" "backup"))))

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
                  (edit-route ip4 ip6 port port2 fw-mark)
                  (zero-command ip4 ip6 port port2 fw-mark)
                  (del-route ip4 ip6 port port2 fw-mark)
                  (set-command)
                  (daemon)
                  (run-ipvsadm "-C"))))

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
