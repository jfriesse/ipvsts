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
(use-modules (ice-9 regex))
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

(use-modules (ipvsts ipvslocal))

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


(define (test:ipvslocal:rules cl net-id vm-id)
  (define rules '())

  (define (run-ipvsadm . params)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " "
                           (string-list->string params " ")))))
      (= res 0)))

  (define (clear-rules)
    (run-ipvsadm "-C")
    (set! rules '()))

  (define (add-service ip6 type addr port scheduler timeout one-packet netmask)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-A"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if ip6 (string-append "[" addr "]:" port)
                       (string-append addr ":" port))
                   (if scheduler (string-append "-s " scheduler) "")
                   (if timeout (string-append "-p " timeout) "")
                   (if one-packet "-O" "")
                  (if netmask (simple-format #f "-M ~A" netmask) "")))

    (define (mod-rules)
      (set! rules
            (append
             rules
             (list
              (list
               (cond ((equal? type 't) 'TCP)
                     ((equal? type 'u) 'UDP)
                     ((equal? type 'f) 'FWM))
               (if ip6 (ip6addr->hexstr addr) (ip4addr->hexstr addr))
               (ipport->hexstr port)
               (if scheduler scheduler "wlc")
               (string-append
                (if one-packet "ops " "")
                (if timeout (simple-format #f "persistent ~A ~A"
                                           (* (string->number timeout) 1000)
                                           (if ip6
                                               (if netmask
                                                   (ip4addr->hexstr (string-append netmask ".0.0.0"))
                                                   (ip4addr->hexstr "128.0.0.0"))
                                               (if netmask
                                                   (ip4addr->hexstr netmask)
                                                   (ip4addr->hexstr "255.255.255.255"))))
                     ""))
                '()))))
      #t)

    (ipvsts:check 'add-service
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))
;;    (write 'loaded)
;;    (write (ipvslocal:rules-sort
;;              (ipvslocal:parse:net-ip_vs cl #f)))
;;    (newline)
;;    (write 'rules)
;;    (write (ipvslocal:rules-sort  rules))
;;    (newline)
;;    (equal? (ipvslocal:rules-sort
;;              (ipvslocal:parse:net-ip_vs cl #f))
;;            (ipvslocal:rules-sort rules)))

  (let ((ip4 (simple-format #f (cfg 'test:vm:ip:addr) net-id vm-id))
        (ip42 (simple-format #f (cfg 'test:vm:ip:addr) (+ net-id 1) (+ vm-id 1)))
        (ip6 (simple-format #f (cfg 'test:vm:ip6:addr) net-id vm-id))
        (ip62 (simple-format #f (cfg 'test:vm:ip6:addr) (+ net-id 1) (+ vm-id 1)))
        (port "80")
        (port2 "81")
        (fw-mark "1")
        (fw-mark2 "2"))
    (ipvsts:check 'rules
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #f 't ip42 port2 "rr" #f #f #f)
                  (add-service #f 'u ip42 port2 "rr" #f #f #f)
                  (add-service #t 't ip62 port2 "rr" #f #f #f)
                  (add-service #t 'u ip62 port2 "rr" #f #f #f)
                  (clear-rules)
                  (add-service #f 't ip4 port #f "12" #f #f)
                  (add-service #f 'u ip4 port #f "12" #f #f)
                  (add-service #f 'u ip42 port2 #f #f #t #f)
                  (add-service #t 't ip6 port #f "12" #f #f)
                  (add-service #t 'u ip6 port #f "12" #f #f)
                  (add-service #t 'u ip62 port2 #f #f #t #f)
                  (clear-rules)
                  (add-service #f 't ip4 port "rr" "12" #f "255.255.255.0")
                  (add-service #f 'u ip4 port #f "12" #t "255.255.255.0")
                  (add-service #t 't ip6 port "rr" "12" #f "112")
                  (add-service #t 'u ip6 port #f "12" #t "112"))))


(test:ipvslocal:rules (rguile-client "127.0.0.1" (+ (cfg 'test:vm:rguile-port-base) 1)) 1 1)
(define (test:ipvslocal)
  (let* ((vm-id 1)
         (net-id 1)
         (vm-disk-name "lvs1")
         (vm-net (list 'user (cons 'net net-id) (cons 'net (+ net-id 1))))
         (cl (rguile-client "127.0.0.1" (+ (cfg 'test:vm:rguile-port-base) vm-id))))

    (define (vm-start)
      (call-with-cfg
       (list (cons 'test:vm:net vm-net))
       (lambda ()
         (vm:start vm-disk-name vm-id))))

  (ipvsts:check 'ipvslocal
                (vm:disk:create-snapshot vm-disk-name)
                (vm-start)
                (vm:sh:set-selinux cl)
                (vm:configure-net cl vm-id vm-net)
                (test:ipvslocal:dont-load-module-on-status cl)
                (test:ipvslocal:auto-load-module cl)
                (test:ipvslocal:auto-unload-module cl)
                (test:ipvslocal:man-page-test cl)
                (test:ipvslocal:bad-params cl net-id vm-id)
                (test:ipvslocal:save-restore cl net-id vm-id))))

(test:ipvslocal)

;; (set-cfg! 'test:arch "i386")
;; (set-cfg! 'test:version "U5")
;; (set-cfg! 'test:name "rhel-5")
;; (set-cfg! 'test:install-url (string-append (cfg 'ipvsts:http-mirror) "/released/RHEL-5-Server/"
;;                                            (cfg 'test:version) "/" (cfg 'test:arch) "/os"))
;; (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-" (cfg 'test:name) ".log"))
;; (set-cfg! 'test:distro 'el5)

