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

(define-module (tests ipvslocalrules))

(use-modules (ice-9 regex))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts ipvslocal))
(use-modules (ipvsts tunit))
(use-modules (ipvsts utils))
(use-modules (ipvsts vmsh))

(use-modules (rguile client))

(export test:ipvslocal:rules)

;; Test creation of rules on ipvsadm side. Parsed rules from /proc/net/ip_vs are compared
;; with internal simulation of rules creation.
(define (test:ipvslocal:rules cl net-id vm-id)
  (define rules '())

  (define (run-ipvsadm . params)
    (let ((res
           (vm:sh:run-command
            cl
            (string-append (cfg 'test:vm:sh:cmd:ipvsadm) " "
                           (string-list->string params " ")))))
      (= res 0)))

  ;; Find list with service
  (define (find-service ip6 type addr port)
    (define (iter l)
      (cond ((null? l) #f)
            ((and
              (equal? (caar l)
                      (cond ((equal? type 't) 'TCP)
                            ((equal? type 'u) 'UDP)
                            ((equal? type 'f) 'FWM)))
              (equal? (cadar l)
                      (if (equal? type 'f)
                          (fwmark->hexstr addr)
                          (if ip6 (ip6addr->hexstr addr) (ip4addr->hexstr addr))))
              (equal? (caddar l)
                      (if (equal? type 'f)
                          "0"
                          (ipport->hexstr port))))
             (car l))
            (#t
             (iter (cdr l)))))
    (iter rules))

  (define (find-service-route ip6 type addr port route-addr route-port)
    (define (iter l)
      (cond ((null? l) #f)
            ((and
              (equal? (caar l)
                      (if ip6 (ip6addr->hexstr route-addr) (ip4addr->hexstr route-addr)))
              (equal? (cadar l)
                      (ipport->hexstr route-port)))
             (car l))
            (#t
             (iter (cdr l)))))

    (let* ((service (find-service ip6 type addr port))
           (routes (car (cddddr (cdr service)))))
      (iter routes)))

  ;; Delete service from rules
  (define (delete-service-from-rules ip6 type addr port)
    (set! rules
          (filter
           (lambda (l)
             (not (and
                   (equal? (car l)
                           (cond ((equal? type 't) 'TCP)
                                 ((equal? type 'u) 'UDP)
                                 ((equal? type 'f) 'FWM)))
                   (equal? (cadr l)
                           (if (equal? type 'f)
                               (fwmark->hexstr addr)
                               (if ip6 (ip6addr->hexstr addr) (ip4addr->hexstr addr))))
                   (equal? (caddr l)
                           (if (equal? type 'f)
                               "0"
                               (ipport->hexstr port))))))
           rules)))

  ;; Delete route from service
  (define (delete-route-from-service service ip6 addr port)
    (set-cdr!
     (cdddr (cdr service))
     (list
      (filter
       (lambda (l)
         (not (and
               (equal? (car l)
                       (if ip6 (ip6addr->hexstr addr) (ip4addr->hexstr addr)))
               (equal? (cadr l)
                       (ipport->hexstr port)))))
       (cadr (cddddr service)))))
    service)

  (define (clear-rules)
    (run-ipvsadm "-C")
    (set! rules '()))

  (define (add-service ip6 type addr port scheduler timeout one-packet netmask)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-A"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))
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
               (if (equal? type 'f)
                   (fwmark->hexstr addr)
                   (if ip6 (ip6addr->hexstr addr) (ip4addr->hexstr addr)))
               (if (equal? type 'f)
                   "0"
                   (ipport->hexstr port))
               (if scheduler scheduler "wlc")
               (string-append
                (if one-packet "ops " "")
                (if timeout (simple-format #f "persistent ~A ~A"
                                           (* (string->number timeout) 1000)
                                           (if ip6
                                               (if netmask
                                                   (ip4addr->hexstr
                                                    (string-append netmask ".0.0.0"))
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
                           (ipvslocal:parse:net-ip_vs cl #f))
                          (ipvslocal:rules-sort rules))))

  (define (delete-service ip6 type addr port)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-D"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))))

    (define (mod-rules)
      (delete-service-from-rules ip6 type addr port)
      #t)

    (ipvsts:check 'delete-service
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort rules)
                          (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))

  (define (edit-service ip6 type addr port scheduler timeout one-packet netmask)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-E"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))
                   (if scheduler (string-append "-s " scheduler) "")
                   (if timeout (string-append "-p " timeout) "")
                   (if one-packet "-O" "")
                   (if netmask (simple-format #f "-M ~A" netmask) "")))

    (define (mod-rules)
      (let ((service (find-service ip6 type addr port)))
        (set-car! (cdddr service)
                  (if scheduler scheduler "wlc"))
        (set-car! (cddddr service)
                  (string-append
                   (if one-packet "ops " "")
                   (if timeout (simple-format #f "persistent ~A ~A"
                                              (* (string->number timeout) 1000)
                                              (if ip6
                                                  (if netmask
                                                      (ip4addr->hexstr
                                                       (string-append netmask ".0.0.0"))
                                                      (ip4addr->hexstr "128.0.0.0"))
                                                  (if netmask
                                                      (ip4addr->hexstr netmask)
                                                      (ip4addr->hexstr "255.255.255.255"))))
                       "")))
        #t))

    (ipvsts:check 'edit-service
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort rules)
                          (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))

  (define (add-route ip6 type addr port route-addr route-port route-type weight)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-a"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))
                   "-r"
                   (if ip6 (string-append "[" route-addr "]:" route-port)
                       (string-append route-addr ":" route-port))
                   (cond ((equal? route-type 'g) "-g")
                         ((equal? route-type 'i) "-i")
                         ((equal? route-type 'm) "-m")
                         (#t ""))
                   (if weight (string-append "-w " weight) "")))

    (define (mod-rules)
      (let* ((service (find-service ip6 type addr port))
             (routes (car (cddddr (cdr service)))))
        (set! routes
              (append
               routes
               (list
                (list
                 (if ip6 (ip6addr->hexstr route-addr) (ip4addr->hexstr route-addr))
                 (ipport->hexstr route-port)
                 (cond ((equal? route-type 'g) "Route")
                       ((equal? route-type 'i) "Tunnel")
                       ((equal? route-type 'm) "Masq")
                       ((equal? route-type 'l) "Local")
                       (#t "Route"))
                 (if weight weight "1")))))
        (set-cdr! (cdddr (cdr service)) (list routes))
        #t))

    (ipvsts:check 'add-route
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort rules)
                          (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))

  (define (del-route ip6 type addr port route-addr route-port)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-d"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))
                   "-r"
                   (if ip6 (string-append "[" route-addr "]:" route-port)
                       (string-append route-addr ":" route-port))))

    (define (mod-rules)
      (let ((service (find-service ip6 type addr port)))
        (delete-route-from-service service ip6 route-addr route-port)))

    (ipvsts:check 'del-route
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort rules)
                          (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))

  (define (edit-route ip6 type addr port route-addr route-port route-type weight)
    (define (run-ipvsadm-params)
      (run-ipvsadm "-e"
                   (cond ((equal? type 't) "-t")
                         ((equal? type 'u) "-u")
                         ((equal? type 'f) "-f"))
                   (if (equal? type 'f)
                       (string-append addr
                                      (if ip6 " -6 " ""))
                       (if ip6 (string-append "[" addr "]:" port)
                           (string-append addr ":" port)))
                   "-r"
                   (if ip6 (string-append "[" route-addr "]:" route-port)
                       (string-append route-addr ":" route-port))
                   (cond ((equal? route-type 'g) "-g")
                         ((equal? route-type 'i) "-i")
                         ((equal? route-type 'm) "-m")
                         (#t ""))
                   (if weight (string-append "-w " weight) "")))

    (define (mod-rules)
      (let ((routes (find-service-route ip6 type addr port route-addr route-port)))
        (set-car! (cddr routes)
                  (cond ((equal? route-type 'g) "Route")
                        ((equal? route-type 'i) "Tunnel")
                        ((equal? route-type 'm) "Masq")
                        ((equal? route-type 'l) "Local")
                        (#t "Route")))
        (set-car! (cdddr routes)
                  (if weight weight "1"))
        #t))

    (ipvsts:check 'edit-route
                  (run-ipvsadm-params)
                  (mod-rules)
                  (equal? (ipvslocal:rules-sort rules)
                          (ipvslocal:rules-sort
                           (ipvslocal:parse:net-ip_vs cl #f)))))

  (define (test-add-service ip4 ip42 ip6 ip62 port port2 fw-mark fw-mark2 fw-mark3 fw-mark4)
    (ipvsts:check 'test-add-service
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #f 'f fw-mark #f #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)
                  (add-service #f 't ip42 port2 "rr" #f #f #f)
                  (add-service #f 'u ip42 port2 "rr" #f #f #f)
                  (add-service #f 'f fw-mark3 #f "rr" #f #f #f)
                  (add-service #t 't ip62 port2 "rr" #f #f #f)
                  (add-service #t 'u ip62 port2 "rr" #f #f #f)
                  (add-service #t 'f fw-mark4 #f "rr" #f #f #f)
                  (clear-rules)
                  (add-service #f 't ip4 port #f "12" #f #f)
                  (add-service #f 'u ip4 port #f "12" #f #f)
                  (add-service #f 'f fw-mark #f #f "12" #f #f)
                  (add-service #f 'u ip42 port2 #f #f #t #f)
                  (add-service #t 't ip6 port #f "12" #f #f)
                  (add-service #t 'u ip6 port #f "12" #f #f)
                  (add-service #t 'f fw-mark2 #f #f "12" #f #f)
                  (add-service #t 'u ip62 port2 #f #f #t #f)
                  (clear-rules)
                  (add-service #f 't ip4 port "rr" "12" #f "255.255.255.0")
                  (add-service #f 'f fw-mark #f "rr" "12" #f "255.255.255.0")
                  (add-service #f 'u ip4 port #f "12" #t "255.255.255.0")
                  (add-service #t 't ip6 port "rr" "12" #f "112")
                  (add-service #t 'f fw-mark2 #f "rr" "12" #f "112")
                  (add-service #t 'u ip6 port #f "12" #t "112")))

  (define (test-del-service ip4 ip42 ip6 ip62 port port2 fw-mark fw-mark2 fw-mark3 fw-mark4)
    (ipvsts:check 'test-del-service
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #f 'f fw-mark #f #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)
                  (add-service #f 't ip42 port2 "rr" #f #f #f)
                  (add-service #f 'u ip42 port2 "rr" #f #f #f)
                  (add-service #f 'f fw-mark3 #f "rr" #f #f #f)
                  (add-service #t 't ip62 port2 "rr" #f #f #f)
                  (add-service #t 'u ip62 port2 "rr" #f #f #f)
                  (add-service #t 'f fw-mark4 #f "rr" #f #f #f)

                  (delete-service #f 't ip4 port)
                  (delete-service #f 'f fw-mark #f)
                  (delete-service #f 'u ip4 port)
                  (delete-service #f 't ip42 port2)
                  (delete-service #t 't ip6 port)
                  (delete-service #f 'f fw-mark3 #f)
                  (delete-service #t 't ip62 port2)
                  (delete-service #t 'u ip6 port)
                  (delete-service #t 'f fw-mark2 #f)
                  (delete-service #f 'u ip42 port2)
                  (delete-service #t 'f fw-mark4 #f)
                  (delete-service #t 'u ip62 port2)

                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)
                  (add-service #f 't ip42 port2 "rr" #f #f #f)
                  (delete-service #f 'u ip4 port)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (delete-service #f 't ip4 port)
                  (delete-service #t 'f fw-mark2 #f)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (delete-service #f 't ip4 port)
                  (delete-service #f 't ip42 port2)
                  (delete-service #f 'u ip4 port)))

  (define (test-edit-service ip4 ip42 ip6 ip62 port port2 fw-mark fw-mark2 fw-mark3 fw-mark4)
    (ipvsts:check 'test-edit-service
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #f 'f fw-mark #f #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)

                  (edit-service #f 't ip4 port "rr" #f #f #f)
                  (edit-service #f 'u ip4 port "rr" #f #f #f)
                  (edit-service #f 'f fw-mark #f "rr" #f #f #f)
                  (edit-service #t 't ip6 port "rr" #f #f #f)
                  (edit-service #t 'u ip6 port "rr" #f #f #f)
                  (edit-service #t 'f fw-mark2 #f "rr" #f #f #f)

                  (edit-service #f 't ip4 port #f "12" #f #f)
                  (edit-service #f 'u ip4 port #f "12" #f #f)
                  (edit-service #f 'f fw-mark #f #f "12" #f #f)
                  (edit-service #t 't ip6 port #f "14" #f #f)
                  (edit-service #t 'u ip6 port "wrr" "15" #f #f)
                  (edit-service #t 'f fw-mark2 #f "wrr" "16" #f #f)

                  (edit-service #f 't ip4 port #f "15" #f "255.255.255.0")
                  (edit-service #f 'u ip4 port #f "16" #t "255.255.255.0")
                  (edit-service #f 'f fw-mark #f #f "17" #f "255.255.0.0")
                  (edit-service #t 't ip6 port #f "14" #f "128")
                  (edit-service #t 'u ip6 port "wrr" "15" #t "127")
                  (edit-service #t 'f fw-mark2 #f "wrr" "16" #f "126")

                  (delete-service #f 't ip4 port)
                  (delete-service #f 'u ip4 port)
                  (delete-service #f 'f fw-mark #f)
                  (delete-service #t 't ip6 port)
                  (delete-service #t 'u ip6 port)
                  (delete-service #t 'f fw-mark2 #f)))

  (define (test-add-del-route ip4 ip42 ip43 ip6 ip62 ip63 port port2 fw-mark fw-mark2 fw-mark3
                              fw-mark4)
    (ipvsts:check 'test-add-del-route
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #f 'f fw-mark #f #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)

                  (add-route #f 't ip4 port ip42 port #f "2")
                  (add-route #f 't ip4 port ip4 port 'l "3")
                  (add-route #f 't ip4 port ip43 port #f "4")
                  (add-route #f 'u ip4 port ip42 port 'g "5")
                  (add-route #f 'u ip4 port ip4 port 'l "6")
                  (add-route #f 'u ip4 port ip43 port 'g "7")
                  (add-route #f 'f fw-mark #f ip42 port2 'i "8")
                  (add-route #f 'f fw-mark #f ip4 port2 'l "9")
                  (add-route #f 'f fw-mark #f ip43 port 'i "10")

                  (add-route #t 't ip6 port ip62 port #f #f)
                  (add-route #t 't ip6 port ip6 port 'l #f)
                  (add-route #t 't ip6 port ip63 port #f #f)
                  (add-route #t 'u ip6 port ip62 port 'g #f)
                  (add-route #t 'u ip6 port ip6 port 'l #f)
                  (add-route #t 'u ip6 port ip63 port 'g #f)
                  (add-route #t 'f fw-mark2 #f ip62 port2 'i #f)
                  (add-route #t 'f fw-mark2 #f ip6 port2 'l #f)
                  (add-route #t 'f fw-mark2 #f ip63 port 'i #f)

                  (del-route #f 't ip4 port ip42 port)
                  (del-route #f 't ip4 port ip4 port)
                  (add-route #f 't ip4 port ip42 port #f #f)
                  (del-route #f 't ip4 port ip43 port)
                  (del-route #f 't ip4 port ip42 port)

                  (del-route #f 'u ip4 port ip42 port)
                  (del-route #f 'u ip4 port ip4 port)
                  (add-route #f 'u ip4 port ip42 port #f #f)
                  (del-route #f 'u ip4 port ip43 port)
                  (del-route #f 'u ip4 port ip42 port)

                  (del-route #f 'f fw-mark #f ip42 port2)
                  (del-route #f 'f fw-mark #f ip4 port2)
                  (add-route #f 'f fw-mark #f ip42 port2 #f #f)
                  (del-route #f 'f fw-mark #f ip43 port)
                  (del-route #f 'f fw-mark #f ip42 port2)

                  (del-route #t 't ip6 port ip62 port)
                  (del-route #t 't ip6 port ip6 port)
                  (add-route #t 't ip6 port ip62 port #f #f)
                  (del-route #t 't ip6 port ip63 port)
                  (del-route #t 't ip6 port ip62 port)

                  (del-route #t 'u ip6 port ip62 port)
                  (del-route #t 'u ip6 port ip6 port)
                  (add-route #t 'u ip6 port ip62 port #f #f)
                  (del-route #t 'u ip6 port ip63 port)
                  (del-route #t 'u ip6 port ip62 port)

                  (del-route #t 'f fw-mark2 #f ip62 port2)
                  (del-route #t 'f fw-mark2 #f ip6 port2)
                  (add-route #t 'f fw-mark2 #f ip62 port2 #f #f)
                  (del-route #t 'f fw-mark2 #f ip63 port)
                  (del-route #t 'f fw-mark2 #f ip62 port2)

                  (add-route #f 't ip4 port ip42 port 'm #f)
                  (add-route #f 't ip4 port ip4 port 'l #f)
                  (add-route #f 't ip4 port ip43 port 'm #f)
                  (add-route #f 'u ip4 port ip42 port 'm #f)
                  (add-route #f 'u ip4 port ip4 port 'l #f)
                  (add-route #f 'u ip4 port ip43 port 'm #f)
                  (add-route #f 'f fw-mark #f ip42 port2 'm #f)
                  (add-route #f 'f fw-mark #f ip4 port2 'l #f)
                  (add-route #f 'f fw-mark #f ip43 port 'm #f)

                  (add-route #t 't ip6 port ip62 port 'm #f)
                  (add-route #t 't ip6 port ip6 port 'l #f)
                  (add-route #t 't ip6 port ip63 port 'm #f)
                  (add-route #t 'u ip6 port ip62 port 'm #f)
                  (add-route #t 'u ip6 port ip6 port 'l #f)
                  (add-route #t 'u ip6 port ip63 port 'm #f)
                  (add-route #t 'f fw-mark2 #f ip62 port2 'm #f)
                  (add-route #t 'f fw-mark2 #f ip6 port2 'l #f)
                  (add-route #t 'f fw-mark2 #f ip63 port 'm #f)))

  (define (test-edit-route ip4 ip42 ip43 ip6 ip62 ip63 port port2 fw-mark fw-mark2 fw-mark3
                           fw-mark4)
    (ipvsts:check 'test-edit-route
                  (clear-rules)
                  (add-service #f 't ip4 port #f #f #f #f)
                  (add-service #f 'u ip4 port #f #f #f #f)
                  (add-service #f 'f fw-mark #f #f #f #f #f)
                  (add-service #t 't ip6 port #f #f #f #f)
                  (add-service #t 'u ip6 port #f #f #f #f)
                  (add-service #t 'f fw-mark2 #f #f #f #f #f)

                  (add-route #f 't ip4 port ip42 port #f #f)
                  (add-route #f 't ip4 port ip4 port 'l #f)
                  (add-route #f 't ip4 port ip43 port #f #f)
                  (add-route #f 'u ip4 port ip42 port 'g #f)
                  (add-route #f 'u ip4 port ip4 port 'l #f)
                  (add-route #f 'u ip4 port ip43 port 'g #f)
                  (add-route #f 'f fw-mark #f ip42 port2 'i #f)
                  (add-route #f 'f fw-mark #f ip4 port2 'l #f)
                  (add-route #f 'f fw-mark #f ip43 port 'i #f)

                  (add-route #t 't ip6 port ip62 port #f #f)
                  (add-route #t 't ip6 port ip6 port 'l #f)
                  (add-route #t 't ip6 port ip63 port #f #f)
                  (add-route #t 'u ip6 port ip62 port 'g #f)
                  (add-route #t 'u ip6 port ip6 port 'l #f)
                  (add-route #t 'u ip6 port ip63 port 'g #f)
                  (add-route #t 'f fw-mark2 #f ip62 port2 'i #f)
                  (add-route #t 'f fw-mark2 #f ip6 port2 'l #f)
                  (add-route #t 'f fw-mark2 #f ip63 port 'i #f)

                  (edit-route #f 't ip4 port ip42 port 'm #f)
                  (edit-route #f 't ip4 port ip43 port #f "1")
                  (edit-route #f 't ip4 port ip4 port 'l "2")

                  (edit-route #f 'u ip4 port ip42 port 'm "55")
                  (edit-route #f 'u ip4 port ip43 port #f #f)
                  (edit-route #f 'u ip4 port ip4 port 'l #f)

                  (edit-route #f 'f fw-mark #f ip42 port2 'm "2")
                  (edit-route #f 'f fw-mark #f ip4 port2 'l "50")
                  (edit-route #f 'f fw-mark #f ip43 port 'g #f)

                  (edit-route #t 't ip6 port ip62 port 'm #f)
                  (edit-route #t 't ip6 port ip63 port #f "1")
                  (edit-route #t 't ip6 port ip6 port 'l "2")

                  (edit-route #t 'u ip6 port ip62 port 'm "55")
                  (edit-route #t 'u ip6 port ip63 port #f #f)
                  (edit-route #t 'u ip6 port ip6 port 'l #f)

                  (edit-route #t 'f fw-mark2 #f ip62 port2 'm "2")
                  (edit-route #t 'f fw-mark2 #f ip6 port2 'l "50")
                  (edit-route #t 'f fw-mark2 #f ip63 port 'g #f)))

  (let ((ip4 (simple-format #f (cfg 'test:vm:ip:addr) net-id vm-id))
        (ip42 (simple-format #f (cfg 'test:vm:ip:addr) (+ net-id 1) (+ vm-id 1)))
        (ip43 (simple-format #f (cfg 'test:vm:ip:addr) (+ net-id 1) (+ vm-id 2)))
        (ip6 (simple-format #f (cfg 'test:vm:ip6:addr) net-id vm-id))
        (ip62 (simple-format #f (cfg 'test:vm:ip6:addr) (+ net-id 1) (+ vm-id 1)))
        (ip63 (simple-format #f (cfg 'test:vm:ip6:addr) (+ net-id 1) (+ vm-id 2)))
        (port "80")
        (port2 "81")
        (fw-mark "1")
        (fw-mark2 "2")
        (fw-mark3 "900")
        (fw-mark4 "901"))
    (ipvsts:check 'rules
                  (test-add-service ip4 ip42 ip6 ip62 port port2
                                    fw-mark fw-mark2 fw-mark3 fw-mark4)
                  (test-del-service ip4 ip42 ip6 ip62 port port2
                                    fw-mark fw-mark2 fw-mark3 fw-mark4)
                  (test-edit-service ip4 ip42 ip6 ip62 port port2
                                     fw-mark fw-mark2 fw-mark3 fw-mark4)
                  (test-add-del-route ip4 ip42 ip43 ip6 ip62 ip63 port port2
                                      fw-mark fw-mark2 fw-mark3 fw-mark4)
                  (test-edit-route ip4 ip42 ip43 ip6 ip62 ip63 port port2
                                   fw-mark fw-mark2 fw-mark3 fw-mark4)
                  (clear-rules))))
