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

(define-module (ipvsts ipvslocal))

(use-modules (ice-9 regex))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts vmsh))

(export ipvslocal:rules-sort ipvslocal:parse:net-ip_vs)

;; Sort parsed rules
(define (ipvslocal:rules-sort rules)
  (define (route-sort rules res)
    (cond ((null? rules) res)
          ((null? (caddr (cdddar rules)))
           (route-sort (cdr rules) (append res (list (car rules)))))
          (#t
           (route-sort (cdr rules)
                       (append
                        res
                        (list
                         (list
                          (car (car rules))
                          (cadr (car rules))
                          (caddr (car rules))
                          (cadddr (car rules))
                          (cadr (cdddr (car rules)))
                          (sort (caddr (cdddar rules))
                                (lambda (i1 i2)
                                  (or (string<? (car i1) (car i2))
                                      (string<? (cadr i1) (cadr i2))))))))))))

  (define (service-sort rules)
    (sort rules
          (lambda (i1 i2)
            (if (string<? (symbol->string (car i1))
                          (symbol->string (car i2)))
                (string<? (cadr i1) (cadr i2))
                (string<? (caddr i1) (caddr i2))))))

  (service-sort (route-sort rules '())))

;; Parse /proc/net/ip_vs (stored in cfg 'test:vm:sh:file:proc-ip_vs) to list of
;; (service_type addr port scheduler params (route1 ... routeN))
;; where route is list of (addr port type weight active_conn inactive_conn)
;; cl is rguile-client and save-active? is flag if active_conn and inactive_conn should be stored
;; or not
(define (ipvslocal:parse:net-ip_vs cl save-active?)
  (define (parse-proto proto-str)
    (cond ((equal? proto-str "TCP") 'TCP)
          ((equal? proto-str "UDP") 'UDP)
          ((equal? proto-str "FWM") 'FWM)
          (#t #f)))

  (define (return-service-matcher addr-str)
    (let ((tu6-m (string-match "^([TU][^ ]+) +\\[([0-9A-Fa-f:]+)]:([0-9A-Fa-f]+) +([^ ]+)" addr-str))
          (tu4-m (string-match "^([TU][^ ]+) +([0-9A-Fa-f]+):([0-9A-Fa-f]+) +([^ ]+)" addr-str))
          (fw-m (string-match "^([F][^ ]+) +([0-9A-Fa-f]+) +([^ ]+)" addr-str)))
      (cond (tu6-m tu6-m)
            (tu4-m tu4-m)
            (fw-m fw-m)
            (#t #f))))

  (define (return-route-matcher addr-str)
    (let ((tu6-m (string-match
                  "^-> +\\[([0-9A-Fa-f:]+)]:([0-9A-Fa-f]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)"
                  addr-str))
          (tu4-m (string-match
                  "^-> +([0-9A-Fa-f]+):([0-9A-Fa-f]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)"
                  addr-str)))
      (cond (tu6-m tu6-m)
            (tu4-m tu4-m)
            (#t #f))))

  (define (parse-line file current-service rules res)
    (cond ((null? file)
           (if (not current-service)
               res
               (append res (list (append current-service (list rules))))))
          ((or (string-match "^TCP +" (car file))
               (string-match "^UDP +" (car file))
               (string-match "^FWM +" (car file)))
           (let* ((service-match (return-service-matcher (car file)))
                  (proto (parse-proto (match:substring service-match 1)))
                  (addr (match:substring service-match 2))
                  (port (if (eq? proto 'FWM) "0" (match:substring service-match 3)))
                  (scheduler (if (eq? proto 'FWM)
                                 (match:substring service-match 3)
                                 (match:substring service-match 4)))
                  (rest
                   (if (equal? (match:suffix service-match) "")
                       ""
                       (substring (match:suffix service-match) 1))))
             (parse-line (cdr file)
                         (list proto addr port scheduler rest)
                         '()
                         (if (not current-service)
                             res
                             (append res (list (append current-service (list rules))))))))
          ((string-match "^ +-> +" (car file))
           (if current-service
               (let* ((route-match (return-route-matcher (string-trim-both (car file))))
                      (addr (match:substring route-match 1))
                      (port (match:substring route-match 2))
                      (type (match:substring route-match 3))
                      (weight (match:substring route-match 4))
                      (active (match:substring route-match 5))
                      (inactive (match:substring route-match 6)))
                 (parse-line (cdr file)
                             current-service
                             (append rules
                                     (if save-active?
                                         (list (list addr port type weight active inactive))
                                         (list (list addr port type weight))))
                             res))
               (parse-line (cdr file) current-service rules res)))
          (#t
           (parse-line (cdr file) current-service rules res))))

  (let ((res (vm:sh:get-file cl (cfg 'test:vm:sh:file:proc-ip_vs))))
    (parse-line (string-split res #\nl) #f '() '())))
