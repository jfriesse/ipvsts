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

(define-module (ipvsts vmsh))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))

(export vm:sh:add-int-update-yum-repo vm:sh:add-int-yum-repo vm:sh:add-yum-repo
        vm:sh:chkconfig vm:sh:create-file vm:sh:delete-yum-repo vm:sh:get-file
        vm:sh:run-command vm:sh:service vm:sh:shutdown vm:sh:yum-install
        vm:sh:yum-update)

;; Add internal yum repo. Base url path is taken from 'test:update-url
(define (vm:sh:add-int-update-yum-repo cl repo)
  (vm:sh:add-yum-repo cl
                      (string-append repo "-updates")
                      (string-append (cfg 'test:update-url) "/" repo)))

;; Add internal yum repo. Base url path is taken from 'test:install-url
(define (vm:sh:add-int-yum-repo cl repo)
  (vm:sh:add-yum-repo cl repo (string-append (cfg 'test:install-url) "/" repo)))

;; Add yum repo with url to client
(define (vm:sh:add-yum-repo cl repo url)
  (ipvsts:log "Adding yum repo ~A(~A)" repo url)
  (vm:sh:create-file cl
                     (simple-format #f
                                    "[~A]\nname=~A\nbaseurl=~A\nenabled=1\ngpgcheck=0\n"
                                    repo
                                    repo
                                    url)
                     (string-append (cfg 'test:vm:sh:yum-repos-dir) "/" repo ".repo")))

;; Perform chkconfig action on service. Action may be on|off
(define (vm:sh:chkconfig cl service action)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:chkconfig) " " service " " action))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Chkconfig ~A ~A (~A) result ~A" service action cmd res)
    (= res 0)))

;; Create file output from input-str string on remote guile cl
(define (vm:sh:create-file cl input-str output)
  (cl
   `(let ((f (open-file ,output "w")))
      (display ,input-str f)
      (close f))))

;; Delete yum repo. Repo can be * which means, all repos are deleted
(define (vm:sh:delete-yum-repo cl repo)
  (=
   (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:rm) " -f "
                                        (cfg 'test:vm:sh:yum-repos-dir)
                                        "/" repo ".repo"))
   0))

;; Retreive file remote guile cl
;; Return #f if file is not found otherwise string with file content
(define (vm:sh:get-file cl file)
  (cl
   `(if (access? ,file R_OK)
        (let ((f (open-file ,file "r"))
              (s (open-output-string)))
          (do ((readed (read-char f) (read-char f)))
              ((eof-object? readed))
            (write-char readed s))
          (let ((res (get-output-string s)))
            (close f)
            (close s)
            res))
        #f)))

;; Run system command on remote guile cl
(define (vm:sh:run-command cl command)
  (cl `(system ,command)))

;; Take action on service. Action may be start|stop|restart
(define (vm:sh:service cl service action)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:service) " " service " " action))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Service ~A ~A (~A) result ~A" service action cmd res)
    (= res 0)))

;; Poweroff vm. If clean is set, clean shutdown is proceed, otherwise
;; unclean (-nf) shutdown is proceeed.
(define (vm:sh:shutdown cl clean)
  (ipvsts:log "Shutting down machine")
  (let ((res
         (vm:sh:run-command
          cl
          (string-append (cfg 'test:vm:sh:cmd:poweroff)
                         " "
                         (if (not clean) "-nf" "")))))
    (cond ((eof-object? res) #t)
          ((= res 0) #t)
          (#t #f))))

;; Install package(s)
(define (vm:sh:yum-install cl packages)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:yum) " install -y " packages))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Installing packages ~A (~A) result ~A" packages cmd res)
    (= res 0)))

;; Perform update
(define (vm:sh:yum-update cl)
  (ipvsts:log "Running yum update")
  (and
   (= (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:yum) " clean all")) 0)
   (= (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:yum) " update -y")) 0)
   (= (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:yum) " clean all")) 0)))
