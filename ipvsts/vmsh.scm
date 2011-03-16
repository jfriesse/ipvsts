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
(use-modules (ipvsts netfuncs))
(use-modules (rguile client))

(export vm:sh:add-int-update-yum-repo vm:sh:add-int-yum-repo vm:sh:add-yum-repo
        vm:sh:chkconfig vm:sh:create-file vm:sh:delete-yum-repo vm:sh:get-file
        vm:sh:is-module-loaded? vm:sh:reboot vm:sh:rpm-install vm:sh:run-command
        vm:sh:run-command-out vm:sh:service vm:sh:shutdown vm:sh:yum-install
        vm:sh:yum-update vm:sh:set-selinux vm:sh:set-disable-dad vm:sh:modprobe)

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

;; Return #t if module is loaded, otherwise #f
(define (vm:sh:is-module-loaded? cl module)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:grep) " " module " "
                             (cfg 'test:vm:sh:file:proc-modules)))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Module ~A loaded = ~A" module (= res 0))
    (= res 0)))

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

;; Reboot vm. If clean is set, clean shutdown is proceed, otherwise
;; unclean (-nf) shutdown is proceeed. If wait-for-boot parameter is set,
;; function waits for boot but maximum for test:vm:max-boot-time
;; host is host where vm is running on given serial line port
(define (vm:sh:reboot host port clean wait-for-boot)
  (ipvsts:log "Rebooting machine")
  (let ((res
         (vm:sh:run-command
          (rguile-client host port)
          (string-append "(sleep 1; "
                         (cfg 'test:vm:sh:cmd:service) " " "ipvsts-rguile" " stop;"
                         (cfg 'test:vm:sh:cmd:reboot)
                         " "
                         (if (not clean) "-nf" "")
                         ") &"))))
    (cond ((= res 0)
           (sleep 3)
           (if wait-for-boot
               (let ()
                 (ipvsts:log "Waiting for boot up")
                 (if (rguile-client-wait-for-operational-state
                      host
                      port
                      (cfg 'test:vm:max-boot-time))
                     (let ()
                       #t)
                     (let ()
                       (ipvsts:log "wait for operational client exceed time limit ~A"
                                   (cfg 'test:vm:max-boot-time))
                       #f)))
               #t))
          (#t #f))))

;; Install RPM package(s)
(define (vm:sh:rpm-install cl packages)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:rpm) " --oldpackage --replacepkgs -U " packages))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Installing packages ~A (~A) result ~A" packages cmd res)
    (= res 0)))

;; Run system command on remote guile cl
(define (vm:sh:run-command cl command)
  (let* ((out-file "/tmp/rguile-system-out.txt")
         (cf-res (vm:sh:create-file cl "" out-file))
         (res (cl `(system (string-append ,command " >" ,out-file " 2>&1")))))
    (ipvsts:log "Command \"~A\" (~A):\n~A"
                command res (vm:sh:get-file cl out-file))
    res))

;; Run system command on remote guile cl. Return is cons of error code and output
(define (vm:sh:run-command-out cl command)
  (let ((res (cl `(system (string-append ,command " >/tmp/rguile-system-out.txt 2>&1"))))
        (output (vm:sh:get-file cl "/tmp/rguile-system-out.txt")))
    (ipvsts:log "Command \"~A\" (~A):\n~A"
                command res output)
    (cons res output)))

;; Take action on service. Action may be start|stop|restart
(define (vm:sh:service cl service action)
  (let* ((cmd (string-append (cfg 'test:vm:sh:cmd:service) " " service " " action))
         (res (vm:sh:run-command cl cmd)))
    (ipvsts:log "Service ~A ~A (~A) result ~A" service action cmd res)
    (= res 0)))

;; Poweroff vm. If clean is set, clean shutdown is proceed, otherwise
;; unclean (-nf) shutdown is proceeed. If wait-for-end parameter is set,
;; function waits for shutdown but maximum for test:vm:max-shutdown-time
;; host is host where vm is running on given serial line port
(define (vm:sh:shutdown host port clean wait-for-end)
  (ipvsts:log "Shutting down machine")
  (let ((res
         (vm:sh:run-command
          (rguile-client host port)
          (string-append (cfg 'test:vm:sh:cmd:poweroff)
                         " "
                         (if (not clean) "-nf" "")))))
    (cond ((eof-object? res) #t)
          ((= res 0)
           (if wait-for-end
               (let ()
                 (ipvsts:log "Waiting for serial port disconnect")
                 (wait-for-tcp-port-close host port (cfg 'test:vm:max-shutdown-time)))
               #t))
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
   (= (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:yum) " update -y")) 0)
   (= (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:yum) " clean all")) 0)))

;; Set selinux
(define (vm:sh:set-selinux cl)
  (ipvsts:log "Setting selinux")
  (= (vm:sh:run-command cl (string-append
                            (cfg 'test:vm:sh:cmd:setenforce) " "
                            (if (cfg 'test:vm:selinux) "1" "0"))
                        )) 0)

;; Set DAD (Duplicate Address Detection)
(define (vm:sh:set-disable-dad cl)
  (if (cfg 'test:vm:disable-dad)
      (begin
        (ipvsts:log "Disabling DAD (Duplicate Address Detection)")
        (= (vm:sh:run-command cl (string-append
                                  (cfg 'test:vm:sh:cmd:sysctl) " -N -a | "
                                  (cfg 'test:vm:sh:cmd:grep) " -F -- "
                                  (cfg 'test:vm:sh:disable-dad-key)
                                  " | (while read line;do "
                                  (cfg 'test:vm:sh:cmd:sysctl) " $line=0"
                                  ";done)")) 0))
      #t))

;; Modprobe module on remote host
(define (vm:sh:modprobe cl module)
  (ipvsts:log "Loading module ~A" module)
  (= (vm:sh:run-command cl (string-append
                            (cfg 'test:vm:sh:cmd:modprobe) " " module)) 0))
