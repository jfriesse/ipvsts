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

(define-module (ipvsts cfg))

(export cfg set-cfg! set-alist-cfg! set-unsetcfg! set-alist-unsetcfg!
        get-param-val)

(define cfg-ht (make-hash-table))

;; Retreives cfg key value
(define (cfg var)
  (hash-ref cfg-ht var))

;; Set cfg key to val
(define (set-cfg! var val)
  (hash-set! cfg-ht var val))

;; Stores list of (key . value) pairs
(define (set-alist-cfg! alist)
  (cond ((null? alist) #t)
        (#t
         (set-cfg! (caar alist) (cdar alist))
         (set-alist-cfg! (cdr alist)))))

;; Set variable only if it's not already set
(define (set-unsetcfg! var val)
  (if (eq? (hash-get-handle cfg-ht var) #f)
      (set-cfg! var val)))

;; Stores list of (key . value) pairs but only if key is not already set
(define (set-alist-unsetcfg! alist)
  (cond ((null? alist) #t)
        (#t
         (set-unsetcfg! (caar alist) (cdar alist))
         (set-alist-unsetcfg! (cdr alist)))))

;; Set default values
(define (set-defaults!)
  ;; Fixed (non computed) values
  (set-alist-cfg!
   '((ipvsts:qemu-img . "/usr/bin/qemu-img")
     (ipvsts:qemu . "/usr/bin/qemu-kvm")
     (ipvsts:http-mirror . "http://download")
     (ipvsts:vm-disk-size . "1.5G")
     (ipvsts:vm-passwd . "password")
     (test:arch . "unknown")
     (test:version . "unknown")
     (test:name . "unknown")
     (test:install-url . "unknown")
     (test:vm:mem . 128)
     (test:vm:net . (user))
     (test:vm:vnc-base . 10)
     (test:vm:rguile-port-base . 2300)
     (test:vm:ip:addr . "192.168.~A.~A")
     (test:vm:ip:mask . "255.255.255.0")
     (test:vm:macaddr . "52:54:00:00:~A:~A")
     (test:vm:mcast-addr . "239.255.0.1")
     (test:vm:mcast-port-base . 4096)
     (test:vm:max-qemu-start-time . 10)
     (test:vm:sh:cmd:chkconfig . "/sbin/chkconfig")
     (test:vm:sh:cmd:poweroff . "/sbin/poweroff")
     (test:vm:sh:cmd:rm . "/bin/rm")
     (test:vm:sh:cmd:service . "/sbin/service")
     (test:vm:sh:cmd:yum . "/usr/bin/yum")
     (test:vm:sh:cmd:udevadm . "/sbin/udevadm")
     (test:vm:sh:network-scripts-dir . "/etc/sysconfig/network-scripts")
     (test:vm:sh:network-scripts-name . "ifcfg-eth~A")
     (test:vm:sh:network-scripts-rm . "ifcfg-eth*")
     (test:vm:sh:yum-repos-dir . "/etc/yum.repos.d")
     (test:vm:sh:udev-net-file . "/etc/udev/rules.d/70-persistent-net.rules")
     (test:distro . 'el6)
     (vminstall:disk:format . "qcow2")
     (vminstall:disk:name . "base")
     (vminstall:http-path:vmlinuz . "images/pxeboot/vmlinuz")
     (vminstall:http-path:initrd . "images/pxeboot/initrd.img")
     (vminstall:http-port . 8888)
     (vminstall:qemu-local-addr . "10.0.2.2")
     (vminstall:mem . 512)))

  ;; Computed values
  (set-cfg! 'ipvsts:vm-dir (string-append (getenv "HOME") "/vms"))
  (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-default.log"))
  (set-cfg! 'test:disk:name (cfg 'vminstall:disk:name))
  (set-cfg! 'test:vm:max-boot-time (* 5 60))
  (set-cfg! 'test:vm:sh:udev-net-str
            (string-append "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"?*\", ATTR{address}==\""
                           "~A\", ATTR{dev_id}==\"0x0\", ATTR{type}==\"1\", KERNEL==\"eth*\", NAME=\""
                           "~A\""))
  (set-cfg! 'vminstall:max-install-time (* 60 60)))

;; Load user defaults from ~/.ipvsts if such file exists
(define (load-user-defaults!)
  (if (access? (string-append (getenv "HOME") "/.ipvsts") R_OK)
    (load (string-append (getenv "HOME") "/.ipvsts"))))

;; Return value of parameter. If args is null, or no name-arg is found in
;; assoc list (car args), name-cfg from cfg is returned. Othervise value from
;; assoc list.
(define (get-param-val name-arg name-cfg args)
  (if (and (not (null? args)) (assoc name-arg (car args)))
      (cdr (assoc name-arg (car args)))
      (cfg name-cfg)))

(set-defaults!)
(load-user-defaults!)
