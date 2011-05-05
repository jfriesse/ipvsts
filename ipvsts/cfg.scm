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

(use-modules (ipvsts utils))

(export cfg set-cfg! set-alist-cfg! set-unsetcfg! set-alist-unsetcfg!
        call-with-cfg)

(define cfg-ht (list (make-hash-table)))

;; Return top value from hash tables list
(define (cfg-top)
  (car cfg-ht))

;; Discards (pop) top value from hash tables list
(define (cfg-pop)
  (set! cfg-ht (cdr cfg-ht)))

;; Push newly allocated hash table with content from current top hash table to hash tables
;; list. This means, that this new table becomes new top
(define (cfg-push)
  (set! cfg-ht (append (list (hash-table-clone (cfg-top))) cfg-ht)))

;; Retreives cfg key value
(define (cfg var)
  (hash-ref (cfg-top) var))

;; Set cfg key to val
(define (set-cfg! var val)
  (hash-set! (cfg-top) var val))

;; Stores list of (key . value) pairs
(define (set-alist-cfg! alist)
  (cond ((null? alist) #t)
        (#t
         (set-cfg! (caar alist) (cdar alist))
         (set-alist-cfg! (cdr alist)))))

;; Set variable only if it's not already set
(define (set-unsetcfg! var val)
  (if (eq? (hash-get-handle (cfg-top) var) #f)
      (set-cfg! var val)))

;; Stores list of (key . value) pairs but only if key is not already set
(define (set-alist-unsetcfg! alist)
  (cond ((null? alist) #t)
        (#t
         (set-unsetcfg! (caar alist) (cdar alist))
         (set-alist-unsetcfg! (cdr alist)))))

;; Call thunk with newly allocated hash table (but initially with old values) and set new values
;; from alist (set-alist-cfg!). On end of thunk, newly allocated hash table is discarded
(define (call-with-cfg alist thunk)
  (let* ((start (lambda () (cfg-push) (set-alist-cfg! alist)))
         (stop (lambda () (cfg-pop))))
    (dynamic-wind start thunk stop)))

;; Set default values
(define (set-defaults!)
  ;; Fixed (non computed) values
  (set-alist-cfg!
   '((ipvsts:qemu-img . "/usr/bin/qemu-img")
     (ipvsts:qemu . "/usr/bin/qemu-kvm")
     (ipvsts:http-mirror . "http://download")
     (ipvsts:vm-disk-size . "2G")
     (ipvsts:vm-passwd . "password")
     (test:arch . "unknown")
     (test:chkconfig:off . ("ipvsadm" "iptables" "ip6tables" "arptables_jf" "network"))
     (test:chkconfig:on . #f)
     (test:force-vm-create . #f)
     (test:version . "unknown")
     (test:name . "unknown")
     (test:install-url . "unknown")
     (test:update-url . #f)
     (test:vm:disable-dad . #f)
     (test:vm:mem . 256)
     (test:vm:net . (user))
     (test:vm:rguile-port-base . 2300)
     (test:vm:update-mem . 768)
     (test:vm:vnc-base . 10)
     (test:vm:ip:addr . "192.168.~A.~A")
     (test:vm:ip:mask . "255.255.255.0")
     (test:vm:ip6:addr . "fec0:1111:2222:3333:4444:5555:~A:~A")
     (test:vm:ip6:prefix . "112")
     (test:vm:macaddr . "52:54:00:00:~A:~A")
     (test:vm:mcast-addr . "239.255.0.1")
     (test:vm:mcast-port-base . 4096)
     (test:vm:max-qemu-start-time . 10)
     (test:vm:selinux . #t)
     (test:vm:sh:cmd:chkconfig . "/sbin/chkconfig")
     (test:vm:sh:cmd:col . "/usr/bin/col")
     (test:vm:sh:cmd:grep . "/bin/grep")
     (test:vm:sh:cmd:ipvsadm . "/sbin/ipvsadm")
     (test:vm:sh:cmd:man . "/usr/bin/man")
     (test:vm:sh:cmd:modprobe . "/sbin/modprobe")
     (test:vm:sh:cmd:poweroff . "/sbin/poweroff")
     (test:vm:sh:cmd:reboot . "/sbin/reboot")
     (test:vm:sh:cmd:rm . "/bin/rm")
     (test:vm:sh:cmd:rpm . "/bin/rpm")
     (test:vm:sh:cmd:service . "/sbin/service")
     (test:vm:sh:cmd:setenforce . "/usr/sbin/setenforce")
     (test:vm:sh:cmd:sysctl . "/sbin/sysctl")
     (test:vm:sh:cmd:tee . "/usr/bin/tee")
     (test:vm:sh:cmd:yum . "/usr/bin/yum")
     (test:vm:sh:cmd:udevadm . "/sbin/udevadm")
     (test:vm:sh:disable-dad-key . ".accept_dad")
     (test:vm:sh:local-subsys-lock . "/var/lock/subsys/local")
     (test:vm:sh:manpage:ipvsadm . "ipvsadm")
     (test:vm:sh:network-scripts-dir . "/etc/sysconfig/network-scripts")
     (test:vm:sh:network-scripts-name . "ifcfg-eth~A")
     (test:vm:sh:network-scripts-rm . "ifcfg-eth*")
     (test:vm:sh:service:ipvsadm . "ipvsadm")
     (test:vm:sh:module-name:ipvs . "ip_vs")
     (test:vm:sh:module-name:ipv6 . "ipv6")
     (test:vm:sh:file:proc-modules . "/proc/modules")
     (test:vm:sh:file:proc-ip_vs . "/proc/net/ip_vs")
     (test:vm:sh:yum-repos-dir . "/etc/yum.repos.d")
     (test:vm:sh:udev-net-file . "/etc/udev/rules.d/70-persistent-net.rules")
     (test:yum:int-repos . ("."))
     (test:yum:packages-to-install . "ipvsadm arptables_jf man")
     (test:rpm:packages-to-install . #f)
     (test:distro . 'el6)
     (test:vm:compress-base-image . #f)
     (vminstall:disk:format . "qcow2")
     (vminstall:disk:name . "base")
     (vminstall:http-path:vmlinuz . "images/pxeboot/vmlinuz")
     (vminstall:http-path:initrd . "images/pxeboot/initrd.img")
     (vminstall:http-port . 8888)
     (vminstall:qemu-local-addr . "10.0.2.2")
     (vminstall:mem . 768)))

  ;; Computed values
  (set-cfg! 'ipvsts:vm-dir (string-append (getenv "HOME") "/vms"))
  (set-cfg! 'test:log-file-name (string-append (getenv "HOME") "/ipvsts-default.log"))
  (set-cfg! 'test:disk:name (cfg 'vminstall:disk:name))
  (set-cfg! 'test:vm:max-boot-time (* 5 60))
  (set-cfg! 'test:vm:max-shutdown-time (* 2 60))
  (set-cfg! 'test:vm:sh:udev-net-str
            (string-append "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"?*\", ATTR{address}==\""
                           "~A\", ATTR{dev_id}==\"0x0\", ATTR{type}==\"1\", KERNEL==\"eth*\", "
                           "NAME=\"~A\""))
  (set-cfg! 'test:yum:update-int-repos #f)
  (set-cfg! 'vminstall:max-install-time (* 60 60)))

;; Load user defaults from ~/.ipvsts if such file exists
(define (load-user-defaults!)
  (if (access? (string-append (getenv "HOME") "/.ipvsts") R_OK)
    (load (string-append (getenv "HOME") "/.ipvsts"))))

(set-defaults!)
(load-user-defaults!)
