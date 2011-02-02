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
(define-module (ipvsts vminstall))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts utils))
(use-modules (ipvsts netfuncs))

(export vminstall:download vminstall:disk-create vminstall:create-ks vminstall:run-install)

;; Download initrd and kernel files and saves them in ipvsts:vm-dir
(define (vminstall:download)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (vmlinuz-path (string-append (cfg 'test:install-url) "/"
                                      (cfg 'vminstall:http-path:vmlinuz)))
         (initrd-path (string-append (cfg 'test:install-url) "/"
                                     (cfg 'vminstall:http-path:initrd))))
    (ipvsts:log "creating ~A" vm-dir)
    (mkdir-safe vm-dir)
    (ipvsts:log "download ~A" vmlinuz-path)
    (http-get-file (string-append vm-dir "/vmlinuz") vmlinuz-path)
    (ipvsts:log "download ~A" initrd-path)
    (http-get-file (string-append vm-dir "/initrd.img") initrd-path)))

;; Create base image ipvsts:vm-dir/test:name named base.img with qcow2 format
(define (vminstall:disk-create)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (cfg 'vminstall:disk:name))
         (disk-size (cfg 'ipvsts:vm-disk-size))
         (system-args (string-append
                       (cfg 'ipvsts:qemu-img) " create -f " (cfg 'vminstall:disk:format)
                       " " vm-dir "/" disk-name ".img" " " disk-size " >/dev/null"))
         (stat (system system-args)))
    (ipvsts:log "creating image ~A return val ~A" system-args (status:exit-val stat))
    (if (= (status:exit-val stat) 0) #t #f)))

;; Returns kickstart
(define (vminstall:create-ks)
  (define (append-file port from-file to-file)
    (let ((path (find-file-in-lpath from-file)))
      (if path
          (let ((file (open-file (string-append path "/" from-file) "r")))
            (if (not file) #f
                (let ()
                  (simple-format port "cat << 'EOF' > ~A\n" to-file)
                  (port-cat file port)
                  (display "EOF\n" port)
                  (close file))))
          #f)))

  (let ((format (cfg 'test:distro))
        (os (open-output-string)))
    (simple-format os "install\nurl --url=~A\n" (cfg 'test:install-url))
    (display "text\nlang en_US.UTF-8\nkeyboard us\n" os)

    (if (equal? format 'el5)
        (display "key --skip\n" os))

    (display "network --bootproto=dhcp\n" os)
    (display "zerombr\n" os)
    (display "clearpart --all --initlabel\npart / --size=1024 --grow\npart swap --size=128\n" os)
    (display "bootloader\ntimezone --utc UTC\n" os)

    (cond ((equal? format 'el5)
           (simple-format os "rootpw ~A\n" (cfg 'ipvsts:vm-passwd)))
          (#t
           (simple-format os "rootpw --plaintext ~A\n" (cfg 'ipvsts:vm-passwd))))

    (display "firewall --disabled\nfirstboot --disabled\nselinux --enforcing\nskipx\npoweroff\n" os)
    (display "%packages --nobase\n" os)

    (cond ((equal? format 'el5)
           (display "@Core\n" os))
          (#t
           (display "@Core --nodefaults\n" os)))

    (display "yum\nguile\n" os)

    (if (not (equal? format 'el5))
        (display "%end\n" os))

    (display "%post\n" os)

    (if (not (and
              (append-file os "rguile/ipvsts-rguile.scm" "/usr/local/bin/ipvsts-rguile.scm")
              (append-file os "rguile/init.d/ipvsts-rguile" "/etc/rc.d/init.d/ipvsts-rguile")))
        #f
        (let ()
          (display "chmod 755 /usr/local/bin/ipvsts-rguile.scm /etc/rc.d/init.d/ipvsts-rguile\n" os)
          (display "/sbin/chkconfig --add ipvsts-rguile\n" os)
          (display "/sbin/chkconfig ipvsts-rguile on\n" os)
          (if (not (equal? format 'el5))
              (display "%end\n" os))

          (let ((res (get-output-string os)))
            (close os)
            res)))))

;; Starts installation.
(define (vminstall:run-install)
  (define (http-server cl path)
    (ipvsts:log "client want's to download path ~A" path)
    (cond ((equal? path "/ipvsts.ks")
           (http-serve-string10 cl (vminstall:create-ks (list (cons 'format distro)))))
          (#t #f)))

  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (cfg 'vminstall:disk:name))
         (mem (cfg 'vminstall:mem))
         (system-args (list (cfg 'ipvsts:qemu) "-kernel" (string-append vm-dir "/vmlinuz")
                            "-initrd" (string-append vm-dir "/initrd.img")
                            "-hda" (string-append vm-dir "/" disk-name ".img")
                            "-m" (number->string mem) "-net" "nic,model=virtio" "-net" "user"
                            "-append"
                            (string-append "ks=http://" (cfg 'vminstall:qemu-local-addr) ":"
                                           (number->string (cfg 'vminstall:http-port)) "/ipvsts.ks")
                            "-vnc"
                            (string-append ":" (number->string (1+ (cfg 'test:vm:vnc-base))))))
         (pid (primitive-fork)))
    (cond ((= pid 0)
           (let ()
             (ipvsts:log "installing vm ~A" system-args)
             (apply system* system-args)
             (exit 0)))
          (#t
           (let ((http-port (httpd:init "127.0.0.1" (cfg 'vminstall:http-port))))
             (ipvsts:log "waiting for qemu to download kickstart")
             (httpd:accept http-port http-server)
             (ipvsts:log "kickstart downloaded")
             (close http-port)
             (ipvsts:log "waiting for end of installation")
             (waitpid pid)
             (ipvsts:log "installation finished")
             #t)))))
