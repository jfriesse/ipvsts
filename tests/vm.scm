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
(define-module (tests vm))

(use-modules (ipvsts cfg))
(use-modules (ipvsts logging))
(use-modules (ipvsts utils))
(use-modules (ipvsts netfuncs))
(use-modules (rguile client))

(export vm:disk:create-snapshot vm:disk:compress vm:start
        vm:sh:run-command vm:sh:create-file vm:sh:get-file
        vm:sh:shutdown vm:configure-net)

;; Create snapshot from image (name can be in assoc list in 'name key, or test:disk:name) and
;; result image is in new-img
(define (vm:disk:create-snapshot new-img . args)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (get-param-val 'name 'test:disk:name args))
         (system-args (string-append
                       (cfg 'ipvsts:qemu-img) " create -f " (cfg 'vminstall:disk:format) " "
                       "-o" "backing_file=" vm-dir "/" disk-name ".img"
                       " " vm-dir "/" new-img ".img"
                       ">/dev/null"))
         (stat (system system-args)))
    (ipvsts:log "creating snapshot ~A return val ~A" system-args (status:exit-val stat))
    (if (= (status:exit-val stat) 0) #t #f)))

;; Compress image (name can be in assoc list in 'name key, or test:disk:name)
(define (vm:disk:compress . args)
  (let* ((vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
         (disk-name (get-param-val 'name 'test:disk:name args))
         (tmp-disk-name (string-append vm-dir "/" "tmpimg-XXXXXX"))
         (new-tmp-disk-name
          (let ()
            (close (mkstemp! tmp-disk-name))
            tmp-disk-name))
         (system-args (string-append
                       (cfg 'ipvsts:qemu-img) " convert -O " (cfg 'vminstall:disk:format) " -c "
                       " " vm-dir "/" disk-name ".img"
                       " " new-tmp-disk-name
                       ">/dev/null"))
         (stat (system system-args)))
    (ipvsts:log "compressing image ~A return val ~A" system-args (status:exit-val stat))
    (if (= (status:exit-val stat) 0)
        (let ()
          (rename-file new-tmp-disk-name (string-append vm-dir "/" disk-name ".img"))
          #t)
        (let ()
          (delete-file new-tmp-disk-name)
          #f))))

;; Start virtual machine with name and order
;; args (assoc list) may contain mem (amount of memory to give to vm) and
;; net (network configuration). Net is in format ([user | (net . net_id)]*)
;; Return #t on success, otherwise #f
(define (vm:start name order . args)
  (define (get-net-qemu-params order params)
    (define (iter params i res)
      (if (null? params) res
          (let ((nic-params
                 (list "-net" (simple-format #f "nic,vlan=~A,model=virtio,macaddr=~A"
                                             i
                                             (simple-format #f (cfg 'test:vm:macaddr)
                                                            (byte->hexstr order)
                                                            (byte->hexstr i))))))
            (cond  ((equal? (car params) 'user)
                    (iter (cdr params)
                          (+ i 1)
                          (append res nic-params
                                  (list "-net" (simple-format #f "user,vlan=~A" i)))))
                   (#t
                    (iter (cdr params)
                          (+ i 1)
                          (append res nic-params
                                  (list "-net"
                                        (simple-format
                                         #f "socket,vlan=~A,mcast=~A:~A"
                                         i (cfg 'test:vm:mcast-addr)
                                         (+ (cfg 'test:vm:mcast-port-base) (cdar params)))))))))))
    (iter params 0 '()))

  (if (wait-for-tcp-port "127.0.0.1" (+ order (cfg 'test:vm:rguile-port-base)) 0)
      (let ()
        (ipvsts:log "qemu serial port ~A is already used"
                    (+ order (cfg 'test:vm:rguile-port-base)))
        #f)
      (let* ((mem (get-param-val 'mem 'test:vm:mem args))
             (net (get-param-val 'net 'test:vm:net args))
             (vm-dir (string-append (cfg 'ipvsts:vm-dir) "/" (cfg 'test:name)))
             (net-args (get-net-qemu-params order net))
             (vm-args (append (list (cfg 'ipvsts:qemu)
                                    "-hda" (string-append vm-dir "/" name ".img")
                                    "-m" (number->string mem)
                                    "-vnc" (string-append
                                            ":"
                                            (number->string (+ order (cfg 'test:vm:vnc-base))))
                                    "-serial"
                                    (string-append "tcp:127.0.0.1:"
                                                   (number->string
                                                    (+ order (cfg 'test:vm:rguile-port-base)))
                                                   ",server,nowait"))
                              net-args))
             (pid (primitive-fork)))
        (cond ((= pid 0)
               (let ()
                 (ipvsts:log "Running vm ~A" vm-args)
                 (apply execlp (append (list (car vm-args)) vm-args))))
              (#t
               (ipvsts:log "waiting for qemu to boot up vm")
               ;; Give qemu some time to boot and also
               ;; test if process is not hunged
               (if (and
                    (wait-for-tcp-port "127.0.0.1"
                                       (+ order (cfg 'test:vm:rguile-port-base))
                                       (cfg 'test:vm:max-qemu-start-time))
                    (= (car (waitpid pid WNOHANG)) 0))
                   (let ()
                     (if (rguile-client-wait-for-operational-state
                          "127.0.0.1"
                          (+ order (cfg 'test:vm:rguile-port-base))
                          (cfg 'test:vm:max-boot-time))
                         (let ()
                           #t)
                         (let ()
                           (ipvsts:log "wait for operational client exceed time limit ~A"
                                       (cfg 'test:vm:max-boot-time))
                           (kill pid SIGINT)
                           (waitpid pid)
                           #f)))
                   (let ()
                     (ipvsts:log "VM failed to start")
                     #f)))))))

;; Run system command on remote guile cl
(define (vm:sh:run-command cl command)
  (cl `(system ,command)))

;; Create file output from input-str string on remote guile cl
(define (vm:sh:create-file cl input-str output)
  (cl
   `(let ((f (open-file ,output "w")))
      (display ,input-str f)
      (close f))))

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

;; Poweroff vm. If clean is set, clean shutdown is proceed, otherwise
;; unclean (-nf) shutdown is proceeed.
(define (vm:sh:shutdown cl clean)
  (let ((res
         (vm:sh:run-command
          cl
          (string-append (cfg 'test:vm:sh:cmd:poweroff)
                         " "
                         (if (not clean) "-nf" "")))))
    (cond ((eof-object? res) #t)
          ((= res 0) #t)
          (#t #f))))

;; Configure network for virtual machine. cl is rguile client, order is
;; run order (id) of machine and net is list of networks avalilable for VM.
;; List can contain user or (net . net_id) items.
;; Macaddr is generated in format test:vm:macaddr, with two ~A replaced by
;; order (id) and position of interface in list. IPAddr is taken from
;; test:vm:ip:addr where two ~A replaced by net_id and order.
(define (vm:configure-net cl order net)
  (define (gen-ifcfg-file pos net)
    (let ((prefix-str
           (simple-format
            #f
            (string-append "DEVICE=\"eth~A\"\nHWADDR=\"~A\"\n"
                           "NM_CONTROLLED=\"no\"\nONBOOT=\"yes\"\n")
            pos
            (simple-format #f (cfg 'test:vm:macaddr)
                           (byte->hexstr order)
                           (byte->hexstr pos)))))
      (cond ((equal? net 'user)
             (string-append prefix-str "BOOTPROTO=\"dhcp\"\n"))
          (#t
           (simple-format #f "~AIPADDR=\"~A\"\nNETMASK=\"~A\"\n"
                          prefix-str
                          (simple-format #f
                                         (cfg 'test:vm:ip:addr)
                                         (cdr net)
                                         order)
                          (cfg 'test:vm:ip:mask))))))

  (define (store-ifcfg-files)
    (define (iter i net)
      (cond ((null? net) #t)
            (#t
             (if (not
                  (vm:sh:create-file
                   cl
                   (gen-ifcfg-file i (car net))
                   (simple-format #f "~A/~A"
                                  (cfg 'test:vm:sh:network-scripts-dir)
                                  (simple-format #f
                                                 (cfg 'test:vm:sh:network-scripts-name)
                                                 i))))
                 #f
                 (iter (1+ i) (cdr net))))))
    (iter 0 net))

  (define (delete-old-ifcfg-files)
    (=
     (vm:sh:run-command cl (string-append (cfg 'test:vm:sh:cmd:rm) " -f "
                                          (cfg 'test:vm:sh:network-scripts-dir)
                                          "/" (cfg 'test:vm:sh:network-scripts-rm)))
     0))

  (define (gen-net-udev-rules)
    (define (iter i net res)
      (cond ((null? net) res)
            (#t
             (iter (1+ i)
                   (cdr net)
                   (string-append
                    res
                    (simple-format
                     #f
                     (cfg 'test:vm:sh:udev-net-str)
                     (simple-format #f (cfg 'test:vm:macaddr) (byte->hexstr order) (byte->hexstr i))
                     (simple-format #f "eth~A" i))
                    "\n")))))
    (iter 0 net ""))

  (and
   (let ()
     (ipvsts:log "Stopping network")
     (= (vm:sh:run-command
         cl
         (string-append (cfg 'test:vm:sh:cmd:service)
                        " network stop")) 0))

   (let ()
     (ipvsts:log "Deleting old ifcfg files")
     (delete-old-ifcfg-files))
   (let ()
     (ipvsts:log "Storing new ifcfg files")
     (store-ifcfg-files))
   (let ()
     (ipvsts:log "Storing udev network rules")
     (vm:sh:create-file cl
                        (gen-net-udev-rules)
                        (cfg 'test'vm:sh:udev-net-file)))
   (let ()
     (ipvsts:log "Restarting udev")
     (= (vm:sh:run-command
         cl
         (string-append (cfg 'test:vm:sh:cmd:udevadm)
                        " trigger")) 0))
   (let ()
     (ipvsts:log "Starting network")
     (= (vm:sh:run-command
         cl
         (string-append (cfg 'test:vm:sh:cmd:service)
                        " network start")) 0))))
