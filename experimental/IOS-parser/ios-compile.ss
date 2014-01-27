#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco IOS Policy Compilation
;; Copyright (C) 2009-2014 Christopher Barratt, Tim Nelson, Brown University
;; All rights reserved.
;;
;;  This file is part of Margrave.
;;
;;  Margrave is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Margrave is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scheme/list)
(require scheme/class)
(require scheme/file)
(require scheme/pretty)
(require racket/match)
(require racket/dict)
(require "ios.ss")
(require "ios-parse.ss")
(require "routers.rkt")

(require web-server/templates)
(require (planet murphy/protobuf:1:1))

(require "ios-flowlog-helpers.rkt")
(require racket/string)
(require (only-in srfi/13 string-pad))

(provide compile-configurations)

(define-syntax combine-rules
  (syntax-rules ()
    [(_ configurations accessor)
     (apply append (map (λ (configuration)
                          ; Please keep and comment out. Useful to know immediately which config is failing:
                          (printf "Processing: ~v~n" (send (send configuration get-hostname) text)) ; DEBUG
                          (send configuration accessor))
                        configurations))]))

;; Takes a filename of a template and a Racket dictionary and fills in the values of the fields
(define (render-template filename dict)
  (define namespace-for-template (make-empty-namespace))
  (namespace-attach-module (current-namespace) 'web-server/templates
namespace-for-template)
  (hash-map dict
    (lambda (key value)
      (define name-of-identifier (string->symbol key))
      (namespace-set-variable-value!
        name-of-identifier
        value
        #f
        namespace-for-template)))
  (parameterize [(current-namespace namespace-for-template)]
    (namespace-require 'web-server/templates))
  (define to-eval #`(include-template #,(datum->syntax
#'render-template filename)))
  (eval to-eval namespace-for-template))

;; string (listof string) boolean -> void
;; pass filename only if there is more than one configuration to do
(define (compile-configurations root-path filenames default-ACL-permit)
  
  ; Let user errors through; catch all other errors and give a "friendly" error message.
  (with-handlers ([(lambda (e) (and #f (exn:fail? e) (not (exn:fail:user? e))))
                   (lambda (e) (raise-user-error (format "Unrecoverable error parsing IOS configurations. Please report this error to the Margrave maintainers. The internal error was: ~a.~n" e)))])
    (define configurations (map (λ (filename)                                
                                  (parse-IOS (open-input-file (make-path root-path filename)
                                                              #:mode
                                                              'text)
                                             default-ACL-permit))
                                filenames))
    
    ; assoc returns the (k, v) pair. we just want v.
    (define (assoc2 key lst) (second (assoc key lst)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Decorrelate and produce rule sets for appropriate policy decisions

    (define inboundacl (assoc2 'permit (policy '(permit) (combine-rules configurations inbound-ACL-rules))))
    (define outboundacl (assoc2 'permit (policy '(permit) (combine-rules configurations outbound-ACL-rules))))
    
    (define reflexive-inserts (assoc2 'insert (policy '(insert) (combine-rules configurations reflexive-insert-rules))))
    (define reflexive-tcp (filter (lambda (arule) (equal? 'tcp (get-proto-for-rule arule))) reflexive-inserts))
    (define reflexive-udp (filter (lambda (arule) (equal? 'udp (get-proto-for-rule arule))) reflexive-inserts))
    
    (define inboundacl-tcp (filter (lambda (arule) (equal? 'tcp (get-proto-for-rule arule))) inboundacl))        
    (define inboundacl-udp (filter (lambda (arule) (equal? 'udp (get-proto-for-rule arule))) inboundacl))
    (define inboundacl-ip (filter (lambda (arule) (equal? 'ip (get-proto-for-rule arule))) inboundacl))
    (define outboundacl-tcp (filter (lambda (arule) (equal? 'tcp (get-proto-for-rule arule))) outboundacl))
    (define outboundacl-udp (filter (lambda (arule) (equal? 'udp (get-proto-for-rule arule))) outboundacl))
    (define outboundacl-ip (filter (lambda (arule) (equal? 'ip (get-proto-for-rule arule))) outboundacl))           

    (define insidenat (assoc2 'translate (policy '(translate) (combine-rules configurations inside-NAT-rules))))
    (define outsidenat (assoc2 'translate (policy '(translate) (combine-rules configurations outside-NAT-rules))))
    
    (define local-switch (policy '(forward pass) (combine-rules configurations local-switching-rules)))
    (define localswitching-forward (assoc2 'forward local-switch))
    (define localswitching-pass (assoc2 'pass local-switch))
    
    (define network-switch (policy '(forward pass) (combine-rules configurations network-switching-rules)))
    (define networkswitching-forward (assoc2 'forward network-switch))
    (define networkswitching-pass (assoc2 'pass network-switch))
    
    (define static-route (policy '(forward route pass) (combine-rules configurations static-route-rules)))
    (define staticroute-forward (assoc2 'forward static-route))
    (define staticroute-route (assoc2 'route static-route))
    (define staticroute-pass (assoc2 'pass static-route))
       
    (define policy-route (policy '(forward route pass) (combine-rules configurations policy-routing-rules)))
    (define policyroute-forward (assoc2 'forward policy-route))
    (define policyroute-route (assoc2 'route policy-route))
    (define policyroute-pass (assoc2 'pass policy-route))
    
    (define default-policy-route (policy '(forward route pass) (combine-rules configurations default-policy-routing-rules)))           
    (define defaultpolicyroute-forward (assoc2 'forward default-policy-route))
    (define defaultpolicyroute-route (assoc2 'route default-policy-route))
    (define defaultpolicyroute-pass (assoc2 'pass default-policy-route))    

    ;;;;;;;;;;;;; Get next-hop ;;;;;;;;;;;;;;
    ; IN: p[fields]
    ; OUT: next-hop [existential variable]
    ; policyroute-route || pr-pass and sr-route || pr-pass and sr-pass and defpol-route 
    ; since these policies were first-applicable, decorrelation within pols is already done
    (define next-hop-fragment 
      `(or ,@policyroute-route
           (and (or ,@policyroute-pass)
                (or ,@staticroute-route))
           (and (or ,@policyroute-pass)
                (or ,@staticroute-pass)
                (or ,@defaultpolicyroute-route))))
    ; Caveat: these will use flat router-names 
    ; Caveat: these will use flat interface names
    ; NOTE: We DO NOT SUPPORT routes that use an interface-name instead of a next-hop. 
    ;       Thus everything needs a next-hop or will be dropped
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; For now, don't use NetworkSwitching or LocalSwitching.
    ; Instead, just feed the resulting program tuples in the subnets table.        
   
    ; Dictionaries for protobufs -> mininet
    (define startup-vars (make-hash))
    (define router-vars (make-hash))
    (define acl-vars (make-hash))
    (dict-set! router-vars "needs-nat-disj" "")
    ; Helper dictionaries for formula construction
    (define nn-for-router (make-hash))    
    (define dst-local-subnet-for-router (make-hash))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; temporary helpers for while we have to generate router_portmap from Racket
    (define max-subnets 0)

    (define (maybe-update-max-subnets num)
      (set! max-subnets (max num max-subnets)))

    (define (make-routerportmap num)
      (if (< num 1)
          ""
          (begin
            (let* ([n (number->string (add1 num))]
                   [tmp (* 2 num)]
                   [hstPt (number->string (sub1 tmp))]
                   [rtrPt (number->string tmp)]
                   [s (string-append "  INSERT (" n ", " hstPt ", " rtrPt ") INTO router_portmap;\n")])
              (string-append s (make-routerportmap (sub1 num)))))))

    ;;;;;;;;;;;;;;;;;;;
    ; Need to assign an ID to the router and an ID to the interface
    (define (ifacedef->tuples arouter interface-defns nat-dpid rname rnum ifindex i ridx acl-dpid)
      (match i
        [`(,name ,primaddr (,primnwa ,primnwm) ,secaddr (,secnwa ,secnwm) ,nat-side) 
         (define inum (number->string (+ 1 ifindex)))
         ; offset the port number on the router by 1, since 1 is reserved for the attached NAT switch
         (define ptnum (number->string (+ 2 ifindex)))

         ;(printf "ridx=~v; rnum=~v; ifindex=~v; rname=~v;~n" ridx rnum ifindex rname) ; DEBUG

         ;;;;;;;;;;;;;;;;         
         ; Produce tuples
         ; TODO: if secondary, need to increment tr_dpid
         (define prim (vals->subnet primaddr primnwa primnwm rnum inum ptnum ridx))
         (define sec (if secaddr (vals->subnet primaddr primnwa primnwm rnum inum ptnum ridx) #f))
         (define alias (vals->ifalias rname name inum))         

         ; local subnets
         (dict-set! dst-local-subnet-for-router rnum 
                    (cons `(= pkt.nwDst ,(string-append primnwa "/" primnwm))
                          (dict-ref dst-local-subnet-for-router rnum)))
         (when sec
           (dict-set! dst-local-subnet-for-router rnum
                      (cons `(= pkt.nwDst ,(string-append secnwa "/" secnwm))
                            (dict-ref dst-local-subnet-for-router rnum))))

         ; needs nat?
         (define needs-nat (if (and nat-side (equal? nat-side 'inside))
                               (string-append (ifvals->needs-nat nn-for-router rnum rname primnwa primnwm) 
                                              (ifvals->needs-nat nn-for-router rnum rname secnwa secnwm))
                               empty))                 

         (define acldefn (vals->ifacldefn acl-dpid ifindex rname name))
         (define natconfigs (if-pair->natconfig interface-defns nat-side nat-dpid))                                    
         ;;;;;;;;;;;;;;;;;

         ;(printf "dst local: ~v~n~v~n" dst-local-subnet-for-router) ; DEBUG

         ;;;;;;;;;;;;;;;;;
         ; generate protobufs as well
         (define aninterf (subnet ""))

         (set-subnet-addr! aninterf primnwa)
         (set-subnet-mask! aninterf (string->number primnwm))
         (set-subnet-gw! aninterf primaddr)

         (set-router-subnets! arouter (cons aninterf (router-subnets arouter) ))

         ; Deal with secondary subnet, if there is one
         (when secaddr 
           (printf "WARNING! Secondary interface detected. Please confirm that the primary and secondaries get different IDs.~n")
           (define aninterf2 (subnet ""))

           (set-subnet-addr! aninterf2 secnwa)
           (set-subnet-mask! aninterf2 (string->number secnwm))
           (set-subnet-gw! aninterf2 secaddr)

           (set-router-subnets! arouter (cons aninterf2 (router-subnets arouter) )))
         ;;;;;;;;;;;;;;;;;

         ; Finally, return the result tuples (protobuf changes are side-effects)
         ; Keep the tuples that are non-#f
         (filter (lambda (x) x) (list prim sec alias needs-nat acldefn natconfigs))]
        [else (pretty-display i) (error "ifacedef->tuple")]))

    ;;;;;;;;;;;;;;;;;;;
    (define (extract-hosts routers-msg config hostidx)
      (define hostname (symbol->string (send (send config get-hostname) name)))
      (define interfaces (send config get-interfaces))
      (define interface-keys (hash-keys interfaces))
      ;(printf "pre-processing hostname: ~v~n" hostname) ; DEBUG
      (define interface-defns (hash-map interfaces extract-ifs))
      (maybe-update-max-subnets (length interface-defns))
      ;(pretty-display interface-defns) ; DEBUG
      (define hostnum (string-append "0x1000000000" (string-pad (number->string (+ hostidx 1)) 2 #\0)))
      (define self-dpid (string-append "1000000000" (string-pad hostnum 2 #\0))) ; TODO(adf): cleanup
      (define nat-dpid (string-append "4000000000" (string-pad hostnum 2 #\0)))
      (define tr-dpid (string-append "2000000000" (string-pad hostnum 2 #\0)))
      (define acl-dpid (string-append "5000000000" (string-pad hostnum 2 #\0)))
      ; Prepare this list of needs-nat expressions
      (dict-set! nn-for-router hostnum empty)
      (dict-set! dst-local-subnet-for-router hostnum empty)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Confirm that no unsupported NAT variety appears
      (define static-NAT (send config get-static-NAT)) 
      (define dynamic-NAT (send config get-dynamic-NAT))
      (for-each (lambda (anat) (unless (send anat supported-flowlog)
                                 (error (format "unsupported NAT: ~v: ~v" (send anat name (string->symbol hostname) "") (send anat direction)))))
                (append static-NAT dynamic-NAT))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define arouter (router ""))
      (set-router-name! arouter hostname)
      (set-router-self-dpid! arouter self-dpid)
      (set-router-nat-dpid! arouter nat-dpid)
      (set-router-tr-dpid! arouter tr-dpid)
      (set-router-acl-dpid! arouter acl-dpid)
      
      (define iftuples (for/list ([ifdef interface-defns] 
                                  [ifindex (build-list (length interface-defns) values)])                         
                          (ifacedef->tuples arouter interface-defns nat-dpid hostname hostnum ifindex ifdef (+ hostidx 1) acl-dpid)))
      (define routertuple (vals->routertuples hostname hostnum)) 
      (define natinfo (vals->nat (router-nat-dpid arouter) hostnum))      
      (define trinfo (vals->tr (router-tr-dpid arouter) hostnum))
      (define aclinfo (vals->acl (router-acl-dpid arouter) hostnum))
      
      ; TODO(tn)+TODO(adf): secondary subnets on interfaces with nat?      
      
      ; finally, reverse since subnets are attached in the order they appear in the protobuf
      (set-router-subnets! arouter (reverse (router-subnets arouter)))      
      (set-routers-routers! routers-msg (cons arouter (routers-routers routers-msg)))

      ; Return the gathered tuples. protobufs changes are side-effects
      (string-append* (flatten (cons routertuple (cons iftuples (cons natinfo (cons trinfo aclinfo)))))))
  
    (define routers-msg (routers ""))
    (set-routers-subnet-base-dpid! routers-msg "300000000000")
    (define startupinserts (string-append* (for/list ([config configurations] [hostidx (build-list (length configurations) values)]) 
                                             
                                             (extract-hosts routers-msg config hostidx))))
            
    ; output the router message for this router
    (call-with-output-file (make-path root-path "IOS.pb.bin") #:exists 'replace
      (lambda (out) 
        (serialize routers-msg out)))
            
    ; First up, generate StartupConfig   
    (dict-set! startup-vars "basename" root-path)
    (dict-set! startup-vars "startupinserts" startupinserts)
    (dict-set! startup-vars "routerportmap" (make-routerportmap max-subnets))
    
    (store (render-template "templates/StartupConfig.template.flg" startup-vars)
           (make-path root-path "IOS.flg"))

    ; generate L3external and L3router

    (dict-set! router-vars "basename" root-path)
    (dict-set! router-vars "nexthop-fragment" (sexpr-to-flowlog next-hop-fragment #f))
    (dict-set! router-vars "nexthop-fragment-for-tr" (string-replace (sexpr-to-flowlog next-hop-fragment #f)
                                                                     "pkt.locSw" "router"))
    
    (dict-set! router-vars "needs-nat-disj" (build-per-router-fmla-from-hash nn-for-router))         
    (dict-set! router-vars "dst-local-subnet" (build-per-router-fmla-from-hash dst-local-subnet-for-router))

    (store (render-template "templates/L3external.template.flg" router-vars)
           (make-path root-path "L3external.flg"))

    (store (render-template "templates/L3router.template.flg" router-vars)
           (make-path root-path "L3router.flg"))

    ; generate L3acl
    ; IP rules to the IP block. Will also apply to TCP packets. So don't duplicate!
    (dict-set! acl-vars "inboundacl-tcp" (sexpr-to-flowlog `(or ,@inboundacl-tcp) #t))
    (dict-set! acl-vars "inboundacl-udp" (sexpr-to-flowlog `(or ,@inboundacl-udp) #t))
    (dict-set! acl-vars "inboundacl-ip" (sexpr-to-flowlog `(or ,@inboundacl-ip) #t))
    (dict-set! acl-vars "outboundacl-tcp" (sexpr-to-flowlog `(or ,@outboundacl-tcp) #t))
    (dict-set! acl-vars "outboundacl-udp" (sexpr-to-flowlog `(or ,@outboundacl-udp) #t))
    (dict-set! acl-vars "outboundacl-ip" (sexpr-to-flowlog `(or ,@outboundacl-ip) #t))

    (dict-set! acl-vars "reflexive-inserts-tcp" "")
    (dict-set! acl-vars "reflexive-inserts-udp" "")
    (define reflexive-insert-tcp-texts (map reflexive-rule-to-flowlog reflexive-tcp))    
    (define reflexive-insert-udp-texts (map reflexive-rule-to-flowlog reflexive-udp)) 
    (unless (empty? reflexive-insert-tcp-texts)
      (dict-set! acl-vars "reflexive-inserts-tcp" (string-append* (cons "ON tcp_packet(pkt): \n" reflexive-insert-tcp-texts))))
    (unless (empty? reflexive-insert-udp-texts)
      (dict-set! acl-vars "reflexive-inserts-udp" (string-append* (cons "ON udp_packet(pkt): \n" reflexive-insert-udp-texts))))
    
    (store (render-template "templates/L3acl.template.flg" acl-vars)
           (make-path root-path "L3acl.flg"))

    ; Finally, we copy the template files which just need their INCLUDE line set properly

    (define basename-only (make-hash))
    (dict-set! basename-only "basename" root-path)

    (define (copy-with-basename filename)
      (store (render-template (string-append "templates/" filename ".template.flg")
                              basename-only)
             (make-path root-path (string-append filename ".flg"))))

    (copy-with-basename "Arp_Cache")
    (copy-with-basename "Mac_Learning.inc")
    (copy-with-basename "NATgeneric")
    (copy-with-basename "NIB.inc")

    ; For debugging purposes:
    (store inboundacl (make-path root-path "InboundACL.p"))
    (store outboundacl (make-path root-path "OutboundACL.p"))
    (store reflexive-inserts (make-path root-path "ReflexiveDebug.p"))
    (store insidenat (make-path root-path "InsideNAT.p"))
    (store outsidenat (make-path root-path "OutsideNAT.p"))
    (store local-switch (make-path root-path "LocalSwitching.p"))
    (store network-switch (make-path root-path "NetworkSwitching.p"))
    (store static-route (make-path root-path "StaticRoute.p"))
    (store policy-route (make-path root-path "PolicyRoute.p"))
    (store default-policy-route (make-path root-path "DefaultPolicyRoute.p"))))

;; string string -> path
(define (make-path base file)
  (build-path (string->path base) (string->path file)))

;; any path -> void
(define (store contents path)
  (begin
    (let [(port (open-output-file path #:mode 'text #:exists 'replace))]
      (pretty-display contents port)  ; FLOWLOG changed to pretty-display from pretty-print
      (close-output-port port))))
