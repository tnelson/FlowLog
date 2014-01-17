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

(provide compile-configurations)

(define-syntax combine-rules
  (syntax-rules ()
    [(_ configurations accessor)
     (apply append (map (λ (configuration)
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
    
    (define (extract-ifs ifaceid iface)
      (define name (symbol->string (send iface text)))
      (define prim-addr-obj (send iface get-primary-address))
      (define sec-addr-obj (send iface get-secondary-address))
      (define prim-netw-obj (send iface get-primary-network))
      (define sec-netw-obj (send iface get-secondary-network))
      (define nat-side (send iface get-nat-side))

      (define prim-addr (send prim-addr-obj text-address))
      (define sec-addr (if sec-addr-obj  
                           (send sec-addr-obj text-address) 
                           #f))
      (define prim-netw `(,(send prim-netw-obj text-address),(send prim-netw-obj text-mask)))
      (define sec-netw (if sec-netw-obj 
                           `(,(send sec-netw-obj text-address),(send sec-netw-obj text-mask))
                           (list #f #f)))
      
      (unless (equal? (symbol->string ifaceid) name) (error "extract-ifs"))
      `(,name 
        ,prim-addr ,prim-netw
        ,sec-addr ,sec-netw ,nat-side))

    ;// subnets(addr,  mask, gw ip,    gw mac,            locSw,            locpt, trSw)
    ;INSERT (10.0.1.0, 24,   10.0.1.1, ca:fe:ca:fe:00:01, 0x1000000000000001, 2, 0x2000000000000001) INTO subnets;
    
    (define (make-tr-dpid ridx inum ox)
      (string-append (if ox "0x" "") "2" (string-pad (number->string ridx) 2 #\0) "00000000000" (string-pad inum 2 #\0)))         

    (define (vals->subnet addr nwa nwm rnum inum ptnum trsw)
      (define gwmac (string-append "ca:fe:ca:fe:00:" (string-pad inum 2 #\0)))
      (string-append "INSERT (" (string-join (list nwa nwm addr gwmac rnum ptnum trsw) ", ") ") INTO subnets;\n"
                     "INSERT (" (string-join (list addr gwmac) ", ") ") INTO cached; // auto\n"
                     "INSERT (" trsw ") INTO switches_without_mac_learning; // auto\n"))

    (define (vals->ifalias rname iname inum)
      (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"") 
                                                   (string-append "\"" iname "\"") 
                                                   inum) ", ") ") INTO portAlias;\n"))
        
    (define (vals->routertuples rname rnum)
      (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"")                                                    
                                                   rnum) ", ") ") INTO routerAlias;\n"
                     "INSERT (" rnum ") INTO switches_without_mac_learning; // auto\n"))

    (define (vals->nat natnum)
      (string-append "INSERT (0x" natnum ") INTO switches_without_mac_learning; // auto\n"))

    (define startup-vars (make-hash))
    (define router-vars (make-hash))
    (dict-set! router-vars "needs-nat-disj" "")
    
    (define (vals->needs-nat nwa nwm)
      (cond [nwa
             (dict-set! router-vars "needs-nat-disj" (string-append (sexpr-to-flowlog `(= pkt.nwSrc ,(string-append nwa "/" nwm)) #f)
                                                                      (dict-ref router-vars "needs-nat-disj")))
             (string-append "INSERT (" nwa ", " nwm ") INTO needs_nat;\n")]
            [else ""]))

    ;;;;;;;;;;;;;;;;;;;
    ; Need to assign an ID to the router and an ID to the interface
    (define (ifacedef->tuples arouter interface-defns nat-dpid rname rnum ifindex i ridx)
      (match i
        [`(,name ,primaddr (,primnwa ,primnwm) ,secaddr (,secnwa ,secnwm) ,nat-side) 
         (define inum (number->string (+ 1 ifindex)))
         ; offset the port number on the router by 1, since 1 is reserved for the attached NAT switch
         (define ptnum (number->string (+ 2 ifindex)))
         (define trsw (make-tr-dpid ridx inum #t))
         ;(printf "ridx=~v; rnum=~v; ifindex=~v; rname=~v;~n" ridx rnum ifindex rname) ; DEBUG
         
         ; TODO: if secondary, need to increment tr_dpid
         (define prim (vals->subnet primaddr primnwa primnwm rnum inum ptnum trsw))
         (define sec (if secaddr (vals->subnet primaddr primnwa primnwm rnum inum ptnum trsw) #f))
         (define alias (vals->ifalias rname name inum))
         (define needs-nat (if (and nat-side (equal? nat-side 'inside))
                               (string-append (vals->needs-nat primnwa primnwm) 
                                              (vals->needs-nat secnwa secnwm))
                               empty))
                  
         
         (define hostaclnum (string-append "3" (string-pad (number->string ridx) 2 #\0) "00000000000" (string-pad ptnum 2 #\0)))
         (define (vals->ifacldefn ridx iidx rname iname)           
           (string-append "INSERT (0x" hostaclnum ") INTO aclDPID;\n"
                          "INSERT (0x" hostaclnum ") INTO switches_without_mac_learning; // auto\n"
                          "INSERT (\"" (symbol->string (build-acl-name rname iname)) "\", 0x" hostaclnum ") INTO routerAlias;\n"))
                 
         ; For this particular type of nat (source list overload dynamic):
         ; For each private interface (nat = inside), and each public interface (nat = outside)
         ; insert a row into natconfig
         ;; TODO(tn)+TODO(adf): more kinds of NAT?
         (define (if-pair->natconfig)
           (if (and nat-side (equal? nat-side 'inside))                                                                                      
               (for/list ([ifdef2 interface-defns]
                          [ifindex2 (build-list (length interface-defns) values)])                 
                 (match ifdef2 
                   [`(,name2 ,primaddr2 (,primnwa2 ,primnwm2) ,secaddr2 (,secnwa2 ,secnwm2) ,nat-side2)                    
                    (if (and nat-side2 (equal? nat-side2 'outside))
                        (string-append "INSERT (0x" nat-dpid "," "1" "," "1" "," primaddr2 ") INTO natconfig;\n"
                                       "INSERT (" primaddr2 ", 0x6, 10000) INTO seqpt; // auto \n"
                                       "INSERT (" primaddr2 ", 0x11, 10000) INTO seqpt; // auto \n")
                        "")]))
               empty))
         
         (define acldefn (vals->ifacldefn ridx ptnum rname name))
         (define natconfigs (if-pair->natconfig))
                           
         (define result (filter (lambda (x) x) (list prim sec alias needs-nat acldefn natconfigs)))
         
         ; generate protobufs as well
         (define aninterf (subnet ""))
         ;(set-subnet-name! aninterf name)
         ;(set-minterface-id! aninterf ifindex)
                  
         (set-subnet-tr-dpid! aninterf (make-tr-dpid ridx (number->string (+ ifindex 1)) #f))
         (set-subnet-acl-dpid! aninterf hostaclnum)
         (set-subnet-addr! aninterf primnwa)
         (set-subnet-mask! aninterf (string->number primnwm))
         (set-subnet-gw! aninterf primaddr)
         
         (set-router-subnets! arouter (cons aninterf (router-subnets arouter) ))
         
         (when secaddr 
           (printf "WARNING! Secondary interface detected. Please confirm that the primary and secondaries get different IDs.~n")
           (define aninterf2 (subnet ""))
           ;(set-minterface-name! aninterf2 name)         
           ;(set-minterface-id! aninterf2 ifindex)
           (set-subnet-tr-dpid! aninterf2 ifindex)
           (set-subnet-addr! aninterf2 secnwa)
           (set-subnet-mask! aninterf2 (string->number secnwm))
           (set-subnet-gw! aninterf2 secaddr)
           (set-subnet-acl-dpid! aninterf2 hostaclnum)
           (set-router-subnets! arouter (cons aninterf2 (router-subnets arouter) )))
         
         result]
        [else (pretty-display i) (error "ifacedef->tuple")]))

    ;;;;;;;;;;;;;;;;;;;
    (define (extract-hosts routers-msg config hostidx)
      (define hostname (symbol->string (send (send config get-hostname) name)))
      (define interfaces (send config get-interfaces))
      (define interface-keys (hash-keys interfaces))
      ;(printf "pre-processing hostname: ~v~n" hostname) ; DEBUG
      (define interface-defns (hash-map interfaces extract-ifs))
      ;(pretty-display interface-defns) ; DEBUG
      (define hostnum (string-append "0x10000000000000" (string-pad (number->string (+ hostidx 1)) 2 #\0)))      

      (define static-NAT (send config get-static-NAT))
      (define dynamic-NAT (send config get-dynamic-NAT))
      (for-each (lambda (anat) (unless (send anat supported-flowlog)
                                 (error (format "unsupported NAT: ~v: ~v" (send anat name (string->symbol hostname) "") (send anat direction)))))
                (append static-NAT dynamic-NAT))

      (define arouter (router ""))
      (set-router-name! arouter hostname)
      (set-router-self-dpid! arouter (string-append "10000000000000" (string-pad hostnum 2 #\0)))
      (define nat-dpid (string-append "40000000000000" (string-pad hostnum 2 #\0)))
      (set-router-nat-dpid! arouter nat-dpid)
      
      (define iftuples (for/list ([ifdef interface-defns] 
                                  [ifindex (build-list (length interface-defns) values)])                         
                          (ifacedef->tuples arouter interface-defns nat-dpid hostname hostnum ifindex ifdef (+ hostidx 1))))
      
      ; TODO(tn)+TODO(adf): secondary subnets on interfaces with nat?
      
      (define routertuple (vals->routertuples hostname hostnum)) 
      (define natinfo (vals->nat (router-nat-dpid arouter)))
      (define tuples (string-append* (flatten (cons routertuple (cons iftuples natinfo)))))
      ; finally, reverse since subnets are attached in the order they appear in the protobuf
      (set-router-subnets! arouter (reverse (router-subnets arouter)))
      
      (set-routers-routers! routers-msg (cons arouter (routers-routers routers-msg)))
      
      tuples)
  
    (define routers-msg (routers ""))
    (define startupinserts (string-append* (for/list ([config configurations] [hostidx (build-list (length configurations) values)]) 
                                             
                                             (extract-hosts routers-msg config hostidx))))
    ;(pretty-display startupinserts) ; DEBUG
    
    ;(printf "~v~n" (send config get-dynamic-NAT))
    
    
    ; output the router message for this router
    (call-with-output-file (make-path root-path "IOS.pb.bin") #:exists 'replace
      (lambda (out) 
        ;(printf "Outputting protobufs spec for this router...~n") ; DEBUG
        ;(printf "~v~n" routers) ; DEBUG
        (serialize routers-msg out)))
    
    ; TODO: On what? packet(p) won't give the nw fields!
    ; and ippacket won't give the ports. 
    ; Do we need to separate out by TCP/UDP etc?
    
    ; TODO: protocols currently "prot-ICMP" etc.
    ; TODO: different kinds of NAT
    ; TODO: flags? no support in flowlog this iteration.
    
    ; FLOWLOG:

    ; First up, generate StartupConfig

   
    (dict-set! startup-vars "basename" root-path)
    (dict-set! startup-vars "startupinserts" startupinserts)
    
    ; IP rules to the IP block. Will also apply to TCP packets. So don't duplicate!
    (dict-set! startup-vars "inboundacl-tcp" (sexpr-to-flowlog `(or ,@inboundacl-tcp) #t))
    (dict-set! startup-vars "inboundacl-udp" (sexpr-to-flowlog `(or ,@inboundacl-udp) #t))
    (dict-set! startup-vars "inboundacl-ip" (sexpr-to-flowlog `(or ,@inboundacl-ip) #t))
    (dict-set! startup-vars "outboundacl-tcp" (sexpr-to-flowlog `(or ,@outboundacl-tcp) #t))
    (dict-set! startup-vars "outboundacl-udp" (sexpr-to-flowlog `(or ,@outboundacl-udp) #t))
    (dict-set! startup-vars "outboundacl-ip" (sexpr-to-flowlog `(or ,@outboundacl-ip) #t))

    ;(dict-set! startup-vars "insidenat" insidenat)
    ;(dict-set! startup-vars "outsidenat" outsidenat)    
    
    
    (store (render-template "templates/StartupConfig.template.flg" startup-vars)
           (make-path root-path "IOS.flg"))

    ; generate L3external and L3router

    (dict-set! router-vars "basename" root-path)
    (dict-set! router-vars "nexthop-fragment" (sexpr-to-flowlog next-hop-fragment #f))

    (store (render-template "templates/L3external.template.flg" router-vars)
           (make-path root-path "L3external.flg"))

    (store (render-template "templates/L3router.template.flg" router-vars)
           (make-path root-path "L3router.flg"))

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
    (store insidenat (make-path root-path "InsideNAT.p"))
    (store outsidenat (make-path root-path "OutsideNAT.p"))
    (store local-switch (make-path root-path "LocalSwitching.p"))
    (store network-switch (make-path root-path "NetworkSwitching.p"))
    (store static-route (make-path root-path "StaticRoute.p"))
    (store policy-route (make-path root-path "PolicyRoute.p"))
    (store default-policy-route (make-path root-path "DefaultPolicyRoute.p"))
    
    ;(define x (routers "xyz"))
    ;(call-with-input-file "test.out"
    ;  (lambda (out) (deserialize x out)))
    
    ))

;; string string -> path
(define (make-path base file)
  (build-path (string->path base) (string->path file)))

;; any path -> void
(define (store contents path)
  (begin
    (let [(port (open-output-file path #:mode 'text #:exists 'replace))]
      (pretty-display contents port)  ; FLOWLOG changed to pretty-display from pretty-print
      (close-output-port port))))

(require racket/string)
(require (only-in srfi/13 string-pad))

(define (simplify-sexpr sexpr positive scrubdltyp)
  (match sexpr    
    [`(or ,args ...)
     (define newargs (remove-duplicates (filter (lambda (a) (not (equal? a 'false))) (map (lambda (x) (simplify-sexpr x positive scrubdltyp)) args))))
     (cond [(member 'true newargs) 'true]
           [(empty? newargs) 'false]
           [(equal? (length newargs) 1) (first newargs)]
           [else `(or ,@newargs)])]
    [`(and ,args ...) 
     (define newargs (remove-duplicates (filter (lambda (a) (not (equal? a 'true))) (map (lambda (x) (simplify-sexpr x positive scrubdltyp)) args))))
     (cond [(member 'false newargs) 'false]
           [(empty? newargs) 'true]
           [(equal? (length newargs) 1) (first newargs)]
           [else `(and ,@newargs)])]
    [`(not ,arg) 
     (define newarg (simplify-sexpr arg (not positive) scrubdltyp))
     (match newarg 
       [`(not ,arg2) arg2]
       [x `(not ,x)])]
    [`(RULE ,linenum ,decision ,varargs ,pred) 
     (define newpred (simplify-sexpr pred positive scrubdltyp))     
     `(RULE ,linenum ,decision ,varargs ,newpred)]
    ; equality or IN:
    [`(= ,arg1 ,arg2)      
     ; The IOS compiler produces "empty" assertions sometimes. deal with them
     (cond [(or (equal? arg1 'IPAddress)
                (equal? arg2 'IPAddress)
                (equal? arg1 'Port)
                (equal? arg2 'Port))
            'true]
           [else `(= ,(simplify-sexpr arg1 positive scrubdltyp) ,(simplify-sexpr arg2 positive scrubdltyp))])]   

    ; deal with protocol names: expand to dltyp and nwproto fields
    [`(prot-TCP protocol)
     (if (and scrubdltyp positive) 'true `(and (= pkt.dlTyp 0x800) (= pkt.nwProto 0x6)))]
    [`(prot-UDP protocol)
     ; 17 dec, 11 hex
     (if (and scrubdltyp positive) 'true `(and (= pkt.dlTyp 0x800) (= pkt.nwProto 0x11)))]
    [`(prot-IP protocol)
     (if (and scrubdltyp positive) 'true `(= pkt.dlTyp 0x800))]

    [`(,(? symbol? predname) ,args ...)
     `(,predname ,@(map (lambda (x) (simplify-sexpr x positive scrubdltyp)) args))] 
    ; implicit and:
    [(list args ...) (simplify-sexpr `(and ,@args) positive scrubdltyp)]
    [(? string? x) x]
    [(? symbol? x) 
     ; Midway I realized that we could just turn "src-addr-in"
     ; into "pkt.nwSrc" here, rather than bit-by-bit in IOS.ss.     
     ; So some will already be converted, some won't.
     (cond [(equal? x 'src-addr-in) "pkt.nwSrc"]
           [(equal? x 'src-port-in) "pkt.tpSrc"]
           [(equal? x 'dest-addr-in) "pkt.nwDst"]
           [(equal? x 'dest-port-in) "pkt.tpDst"]
           [(equal? x 'src-addr-out) "new.nwSrc"]
           [(equal? x 'src-port-out) "new.tpSrc"]
           [(equal? x 'dest-addr-out) "new.nwDst"]
           [(equal? x 'dest-port-out) "new.tpDst"]           
           [else (symbol->string x)])]    
    [x (pretty-display x) (raise "error with simplify-sexpr")]))

(define debug-include-comments #f)

(define (sexpr-to-flowlog sexpr scrubdltyp)
  (sexpr-to-flowlog-helper (simplify-sexpr sexpr #t scrubdltyp)))

(define (sexpr-to-flowlog-helper simplified)   
  ;(display "sexpr-to-flowlog >>>")
  ;(pretty-display simplified)
  (match simplified    
    ; concatenate strings for each arg, use "OR" as separator    
    [`(or ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog-helper (remove-duplicates args)) " \nOR\n ") " )")]
    [`(and ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog-helper (remove-duplicates args)) " AND ")" )")]
    [`(not ,arg) (string-append "NOT " (sexpr-to-flowlog-helper arg))]
    [`(RULE ,linenum ,decision ,varargs ,pred) (string-append (if debug-include-comments 
                                                                  (string-append "\n// " (symbol->string linenum) "\n")
                                                                  "") 
                                                              (sexpr-to-flowlog-helper pred))]
    ; equality or IN:
    [`(= ,arg1 ,arg2)      
     (define s1 (sexpr-to-flowlog-helper arg1))
     (define s2 (sexpr-to-flowlog-helper arg2))     
     (cond [(regexp-match #rx"^[0-9\\.]+/" s1)
            (string-append s2 " IN " s1)]
           [(regexp-match #rx"^[0-9\\.]+/" s2)
            (string-append s1 " IN " s2)]
           [else (string-append "(" s1 " = " s2 ")")])]    
    ; table reference
    ; (this needs to come after concrete keywords like RULE, and, =, etc.)
    [`(,(? symbol? predname) ,args ...)      
     (string-append (symbol->string predname) "( " (string-join (map sexpr-to-flowlog-helper args) ", ")" )")] 
    [(? string? x) x]
    [(? symbol? x) (symbol->string x)]    
    [x (pretty-display x) (raise "error with sexpr-to-flowlog-helper")]))
