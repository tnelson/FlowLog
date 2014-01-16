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
(require "ios.ss")
(require "ios-parse.ss")

(require (planet murphy/protobuf:1:1))
(require (planet murphy/protobuf/syntax))

;+message Subnet {
; +  optional string addr    = 1;  // required
; +  optional int32  mask    = 2;  // required
; +  optional string gw      = 3;  // required
; +  optional string tr_dpid = 4;  // required
; +}
; +
    
(define-message-type msubnet
  ([required primitive:string addr 1]
   [required primitive:int32 mask 2]
   [required primitive:string gw 3]
   [required primitive:string tr_dpid 4]))


; +message Network {
; +  optional string addr = 1;  // required
; +  optional int32  mask = 2;  // required
; +}    
(define-message-type mnetwork
  ([required primitive:string addr 1]
   [required primitive:int32 mask 2]))


; +
; +message Peer {
; +  optional string ip   = 1;  // required
; +  optional int32  mask = 2;  // required
; +  optional string mac  = 3;  // required
; +
; +  repeated Network networks = 4;
; +}
(define-message-type mpeer
  ([required primitive:string ip 1]
   [required primitive:int32 mask 2]
   [required primitive:string mac 3]
   [repeated mnetwork networks 4]))


 ;+
 ;+message Router {
 ;+  optional string name      = 1;  // required
 ;+  optional string self_dpid = 2;  // required
 ;+  optional string nat_dpid  = 3;  // required
 ;+
 ;+  repeated Subnet subnets = 4;
 ;+  repeated Peer peers = 5;
 ;+}
(define-message-type mrouter
  ([required primitive:string name 1]
   [required primitive:string self_dpid 2]
   [required primitive:string nat_dpid 3]
   [repeated msubnet subnets 4]
   [repeated mpeer peers 5]))
 
 ;+
 ;+message Routers {
 ;+  repeated Router routers = 1;
 ;+}
(define-message-type mrouters
  ([repeated mrouter routers 1]))

(provide compile-configurations)

(define-syntax combine-rules
  (syntax-rules ()
    [(_ configurations accessor)
     (apply append (map (λ (configuration)
                          (send configuration accessor))
                        configurations))]))

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
        ,sec-addr ,sec-netw ))

    ;// subnets(addr,  mask, gw ip,    gw mac,            locSw,            locpt, trSw)
    ;INSERT (10.0.1.0, 24,   10.0.1.1, ca:fe:ca:fe:00:01, 0x1000000000000001, 2, 0x2000000000000001) INTO subnets;
    
    (define (vals->subnet addr nwa nwm rnum inum ptnum)
      (define gwmac (string-append "ca:fe:ca:fe:00:" (string-pad inum 2 #\0)))
      (define trsw (string-append "0x20000000000000" (string-pad inum 2 #\0)))
      (string-append "INSERT (" (string-join (list nwa nwm addr gwmac rnum ptnum trsw) ", ") ") INTO subnets;\n"
                     "INSERT (" (string-join (list addr gwmac) ", ") ") INTO cached; // auto\n"
                     "INSERT (" trsw ") INTO switches_without_mac_learning; // auto\n"))

    (define (vals->ifalias rname iname inum)
      (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"") 
                                                   (string-append "\"" iname "\"") 
                                                   inum) ", ") ") INTO portAlias;\n"))
        
    (define (vals->routeralias rname rnum)
      (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"")                                                    
                                                   rnum) ", ") ") INTO routerAlias;\n"
                     "INSERT (" rnum ") INTO switches_without_mac_learning; // auto\n"))

    (define (vals->nat natnum)
      (string-append "INSERT (0x" natnum ") INTO switches_without_mac_learning; // auto\n"))
    
    ; Need to assign an ID to the router and an ID to the interface
    (define (ifacedef->tuples arouter rname rnum ifindex i)      
      (match i
        [`(,name ,primaddr (,primnwa ,primnwm) ,secaddr (,secnwa ,secnwm)) 
         (define inum (number->string (+ 1 ifindex)))
         ; offset the port number on the router by 1, since 1 is reserved for the attached NAT switch
         (define ptnum (number->string (+ 2 ifindex)))
         (define prim (vals->subnet primaddr primnwa primnwm rnum inum ptnum))
         (define sec (if secaddr (vals->subnet primaddr primnwa primnwm rnum inum ptnum) #f))
         (define alias (vals->ifalias rname name inum))
         (define result (filter (lambda (x) x) (list prim sec alias)))
         
         ; generate protobufs as well
         (define aninterf (msubnet ""))
         ;(set-msubnet-name! aninterf name)         
         ;(set-minterface-id! aninterf ifindex)
         (set-msubnet-tr_dpid! aninterf (string-append "20000000000000" (string-pad (number->string (+ ifindex 1)) 2 #\0)))
         (set-msubnet-addr! aninterf primnwa)         
         (set-msubnet-mask! aninterf (string->number primnwm))
         (set-msubnet-gw! aninterf primaddr)
         
         (set-mrouter-subnets! arouter (cons aninterf (mrouter-subnets arouter) ))
         
         (when secaddr 
           (printf "WARNING! Secondary interface detected. Please confirm that the primary and secondaries get different IDs.~n")
           (define aninterf2 (msubnet ""))
           ;(set-minterface-name! aninterf2 name)         
           ;(set-minterface-id! aninterf2 ifindex)
           (set-msubnet-tr_dpid! aninterf2 ifindex)         
           (set-msubnet-addr! aninterf2 secnwa)         
           (set-msubnet-mask! aninterf2 (string->number secnwm))
           (set-msubnet-gw! aninterf2 secaddr)
           (set-mrouter-subnets! arouter (cons aninterf2 (mrouter-subnets arouter) )))
         
         result]
        [else (pretty-display i) (error "ifacedef->tuple")]))
        
    (define (extract-hosts routers config hostidx)      
      (define hostname (symbol->string (send (send config get-hostname) name)))
      (define interfaces (send config get-interfaces))
      (define interface-keys (hash-keys interfaces))
      (printf "pre-processing hostname: ~v~n" hostname)     
      (define interface-defns (hash-map interfaces extract-ifs))
      (pretty-display interface-defns) 
      (define hostnum (string-append "0x10000000000000" (string-pad (number->string (+ hostidx 1)) 2 #\0)))
      
      
      (define arouter (mrouter ""))
      (set-mrouter-name! arouter hostname)
      (set-mrouter-self_dpid! arouter (string-append "10000000000000" (string-pad hostnum 2 #\0)))
      (set-mrouter-nat_dpid! arouter (string-append "40000000000000" (string-pad hostnum 2 #\0)))
      
      (define iftuples (for/list ([ifdef interface-defns] 
                                    [ifindex (build-list (length interface-defns) values)])
                           (ifacedef->tuples arouter hostname hostnum ifindex ifdef)))
      (define routertuple (vals->routeralias hostname hostnum)) 
      (define natinfo (vals->nat (mrouter-nat_dpid arouter)))
      (define tuples (string-append* (flatten (cons routertuple (cons iftuples natinfo)))))
      ; finally, reverse since subnets are attached in the order they appear in the protobuf
      (set-mrouter-subnets! arouter (reverse (mrouter-subnets arouter)))
      
      (set-mrouters-routers! routers (cons arouter (mrouters-routers routers)))     
      
      tuples)
  
    (define routers (mrouters ""))
    (define startupinserts (string-append* (for/list ([config configurations] [hostidx (build-list (length configurations) values)]) 
                                             (extract-hosts routers config hostidx))))
    (pretty-display startupinserts)
    
    ; output the router message for this router
    (call-with-output-file (make-path root-path "IOS.pb.bin") #:exists 'replace
      (lambda (out) 
        (printf "Outputting protobufs spec for this router...~n")
        (printf "~v~n" routers)
        (serialize routers out)))
    
    ; TODO: On what? packet(p) won't give the nw fields!
    ; and ippacket won't give the ports. 
    ; Do we need to separate out by TCP/UDP etc?
    
    ; TODO: protocols currently "prot-ICMP" etc.
    ; TODO: different kinds of NAT
    ; TODO: flags? no support in flowlog this iteration.
    
    ; FLOWLOG:
    (store (string-append "next hop fragment:\n"
                          (sexpr-to-flowlog next-hop-fragment)
                          "\n\n"
                          "entrance acl:\n "
                          (sexpr-to-flowlog `(or ,@inboundacl))
                          "\n\nexit acl:\n "
                          (sexpr-to-flowlog `(or ,@outboundacl))
                          "\n\n\n"
                          "INCLUDE \"examples/L3router.flg\";\n"
                          "INCLUDE \"examples/Mac_Learning.inc.flg\";\n\n"
                          "TABLE routerAlias(string, switchid);\n"
                          "TABLE portAlias(string, string, portid);\n\n"
                          "ON startup(e):\n"
                          startupinserts "\n"                         
                        )
           (make-path root-path "IOS.flg"))        
    
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
    
    ;(define x (mrouters "xyz"))
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

(define (simplify-sexpr sexpr)
  (match sexpr    
    [`(or ,args ...)
     (define newargs (remove-duplicates (filter (lambda (a) (not (equal? a 'false))) (map simplify-sexpr args))))
     (cond [(member 'true newargs) 'true]
           [(empty? newargs) 'false]
           [(equal? (length newargs) 1) (first newargs)]
           [else `(or ,@newargs)])]
    [`(and ,args ...) 
     (define newargs (remove-duplicates (filter (lambda (a) (not (equal? a 'true))) (map simplify-sexpr args))))
     (cond [(member 'false newargs) 'false]
           [(empty? newargs) 'true]
           [(equal? (length newargs) 1) (first newargs)]
           [else `(and ,@newargs)])]
    [`(not ,arg) 
     (define newarg (simplify-sexpr arg))
     (match newarg 
       [`(not ,arg2) arg2]
       [x `(not ,x)])]
    [`(RULE ,linenum ,decision ,varargs ,pred) 
     (define newpred (simplify-sexpr pred))     
     `(RULE ,linenum ,decision ,varargs ,newpred)]
    ; equality or IN:
    [`(= ,arg1 ,arg2)      
     ; The IOS compiler produces "empty" assertions sometimes. deal with them
     (cond [(or (equal? arg1 'IPAddress)
                (equal? arg2 'IPAddress)
                (equal? arg1 'Port)
                (equal? arg2 'Port))
            'true]
           [else sexpr])]   

    ; deal with protocol names: expand to dltyp and nwproto fields
    [`(prot-TCP protocol)
     `(and (= p.dlTyp 0x800) (= p.nwProto 0x6))]
    [`(prot-UDP protocol)
     `(and (= p.dlTyp 0x800) (= p.nwProto 0x17))]
    [`(prot-IP protocol)
     `(= p.dlTyp 0x800)]

    [`(,(? symbol? predname) ,args ...)
     sexpr] 
    ; implicit and:
    [(list args ...) (simplify-sexpr `(and ,@args))]
    [(? string? x) x]
    [(? symbol? x) 
     ; Midway I realized that we could just turn "src-addr-in"
     ; into "p.nwSrc" here, rather than bit-by-bit in IOS.ss.     
     ; So some will already be converted, some won't.
     (cond [(equal? x 'src-addr-in) "p.nwSrc"]
           [(equal? x 'src-port-in) "p.tpSrc"]
           [(equal? x 'dest-addr-in) "p.nwDst"]
           [(equal? x 'dest-port-in) "p.tpDst"]
           [(equal? x 'src-addr-out) "new.nwSrc"]
           [(equal? x 'src-port-out) "new.tpSrc"]
           [(equal? x 'dest-addr-out) "new.nwDst"]
           [(equal? x 'dest-port-out) "new.tpDst"]           
           [else (symbol->string x)])]    
    [x (pretty-display x) (raise "error with simplify-sexpr")]))

(define (sexpr-to-flowlog sexpr)
  (sexpr-to-flowlog-helper (simplify-sexpr sexpr)))

(define (sexpr-to-flowlog-helper simplified)   
  ;(display "sexpr-to-flowlog >>>")
  ;(pretty-display simplified)
  (match simplified    
    ; concatenate strings for each arg, use "OR" as separator    
    [`(or ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " \nOR\n ") " )")]
    [`(and ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " AND ")" )")]
    [`(not ,arg) (string-append "NOT " (sexpr-to-flowlog arg))]
    [`(RULE ,linenum ,decision ,varargs ,pred) (string-append "\n// " (symbol->string linenum) "\n" (sexpr-to-flowlog pred))]
    ; equality or IN:
    [`(= ,arg1 ,arg2)      
     (define s1 (sexpr-to-flowlog arg1))
     (define s2 (sexpr-to-flowlog arg2))
     (if (regexp-match #rx"^[0-9\\.]+/" s1)
         (string-append s2 " IN " s1)
         (string-append "(" s1 " = " s2 ")"))]    
    ; table reference
    ; (this needs to come after concrete keywords like RULE, and, =, etc.)
    [`(,(? symbol? predname) ,args ...)      
     (string-append (symbol->string predname) "( " (string-join (map sexpr-to-flowlog args) ", ")" )")] 
    [(? string? x) x]
    [(? symbol? x) (symbol->string x)]    
    [x (pretty-display x) (raise "error with sexpr-to-flowlog")]))




    
   #|(define flattened-forward
      `(and 
        ; Pass ACLs (in and out)
        (or ,@inboundacl) 
        (or ,@outboundacl)
        ; NAT
        (or (and (or ,@insidenat) 
                 (internalNATPort p.locSw p.locPt))
            (and (or ,@outsidenat) 
                 (externalNATPort p.locSw p.locPt)))
        ; routing, switching
        (or ,@localswitching-forward
            (and ,@localswitching-pass                         
                 (or ,@policyroute-forward
                     (and ,@policyroute-route
                          ,@networkswitching-forward)
                     (and ,@policyroute-pass
                          (or ,@staticroute-forward
                              (and ,@staticroute-route
                                   ,@networkswitching-forward)                              
                              (and ,@staticroute-pass
                                   (or ,@defaultpolicyroute-forward
                                       (and ,@defaultpolicyroute-route
                                            ,@networkswitching-forward)
                                       ; Final option: Packet is dropped.
                                       ; No final option here in actual program.
                                       ; If no satisfying new, then nothing to do.
                                       )))))))))
    |#