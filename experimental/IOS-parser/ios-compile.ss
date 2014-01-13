#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco IOS Policy Compilation
;; Copyright (C) 2009-2010 Christopher Barratt & Brown University
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
;    (pretty-display next-hop-fragment)
    
    (define (extract-ifdata config)      
      (define hostname (send config get-hostname))
      (define interfaces (send config get-interfaces))
      (pretty-display hostname)
      (pretty-display interfaces)
      )
    
    (map extract-ifdata configurations)
    
    ; TODO: On what? packet(p) won't give the nw fields!
    ; and ippacket won't give the ports. 
    ; Do we need to separate out by TCP/UDP etc?
    
    ; TODO: protocols currently "prot-ICMP" etc.
    ; TODO: different kinds of NAT
    ; TODO: flags? no support in flowlog this iteration.
    
    ; FLOWLOG:
    (store (string-append "NEXT-HOP FRAGMENT: \n\n"
                          (sexpr-to-flowlog next-hop-fragment)
                          ";\n\n"
                          
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
  (display "sexpr-to-flowlog >>>")
  (pretty-display simplified)
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

