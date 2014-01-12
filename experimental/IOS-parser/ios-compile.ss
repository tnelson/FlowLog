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
    
    (define inboundacl (policy 'InboundACL (combine-rules configurations inbound-ACL-rules)))
    (define outboundacl (policy 'OutboundACL (combine-rules configurations outbound-ACL-rules)))
    
    (define inside-NAT (policy 'InsideNAT (combine-rules configurations inside-NAT-rules)))
    (define outside-NAT (policy 'InsideNAT (combine-rules configurations outside-NAT-rules)))
    
    (define local-switch (policy 'LocalSwitching (combine-rules configurations local-switching-rules)))
    (define localswitching-forward (filter (lambda (r) (equal? 'forward (third r))) local-switch))
    (define localswitching-pass (filter (lambda (r) (equal? 'pass (third r))) local-switch))
    
    (define network-switch (policy 'NetworkSwitching (combine-rules configurations network-switching-rules)))
    (define networkswitching-forward (filter (lambda (r) (equal? 'forward (third r))) network-switch))
    (define networkswitching-pass (filter (lambda (r) (equal? 'pass (third r))) network-switch))
    
    (define static-route (policy 'StaticRouting (combine-rules configurations static-route-rules)))
    (define staticroute-forward (filter (lambda (r) (equal? 'forward (third r))) static-route))
    (define staticroute-route (filter (lambda (r) (equal? 'route (third r))) static-route))
    (define staticroute-pass (filter (lambda (r) (equal? 'pass (third r))) static-route))
    
    
    
    (define policy-route (policy 'PolicyRouting (combine-rules configurations policy-routing-rules)))
    (define policyroute-forward (filter (lambda (r) (equal? 'forward (third r))) policy-route))
    (define policyroute-route (filter (lambda (r) (equal? 'route (third r))) policy-route))
    (define policyroute-pass (filter (lambda (r) (equal? 'pass (third r))) policy-route))
    
    (define default-policy-route (policy 'DefaultPolicyRouting (combine-rules configurations default-policy-routing-rules)))           
    (define defaultpolicyroute-forward (filter (lambda (r) (equal? 'forward (third r))) default-policy-route))
    (define defaultpolicyroute-route (filter (lambda (r) (equal? 'route (third r))) default-policy-route))
    (define defaultpolicyroute-pass (filter (lambda (r) (equal? 'pass (third r))) default-policy-route))    
      
      ;; 
      ; *** TODO *** concern: need to bake in first-applicable between forward-route-pass?
      ;; ^^^ TODO!
      
      ; FLOWLOG: cross-policy flattening will happen here
        ; FLOWLOG: decorrelation, etc. should happen within the policy function
                
   
        ; from margrave IOS
#|        (or (and (= next-hop dest-addr_) 
                 ([,localswitching forward] ahostname dest-addr_ exit))
            (and ([,localswitching pass] ahostname dest-addr_ exit))
            
            (or ([,policyroute forward] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                (and ([,policyroute route] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                     ([,networkswitching forward] ahostname next-hop exit))
                (and ([,policyroute pass] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                     (or ([,staticroute forward] ahostname dest-addr_ next-hop exit)
                         (and ([,staticroute route] ahostname dest-addr_ next-hop exit)
                              ([,networkswitching forward] ahostname next-hop exit))
                         (and ([,staticroute pass] ahostname dest-addr_ next-hop exit)
                              (or ([,defaultpolicyroute forward] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                  (and ([,defaultpolicyroute route] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                       ([,networkswitching forward] ahostname next-hop exit))
                                  ; Final option: Packet is dropped.
                                  (and ([,defaultpolicyroute pass] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                       (= next-hop dest-addr_)
                                       (interf-drop exit)))))))))))))))
      |#  
        
    ; need to map fourth over all of these
        (define flattened-policies
          `(and 
            ; Pass ACLs (in and out)
            (or ,@inboundacl) 
            (or ,@outboundacl)
            ; NAT
            ; TODO
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

        ;(display flattened-policies)
    
    ; TODO: On what? packet(p) won't give the nw fields!
    ; and ippacket won't give the ports. 
    ; Do we need to separate out by TCP/UDP etc?
    
        ; FLOWLOG:
        (store (string-append "ON ip_packet(p): DO forward(new) WHERE \n" (sexpr-to-flowlog flattened-policies))
               (make-path root-path "IOS.flg"))        
        
        ; For debugging purposes:
        (store inboundacl (make-path root-path "InboundACL.p"))
        (store outboundacl (make-path root-path "OutboundACL.p"))
        (store inside-NAT (make-path root-path "InsideNAT.p"))
        (store outside-NAT (make-path root-path "OutsideNAT.p"))
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

(define (sexpr-to-flowlog sexpr) 
  (display "sexpr-to-flowlog >>>")
  (pretty-display sexpr)
  (match sexpr    
    ; concatenate strings for each arg, use "OR" as separator
    [`(or ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " \nOR\n ") " )")]
    [`(and ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " AND ")" )")]
    [`(not ,arg) (string-append "NOT " (sexpr-to-flowlog arg))]
    [`(RULE ,linenum ,decision ,varargs ,pred) (string-append "\n// " (symbol->string linenum) "\n" (sexpr-to-flowlog pred))]
    [`(= ,arg1 ,arg2)      
     (define s1 (sexpr-to-flowlog arg1))
     (define s2 (sexpr-to-flowlog arg2))
     (if (regexp-match #rx"^[0-9\\.]+/" s1)
         (string-append s2 " IN " s1)
         (string-append "(" s1 " = " s2 ")"))]
    ; This needs to come after RULE and =
    [`(,(? symbol? predname) ,args ...)      
     (string-append (symbol->string predname) "( " (string-join (map sexpr-to-flowlog args) ", ")" )")]    
    [(list args ...) (sexpr-to-flowlog `(and ,@args))]    
    [(? string? x) x]
    [(? symbol? x) (symbol->string x)]    
    [x (pretty-display x) (raise "error with sexpr-to-flowlog")]))

