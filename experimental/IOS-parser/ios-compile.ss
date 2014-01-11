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
    (let* [(configurations (map (λ (filename)                                
                                  (parse-IOS (open-input-file (make-path root-path filename)
                                                              #:mode
                                                              'text)
                                             default-ACL-permit))
                                filenames))
           (inbound-ACL (combine-rules configurations inbound-ACL-rules))
           (outbound-ACL (combine-rules configurations outbound-ACL-rules))
           (inside-NAT (combine-rules configurations inside-NAT-rules))
           (outside-NAT (combine-rules configurations outside-NAT-rules))
           (local-switch (combine-rules configurations local-switching-rules))
           (network-switch (combine-rules configurations network-switching-rules))
           (static-route (combine-rules configurations static-route-rules))
           (policy-route (combine-rules configurations policy-routing-rules))
           (default-policy-route (combine-rules configurations default-policy-routing-rules))           
           ]
      (begin
        
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
        
        (define (flatten-policies inboundacl outboundacl)          
          `(and 
            ; Pass ACLs (in and out)
            (or ,@inboundacl) 
            (or ,@outboundacl)
            ; NAT
            ; TODO
            ; routing, switching
            (or ([,localswitching forward] ahostname dest-addr_ exit)
                (and ([,localswitching pass] ahostname dest-addr_ exit)                          
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
                                           ; No final option here in actual program.
                                           ; If no satisfying new, then nothing to do.
                                           )))))))))

        
        
        ; FLOWLOG:
        (store (sexpr-to-flowlog (flatten-policies (policy 'InboundACL inbound-ACL) 
                                                              (policy 'OutboundACL outbound-ACL)))
                          (make-path root-path "IOS.flg"))        
        
        ; For debugging purposes:
        (store (policy 'InboundACL inbound-ACL) (make-path root-path "InboundACL.p"))
        (store (policy 'OutboundACL outbound-ACL) (make-path root-path "OutboundACL.p"))
        (store (policy 'InsideNAT inside-NAT) (make-path root-path "InsideNAT.p"))
        (store (policy 'OutsideNAT outside-NAT) (make-path root-path "OutsideNAT.p"))
        (store (policy 'LocalSwitching local-switch) (make-path root-path "LocalSwitching.p"))
        (store (policy 'NetworkSwitching network-switch) (make-path root-path "NetworkSwitching.p"))
        (store (policy 'StaticRoute static-route) (make-path root-path "StaticRoute.p"))
        (store (policy 'PolicyRoute policy-route) (make-path root-path "PolicyRoute.p"))
        (store (policy 'DefaultPolicyRoute default-policy-route) (make-path root-path "DefaultPolicyRoute.p"))
        
        ))))

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
  (display ">>>")
  (pretty-display sexpr)
  (match sexpr
    ; concatenate strings for each arg, use "OR" as separator
    [`(or ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " \nOR\n ") " )")]
    [`(and ,args ...) (string-append "( " (string-join (map sexpr-to-flowlog (remove-duplicates args)) " AND ")" )")]
    [`(not ,arg) (string-append "NOT " (sexpr-to-flowlog arg))]
    [`(,(? symbol? predname) ,args ...) 
     (string-append (symbol->string predname) "( " (string-join (map sexpr-to-flowlog args) " AND ")" )")]
    [`(= ,arg1 ,arg2)      
     (define s1 (sexpr-to-flowlog arg1))
     (define s2 (sexpr-to-flowlog arg2))
     (if (regexp-match #rx"^[0-9\\.]+/" s1)
         (string-append s2 " IN " s1)
         (string-append "(" s1 " = " s2 ")"))]
    
    [(list linenum decision varargs pred) (string-append "// " (symbol->string linenum) "\n" (sexpr-to-flowlog pred))]
    [(? string? x) x]
    [(? symbol? x) (symbol->string x)]    
    [x (pretty-display x) (raise "error with sexpr-to-flowlog")]))

