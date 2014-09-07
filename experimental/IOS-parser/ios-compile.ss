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
                          ; (printf "Processing: ~v~n" (send (send configuration get-hostname) text)) ; DEBUG
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
(define (compile-configurations root-path filenames default-ACL-permit connfilename)
  
  ; Let user errors through; catch all other errors and give a "friendly" error message.
  (with-handlers ([(lambda (e) (and #f (exn:fail? e) (not (exn:fail:user? e))))
                   (lambda (e) (raise-user-error (format "Unrecoverable error parsing IOS configurations. Please report this error to the Margrave maintainers. The internal error was: ~a.~n" e)))])
    (define configurations (map (λ (filename)                                
                                  (printf "Parsing: ~a~n" filename)
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
    
   ;(define local-switch (policy '(forward pass) (combine-rules configurations local-switching-rules)))
   ; (define localswitching-forward (assoc2 'forward local-switch))
   ; (define localswitching-pass (assoc2 'pass local-switch))
    
   ; (define network-switch (policy '(forward pass) (combine-rules configurations network-switching-rules)))
   ; (define networkswitching-forward (assoc2 'forward network-switch))
   ; (define networkswitching-pass (assoc2 'pass network-switch))
    
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

    (define statics (combine-rules configurations get-static-routes))    
    
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
    
    ;(printf "next hop fragment: ~v~n" next-hop-fragment) ; debug
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

    (define warned-secondary (box #f)) 
    (define last-physicalpt-used (box 0))
    (define last-routingpt-used (box 1))
    (define vlan-iname-to-physicalport (box empty))
    
    ;;;;;;;;;;;;;;;;;;;
    ; Need to assign an ID to the router and an ID to the interface
    ; (we are in the scope of compile-configurations)
    (define (ifacedef->tuples arouter interface-defns nat-dpid rname rnum ifindex i ridx acl-dpid vlan-dpid)
      (define name (ifacedef-name i))
      (define primaddr (ifacedef-prim-addr i))
      (match-define (list primnwa primnwm) (ifacedef-prim-netw i))      
      (define secaddr (ifacedef-sec-addr i))
      (match-define (list secnwa secnwm) (ifacedef-sec-netw i))
      (define nat-side (ifacedef-nat-side i))
      (define switchport-mode (ifacedef-switchport-mode i))
      (define switchport-vlans (ifacedef-switchport-vlans i))
      (define ospf-cost (ifacedef-ospf-cost i))
             
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ; port IDs and NIC MAC addresses
         ; - All interfaces (virtual, switchport, and L3 physical) have a MAC address
         ; - There are two separate spaces of port IDs:
         ;   + physical (L3 physical and switchport): these are used by the VLAN sub-router; represent physical connections to subnets 
         ;   + routing (L3 physical and virtual): used by the internal sub-routers to handle traffic completing a L2 hop and being L3-routed.         
         
         ; used for mac address; assign in sequence regardless of interface type
         (define macnum (number->string ifindex))
                          
         ; use appropriate type. this means that we no longer have a single number that
         ; can be followed through the pipeline, but we never had that anyway (rtr used pt1 for nat)
         ; recall that in the router-table, port 1 is reserved for the attached NAT switch (hence starting with 1 in the box above)
         (define physical-ptnum (cond [(is-virtual-interface? name) "0"] 
                                      [else 
                                       (define v (+ 1 (unbox last-physicalpt-used)))
                                       (set-box! last-physicalpt-used v)
                                       (number->string v)]))
         
         (define routing-ptnum (cond [(equal? switchport-mode 'no) 
                                      (define v (+ 1 (unbox last-routingpt-used)))
                                      (set-box! last-routingpt-used v )
                                      (number->string v)] 
                                     [else "0"]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         (printf "if name=~v; ridx=~v; rnum=~v; ifindex=~v; rname=~v; cost=~v mode=~v vlans=~v ppt=~v rpt=~v~n"
                 name ridx rnum ifindex rname ospf-cost switchport-mode switchport-vlans physical-ptnum routing-ptnum) ; DEBUG
      
         (define switchport-mode-inserts (val->spmode vlan-dpid physical-ptnum switchport-mode))
         (define switchport-vlan-inserts (vals->vlans vlan-dpid physical-ptnum switchport-vlans))                          
         (define maybe-vlan-interface-inserts (vals->vlan-iface vlan-dpid routing-ptnum name))
         
         ;;;;;;;;;;;;;;;;         
         ; Produce tuples
         ; TODO: if secondary, need to increment tr_dpid
         (define prim (vals->subnet primaddr primnwa primnwm rnum macnum routing-ptnum ridx))
         ;(define sec (if secaddr (vals->subnet secaddr secnwa secnwm rnum macnum ptnum ridx) #f))
         (define sec "")
         (define alias (vals->ifalias rname name routing-ptnum)) 

         ; local subnets (if any)
         (when primnwa
           (dict-set! dst-local-subnet-for-router rnum 
                      (cons `(= pkt.nwDst ,(string-append primnwa "/" primnwm))
                            (dict-ref dst-local-subnet-for-router rnum))))
         ;(when sec
         ;  (dict-set! dst-local-subnet-for-router rnum
         ;             (cons `(= pkt.nwDst ,(string-append secnwa "/" secnwm))
         ;                   (dict-ref dst-local-subnet-for-router rnum))))

         ; needs nat?
         (define needs-nat (if (and nat-side (equal? nat-side 'inside))
                               (string-append (ifvals->needs-nat nn-for-router rnum rname primnwa primnwm) 
                                              (ifvals->needs-nat nn-for-router rnum rname secnwa secnwm))
                               empty))                 
         
         (define acldefn (vals->ifacldefn acl-dpid (string->number routing-ptnum) rname name switchport-mode))
         (define natconfigs (if-pair->natconfig interface-defns nat-side nat-dpid))                                    
         ;;;;;;;;;;;;;;;;;

         ;(printf "dst local: ~v~n~v~n" dst-local-subnet-for-router) ; DEBUG

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ; generate data for protobufs "subnet" messages         
         ; a virtual interface needs to know which physical port it is attached to
                 
         (define aninterf (subnet ""))
         
         ; This is a virtual interface. Extract the vlan it connects to and find the physical port id
         ; ASSUMPTION: switchport interfaces have all already been processed and assigned a physical port.
         ; string list-of(tuple) -> string
         (define (get-physical-portnums name interface-defns)
           (define vlanid (first (string-split (string-downcase name) "vlan")))
           (filter-map (lambda (pr) (if (equal? (first pr) vlanid) (second pr) #f)) (unbox vlan-iname-to-physicalport)))
         
      ; Remember ifacename -> vlans connected (we need to tell mininet which physical ports for each subnet)
      (case (ifacedef-switchport-mode i)
        [(access trunk)
         (for-each (lambda (vlanid)                  
                 (set-box! vlan-iname-to-physicalport (cons (list vlanid (string->number physical-ptnum)) (unbox vlan-iname-to-physicalport)))) 
               (ifacedef-switchport-vlans i))] 
        [else void])
                  
         ; this interface declares a subnet (may be virtual, but won't be a switchport)
         (when primnwa
           (set-subnet-addr! aninterf primnwa)
           (set-subnet-mask! aninterf (string->number primnwm))
           (set-subnet-gw! aninterf primaddr)           
                  
           ; for debugging. only records the L3 interface name (possibly virtual)   
           (set-subnet-ifname! aninterf name) 
           
           (if (is-virtual-interface? name)
               (set-subnet-physical-portid! aninterf (get-physical-portnums name interface-defns))
               (set-subnet-physical-portid! aninterf (list (string->number physical-ptnum))))
           
           (set-router-subnets! arouter (cons aninterf (router-subnets arouter) )))
      
         (unless (equal? physical-ptnum "0")
           (define aport (port ""))
           (set-port-id! aport (string->number physical-ptnum))
           (set-port-name! aport name)
           (unless (equal? switchport-mode 'no)
             (set-port-vlan-type! aport (symbol->string switchport-mode)))
           (set-router-ports! arouter (cons aport (router-ports arouter))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         ; Deal with secondary subnet, if there is one
         (when secaddr
           (unless (unbox warned-secondary)
             (printf "WARNING! Secondary interface(s) detected on ~v. Ignoring...~n" rname)
             (set-box! warned-secondary #t))
           )
         ;;;;;;;;;;;;;;;;;

      ; WARNING: routing-ptnum is a separate counter from physical-ptnum, used for producing ACL alias, etc.
      ; Relations used by the VLAN sub-router need to start their routing ports from (#physicals)+1

      (define adjusted-routing-ptnum (->string (+ (string->number routing-ptnum) (- (unbox last-physicalpt-used) 1))))
      (printf "Processing port. Physical: ~a. Get-phys: ~a. Routing: ~a. Incr routing by # physical: ~a~n" 
              physical-ptnum
              (get-physical-portnums name interface-defns)
              routing-ptnum
              adjusted-routing-ptnum)
      
      (define ospf-cost-inserts (val->ospf rnum routing-ptnum ospf-cost))           
      (define static-nexthops-inserts (val->statics rnum routing-ptnum statics))
           
      (define p2r (val->p2r vlan-dpid
                            (cond [(equal? physical-ptnum "0")                                   
                                   (get-physical-portnums name interface-defns)]
                                  [else                                 
                                   (list physical-ptnum)])
                            routing-ptnum
                            adjusted-routing-ptnum))
      
      ; Finally, return the result tuples (protobuf changes are side-effects)
      ; Keep the tuples that are non-#f         
      
      
      (filter (lambda (x) x) (list "\n" alias p2r prim sec needs-nat acldefn natconfigs
                                   switchport-mode-inserts switchport-vlan-inserts ospf-cost-inserts
                                   maybe-vlan-interface-inserts static-nexthops-inserts)))
 
    (define total-parsed-ace-count (box 0))      
    (define total-used-ace-count (box 0))

    
    ;;;;;;;;;;;;;;;;;;;
    (define (extract-hosts routers-msg config hostidx)      
      (define hostname (symbol->string (send (send config get-hostname) name)))
      ; Please keep and comment out. Useful to know immediately which config is failing:
      (printf "~nprocessing host config: ~v~n" hostname) ; DEBUG      
      
      (define interfaces (send config get-non-shutdown-interfaces)) ; IGNORE "shutdown" interfaces                     
      (printf "#interfaces: ~v #non-shutdown: ~v~n" (hash-count (send config get-interfaces)) (hash-count interfaces))
      
      ; Filter out interfaces that extract-ifs returns #f for. (for instance, ifs with no subnet)      
      (define unfiltered-interface-defns (hash-map interfaces extract-ifs))
      (define interface-defns (filter (lambda (i) i) unfiltered-interface-defns))
      (printf "Processed ~v interfaces; ~v had subnets and will be handled.~n" (length unfiltered-interface-defns) (length interface-defns)) ; DEBUG     
      
      ; reset per router
      (set-box! last-physicalpt-used 0)
      (set-box! last-routingpt-used 1)
      (set-box! vlan-iname-to-physicalport empty)

      
      ;;;;;;;;;;;;;;;;;;;;
      ;; to be moved to helper module
      ; provide extra information
      (define acls (send config get-ACLs))
      (define acl-ids-used (foldl (lambda (iface acc)  
                                    (define inid (get-field inbound-ACL-ID iface))
                                    (define outid (get-field outbound-ACL-ID iface))
                                    (define ifname (send iface text))                                    
                                    ;(printf "acl ~v and ~v used on interface ~v~n" inid outid ifname)
                                    (cond [(and (not (equal? 'default inid)) (not (equal? 'default outid))) (cons inid (cons outid acc))]
                                          [(not (equal? 'default inid))(cons inid acc)]
                                          [(not (equal? 'default outid)) (cons outid acc)]
                                          [else acc])) empty (hash-values interfaces)))
  
      ; start at -1 because the 'default ACE is always created
      (define quick-parsed-ace-count (box -1))
      ; but count 'default if used
      (define quick-used-ace-count (box 0))

      ; iterate by ACL, not interface (no risk of double-count)
      (hash-map acls (lambda (key acl)
                      ; (printf "acl ~v had: ~v~n" key (send acl get-ACEs))
                       (set-box! quick-parsed-ace-count (+ (length (send acl get-ACEs)) (unbox quick-parsed-ace-count)))
                       (when (member key acl-ids-used)
                         (set-box! quick-used-ace-count (+ (length (send acl get-ACEs)) (unbox quick-used-ace-count))))))
      
      (printf "#acl entries: ~v. #acl entries used for filtering on an interface: ~v~n" (unbox quick-parsed-ace-count) (unbox quick-used-ace-count))
      (define acl-ids-used-nodupes (remove-duplicates acl-ids-used))
      (printf "There were ~v ACLs defined. ~v (~v) were used on interfaces (either inbound or outbound) ~v times in total.~n" 
              (hash-count acls)
              (length acl-ids-used-nodupes)
              acl-ids-used-nodupes
              (length acl-ids-used))
           
      (set-box! total-used-ace-count (+ (unbox quick-used-ace-count) (unbox total-used-ace-count)))
      (set-box! total-parsed-ace-count (+ (unbox quick-parsed-ace-count) (unbox total-parsed-ace-count)))      
      ;; ***TODO*** to understand rule counts, need to know how many times each acl is applied.
      
      (define (get-acl-counts id)
        (define aces (send (hash-ref acls id) get-ACEs))
        (for/fold ([overlaps 0] [permits 0] [denies 0]) ([ace aces])
          (define dec (send ace decision))
          (cond [(equal? 'permit dec) (values (+ overlaps denies) (+ permits 1) denies)]
                [(equal? 'deny dec) (values overlaps permits (+ denies 1))]
                [else (error (format "unexpected decision ~v" dec))])))
      
      (for-each (lambda (id) 
                  (define-values (overlaps permits denies) (get-acl-counts id))
                  (printf "id=~v had ~v overlaps, ~v permits, ~v denies. (ICMP will be ignored.)~n" id overlaps permits denies)) acl-ids-used-nodupes)
      (printf "[Do not forget that interfaces without ACLs, and directions without ACLs, will produce flowlog rules...]~n")
      ;;;;;;;;;;;;;;;;;;;;
      
      (maybe-update-max-subnets (length interface-defns))
      ;(pretty-display interface-defns) ; DEBUG
      (define hostnum (string-append "0x1000000000" (string-pad (number->string (+ hostidx 1)) 2 #\0)))
      (define self-dpid (string-append "1000000000" (string-pad hostnum 2 #\0))) ; TODO(adf): cleanup
      (define nat-dpid  (string-append "4000000000" (string-pad hostnum 2 #\0)))
      (define tr-dpid   (string-append "2000000000" (string-pad hostnum 2 #\0)))
      (define acl-dpid  (string-append "5000000000" (string-pad hostnum 2 #\0)))
      (define vlan-dpid (string-append "6000000000" (string-pad hostnum 2 #\0)))
      ; Prepare this list of needs-nat expressions
      (dict-set! nn-for-router hostnum empty)
      (dict-set! dst-local-subnet-for-router hostnum empty)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Confirm that no unsupported NAT variety appears
      (define static-NAT (send config get-static-NAT)) 
      (define dynamic-NAT (send config get-dynamic-NAT))
      (for-each (lambda (anat) 
                  (unless (send anat supported-flowlog)
                    (error (format "unsupported NAT: ~v: ~v" (send anat name (string->symbol hostname) "") (send anat direction))))
                  )                
                (append static-NAT dynamic-NAT))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define arouter (router ""))
      (set-router-name! arouter hostname)
      (set-router-self-dpid! arouter self-dpid)
      (set-router-nat-dpid! arouter nat-dpid)
      (set-router-tr-dpid! arouter tr-dpid)
      (set-router-acl-dpid! arouter acl-dpid)
      (set-router-vlan-dpid! arouter vlan-dpid)      
      
      (define (is-switchport-interface i)           
        (not (equal? (ifacedef-switchport-mode i)'no)))
              
      ; Do switchport interfaces first
      (define iftuples (for/list ([ifdef (sort interface-defns (lambda (i1 i2)
                                                                 (or (is-switchport-interface i1)
                                                                     (not (is-switchport-interface i2)))))] 
                                  [ifindex (build-list (length interface-defns) values)])                         
                          (ifacedef->tuples arouter interface-defns nat-dpid hostname hostnum (+ ifindex 1) ifdef (+ hostidx 1) acl-dpid vlan-dpid)))
      (define routertuple (vals->routertuples hostname hostnum)) 
      (define natinfo (vals->nat (router-nat-dpid arouter) hostnum))      
      (define trinfo (vals->tr (router-tr-dpid arouter) hostnum))
      (define aclinfo (vals->acl (router-acl-dpid arouter) hostnum))
      (define vlaninfo (vals->vlan vlan-dpid hostnum))
      
      ; TODO(tn)+TODO(adf): secondary subnets on interfaces with nat?      
      
      ; finally, reverse since subnets are attached in the order they appear in the protobuf
      (set-router-subnets! arouter (reverse (router-subnets arouter)))      
      (set-routers-routers! routers-msg (cons arouter (routers-routers routers-msg)))

      ; report to mininet how many physical ports this router has
      (set-router-num-physical! arouter (unbox last-physicalpt-used))
      
      ; Return the gathered tuples. protobufs changes are side-effects
      (string-append* (flatten (list (format "~n// For router ~a (id = ~a)~n" hostname hostnum)   
                                     (format "// ACL = ~a; TR = ~a; NAT = ~a~n" (router-acl-dpid arouter) (router-tr-dpid arouter) (router-nat-dpid arouter))
                                     iftuples
                                     "\n"
                                     vlaninfo
                                     aclinfo
                                     trinfo
                                     natinfo
                                     routertuple
                                     "\n"))))
                                     ;(cons routertuple "\n" (cons iftuples (cons natinfo (cons trinfo aclinfo)))))))
  
    (define routers-msg (routers ""))
    (set-routers-subnet-base-dpid! routers-msg "300000000000")
    (define startupinserts (string-append* (for/list ([config configurations] [hostidx (build-list (length configurations) values)]) 
                                             
                                             (extract-hosts routers-msg config hostidx))))
            
    
    (printf "total parsed ACL elements: ~v. total used ACL elements: ~v~n" (unbox total-parsed-ace-count) (unbox total-used-ace-count))
        
    ; prepare any fixed connection info for mininet
    (set-routers-connections! routers-msg (handle-connections-file root-path connfilename))
    
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
    
    ; For embedding policy in FL rule
    (dict-set! router-vars "policyroute-route" (sexpr-to-flowlog policyroute-route #f))
    (dict-set! router-vars "policyroute-pass" (sexpr-to-flowlog policyroute-pass #f))
    
    (dict-set! router-vars "needs-nat-disj" (build-per-router-fmla-from-hash nn-for-router))         
    (dict-set! router-vars "dst-local-subnet" (build-per-router-fmla-from-hash dst-local-subnet-for-router))

    (store (render-template "templates/L3external.template.flg" router-vars)
           (make-path root-path "L3external.flg"))

    (store (render-template "templates/L3router.template.flg" router-vars)
           (make-path root-path "L3router.flg"))

    
    (store (render-template "templates/NIB.template.flg" router-vars)
           (make-path root-path "NIB.flg"))
    
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

    (store (render-template "templates/Vlans.template.flg" router-vars)
           (make-path root-path "Vlans.flg"))
    
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
    (copy-with-basename "NIB")

    (printf "Template values:~n~n")
    (printf "~ndst-local-subnet: ~a~n" (dict-ref router-vars "dst-local-subnet"))    
    (printf "~nneeds-nat-disj: ~a~n" (dict-ref router-vars "needs-nat-disj"))
    (printf "~nnext-hop-fragment: ~a~n" (dict-ref router-vars "nexthop-fragment"))
    (printf "~nnext-hop-fragment-for-tr: ~a~n" (dict-ref router-vars "nexthop-fragment-for-tr"))
    (printf "~npolicyroute-route: ~a~n" (dict-ref router-vars "policyroute-route"))
    (printf "~npolicyroute-pass: ~a~n" (dict-ref router-vars "policyroute-pass"))
    
    ; For debugging purposes:
    (store inboundacl (make-path root-path "InboundACL.p"))
    (store outboundacl (make-path root-path "OutboundACL.p"))
    (store reflexive-inserts (make-path root-path "ReflexiveDebug.p"))
    (store insidenat (make-path root-path "InsideNAT.p"))
    (store outsidenat (make-path root-path "OutsideNAT.p"))
    ;(store local-switch (make-path root-path "LocalSwitching.p"))
    ;(store network-switch (make-path root-path "NetworkSwitching.p"))
    (store static-route (make-path root-path "StaticRoute.p"))
    (store policy-route (make-path root-path "PolicyRoute.p"))
    (store default-policy-route (make-path root-path "DefaultPolicyRoute.p"))))

;; string string -> path
(define (make-path base file)
  (build-path (string->path base) (string->path file)))

(define (handle-connections-file root-path connfilename)
  (when connfilename ; #f if none provided
    (define lines (file->lines (build-path root-path connfilename)))
    (for/list ([line lines])
      (define pieces (string-split line))      
      (match-define (list r1 p1 r2 p2) pieces)
      (define c (connection ""))
      (set-connection-router1! c r1)
      (set-connection-router2! c r2)
      (set-connection-iface1! c p1)
      (set-connection-iface2! c p2)
      c)))

;; any path -> void
(define (store contents path)
  (begin
    (let [(port (open-output-file path #:mode 'text #:exists 'replace))]
      (pretty-display contents port)  ; FLOWLOG changed to pretty-display from pretty-print
      (close-output-port port))))
