; Helpers for IOS -> Flowlog conversion
#lang racket

(require racket/string)
(require (only-in srfi/13 string-pad string-contains))
(require "ios.ss")

(provide (all-defined-out) 
         (struct-out ifacedef))
; must provide struct export separately
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to convert sexprs to Flowlog text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    [`(prot-TCP ,protocolvar)
     (if (and scrubdltyp positive) 'true `(and (= pkt.dlTyp 0x800) (= pkt.nwProto 0x6)))]
    [`(prot-UDP ,protocolvar)
     ; 17 dec, 11 hex
     (if (and scrubdltyp positive) 'true `(and (= pkt.dlTyp 0x800) (= pkt.nwProto 0x11)))]
    [`(prot-IP ,protocolvar)
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

(define (reflexive-rule-to-flowlog sexpr)
  (match (simplify-sexpr sexpr #t #t)
    [`(RULE ,linenum insert ,reflexname ,pred)
     (string-append (if debug-include-comments 
                        (string-append "\n// " (symbol->string linenum) "\n")
                        "") 
                    " INSERT (" (symbol->string reflexname) 
                    ", pkt.nwSrc, pkt.tpSrc, pkt.nwProto, pkt.nwDst, pkt.tpDst) INTO reflexiveACL WHERE \n"                                                           
                    (sexpr-to-flowlog-helper pred)
                    ; don't make all packets go to ctrler
                    "AND not reflexiveACL(" (symbol->string reflexname) ", pkt.nwSrc, pkt.tpSrc, pkt.nwProto, pkt.nwDst, pkt.tpDst)"
                    ";\n")]))
  

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Accept an interface ID and an interface%, produce a tuple with interface properties
; (Meant to be called with map-hash, hence the double-parameter)

; struct contains some cleanup vs. the interface% class
(define sorf? (or/c false? string?))
(define-struct/contract ifacedef 
  ([name string?]
   [prim-addr sorf?]
   ; "The number of elements in the list must match the number of arguments supplied to list/c"
   [prim-netw (list/c sorf? sorf?)] ; addr mask
   [sec-addr sorf?]
   [sec-netw (list/c sorf? sorf?) ] ; addr mask
   [nat-side symbol?]
   [switchport-mode symbol?]
   ; Listof is variable length
   [switchport-vlans (listof string?)] ; vlan ids as strings
   [ospf-cost (or/c symbol? string?)])) ; cost as string

(define (->string x)
  (cond [(number? x) (number->string x)]
        [(symbol? x) (symbol->string x)]
        [else x]))

(define (extract-ifs ifaceid iface)
  (define name (symbol->string (send iface text)))
  (define prim-addr-obj (send iface get-primary-address))
  (define sec-addr-obj (send iface get-secondary-address))
  (define prim-netw-obj (send iface get-primary-network))
  (define sec-netw-obj (send iface get-secondary-network))
  (define nat-side (send iface get-nat-side))
    
  (define switchport-mode (get-field switchport-mode iface))
  (define switchport-vlans (map ->string (get-field switchport-vlans iface)))
  (define ospf-cost (->string (get-field ospf-cost iface)))
    
  (cond [(not (equal? switchport-mode 'no))
         (ifacedef name #f '(#f #f) #f '(#f #f) nat-side switchport-mode switchport-vlans ospf-cost)]
        [(not prim-addr-obj) 
         (printf "extract-ifs IGNORING: ~v; had neither a primary address nor a switchport mode~n" name)
         #f]
        [else    
  
         (define prim-addr (send prim-addr-obj text-address))
         (define sec-addr (if sec-addr-obj  
                              (send sec-addr-obj text-address) 
                              #f))
         (define prim-netw `(,(send prim-netw-obj text-address),(send prim-netw-obj text-mask)))
         (define sec-netw (if sec-netw-obj 
                              `(,(send sec-netw-obj text-address),(send sec-netw-obj text-mask))
                              (list #f #f)))
         
         (unless (equal? (string-uncapitalize (symbol->string ifaceid)) name)
           (error (format "extract-ifs: ~v vs. ~v" (symbol->string ifaceid) name)))
         
         (ifacedef name prim-addr prim-netw sec-addr sec-netw nat-side switchport-mode switchport-vlans ospf-cost)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to produce startup insert tuples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vals->subnet addr nwa nwm rnum inum ptnum ridx)
  (cond [(not addr) ""] 
        [else
         (define gwmac (string-append "ca:fe:00:" (string-pad (number->string ridx) 2 #\0) ":00:" (string-pad inum 2 #\0)))
         (string-append "INSERT (" (string-join (list nwa nwm addr gwmac rnum ptnum) ", ") ") INTO subnets;\n"
                        "INSERT (" (string-join (list addr gwmac) ", ") ") INTO cached; // auto\n")]))

(define (vals->ifalias rname iname inum)
  ;(string-append "INSERT (" (string-join (list (string-append "\"" rname "\"") 
  ;                                             (string-append "\"" iname "\"") 
  ;                                             inum) ", ") ") INTO portAlias;\n")
  (format "// Interface ~v~n" iname))

(define (vals->routertuples rname rnum)
  (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"")                                                    
                                               rnum) ", ") ") INTO routerAlias;\n"
                                                           "INSERT (" rnum ") INTO switches_without_mac_learning; // auto\n"))

(define (vals->nat nat-dpid rnum)
  (string-append "INSERT (0x" nat-dpid ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (0x" nat-dpid ") INTO switches_without_arp; // auto\n"
                 "INSERT (" rnum ", 0x" nat-dpid ") INTO router_nat;\n"))

(define (vals->tr tr-dpid rnum)
  (string-append "INSERT (0x" tr-dpid ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (0x" tr-dpid ") INTO switches_without_arp; // auto\n"
                 "INSERT (" rnum ", 0x" tr-dpid ") INTO router_tr;\n"))

(define (vals->acl acl-dpid rnum)
  (string-append "INSERT (0x" acl-dpid ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (0x" acl-dpid ") INTO switches_without_arp; // auto\n"
                 "INSERT (" rnum ", 0x" acl-dpid ") INTO router_acl;\n"))

(define (vals->vlan vlan-dpid rnum)
  (string-append "INSERT (0x" vlan-dpid ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (0x" vlan-dpid ") INTO switches_without_arp; // auto\n"
                 "INSERT (" rnum ", 0x" vlan-dpid ") INTO router_vlan;\n"))

(define (val->ospf rnum ptnum cost)  
  (if (or (not cost) (equal? cost "no"))
      empty
      (list (format "INSERT (~a,~a,~a) INTO ospf_costs;~n" rnum ptnum cost))))
(define (val->spmode rnum ptnum mode)
  ;(printf "spmode: ~a ~a ~a~n" rnum ptnum mode)
  (if (equal? mode 'no)
      empty
      (list (format "INSERT (~a,~a,\"~a\") INTO sp_modes;~n" rnum ptnum mode))))
(define (vals->vlans rnum ptnum vlanlist)  
  (string-append* (map (lambda (vlan) (format "INSERT (~a,~a,~a) INTO sp_vlans;~n" rnum ptnum vlan)) vlanlist)))
(define (val->p2r rnum ppt rpt)    
  (list (format "INSERT (~a,~a,~a) INTO p2r;~n" rnum ppt rpt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Switched Virtual Interfaces
; "vlan" interfaces used by vlans to cross L3 boundries
(define (is-virtual-interface? name)
  (string-contains (string-downcase name) "vlan"))
 
(define (vals->vlan-iface rnum ptnum name)  
  (cond [(is-virtual-interface? name)         
         (format "INSERT (~a,~a,~a) INTO virtual_interfaces;~n" rnum ptnum (first (string-split (string-downcase name) "vlan")))]
        [else ""]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; index is the 0-indexed count of this interface (i.e., index + 2 is the port on the "router")
; so the host of the ACL table will be: 2 * index + 1   (as OpenFlow ports are 1-indexed)
; and the router side of the ACL table will be: 2 * index + 2
(define (vals->ifacldefn acl-dpid rpt rname iname switchport-mode)  
  (define alias-str (symbol->string (build-acl-name rname iname)))
  ; compute host-side and router-side port ids (both "routing" port number context)
  ; rpt starts with *2* already; need to subtract 2 to get the base
  (define hside-pt (number->string (+ (* 2 (- rpt 2)) 1)))
  (define rside-pt (number->string (+ (* 2 (- rpt 2)) 2)))
  (if (not (equal? switchport-mode 'no))
    ""
    (string-append "INSERT (\"" alias-str "\", 0x" acl-dpid ", " hside-pt ", " rside-pt ") INTO aclAlias;\n")))

; For this particular type of nat (source list overload dynamic):
; For each private interface (nat = inside), and each public interface (nat = outside)
; insert a row into natconfig

; To get at the "global" NAT lines, use: 
;(define static-NAT (send config get-static-NAT)) 
;(define dynamic-NAT (send config get-dynamic-NAT))

(define (if-pair->natconfig interface-defns nat-side nat-dpid)
  (if (equal? nat-side 'inside)
      (for/list ([ifdef2 interface-defns])      
        (define primaddr2 (ifacedef-prim-addr ifdef2))          
        (if (equal? (ifacedef-nat-side ifdef2) 'outside)
            (string-append "INSERT (0x" nat-dpid "," "1" "," "1" "," primaddr2 ") INTO natconfig;\n"
                           "INSERT (" primaddr2 ", 0x6, 10000) INTO seqpt; // auto \n"
                           "INSERT (" primaddr2 ", 0x11, 10000) INTO seqpt; // auto \n")
            ""))
      empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Build needs_nat.
; This function has side effects: modifies the router-vars.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ifvals->needs-nat nn-for-router rnum rname nwa nwm)
  (cond [nwa
         (dict-set! nn-for-router rnum (cons `(and (= pkt.nwSrc ,(string-append nwa "/" nwm))
                                                   (not (= pkt.nwDst ,(string-append nwa "/" nwm))))
                                             (dict-ref nn-for-router rnum)))
         (string-append "INSERT (" rnum ", " nwa ", " nwm ") INTO needs_nat;\n")]
        [else ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Construct template formula for a specific router
; "nn" is an artifact of former use of this func for just "needs-nat"
; IMPORTANT: this assumes current location is at x-router, not x-nat (hence the use of rnum)
(define (build-per-router-fmla-for-router rnum nnlst)
  `(and (or (= pkt.locSw ,rnum) (router_tr ,rnum pkt.locSw)) 
        (or ,@nnlst)))

; Given a hash mapping router to lst for use with build-per-fouter-fmla..., construct fmla
(define (build-per-router-fmla-from-hash nn-for-router)
  (define fmla `(or ,@(hash-map nn-for-router build-per-router-fmla-for-router))) 
  (sexpr-to-flowlog fmla #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;