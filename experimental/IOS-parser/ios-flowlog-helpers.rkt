; Helpers for IOS -> Flowlog conversion
#lang racket

(require racket/string)
(require (only-in srfi/13 string-pad))
(require "ios.ss")

(provide (all-defined-out))

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
    [`(RULE ,linenum insert ,varargs ,pred) (string-append (if debug-include-comments 
                                                                  (string-append "\n// " (symbol->string linenum) "\n")
                                                                  "") 
                                                           " INSERT (pkt.nwSrc, pkt.tpSrc, pkt.nwProto, pkt.nwDst, pkt.tpDst) INTO reflexiveACL WHERE \n"                                                           
                                                              (sexpr-to-flowlog-helper pred)
                                                              "\n;")]
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
  
  (unless (equal? (string-uncapitalize (symbol->string ifaceid)) name)
    (error (format "extract-ifs: ~v vs. ~v" (symbol->string ifaceid) name)))
  `(,name 
    ,prim-addr ,prim-netw
    ,sec-addr ,sec-netw ,nat-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to produce startup insert tuples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vals->subnet addr nwa nwm rnum inum ptnum ridx)
  (define gwmac (string-append "ca:fe:00:" (string-pad (number->string ridx) 2 #\0) ":00:" (string-pad inum 2 #\0)))
  (string-append "INSERT (" (string-join (list nwa nwm addr gwmac rnum ptnum) ", ") ") INTO subnets;\n"
                 "INSERT (" (string-join (list addr gwmac) ", ") ") INTO cached; // auto\n"))

(define (vals->ifalias rname iname inum)
  (string-append "INSERT (" (string-join (list (string-append "\"" rname "\"") 
                                               (string-append "\"" iname "\"") 
                                               inum) ", ") ") INTO portAlias;\n"))

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

; index is the 0-indexed count of this interface (i.e., index + 2 is the port on the "router")
; so the host of the ACL table will be: 2 * index + 1   (as OpenFlow ports are 1-indexed)
; and the router side of the ACL table will be: 2 * index + 2
(define (vals->ifacldefn acl-dpid index rname iname)
  (define alias-str (symbol->string (build-acl-name rname iname)))
  (define hside-pt (number->string (+ (* 2 index) 1)))
  (define rside-pt (number->string (+ (* 2 index) 2)))
  (string-append "INSERT (\"" alias-str "\", 0x" acl-dpid ", " hside-pt ", " rside-pt ") INTO aclAlias;\n"))

; For this particular type of nat (source list overload dynamic):
; For each private interface (nat = inside), and each public interface (nat = outside)
; insert a row into natconfig
;; TODO(tn)+TODO(adf): more kinds of NAT? To get at the "global" NAT lines, use: 
;(define static-NAT (send config get-static-NAT)) 
;(define dynamic-NAT (send config get-dynamic-NAT))

(define (if-pair->natconfig interface-defns nat-side nat-dpid)
  (if (equal? nat-side 'inside)
      (for/list ([ifdef2 interface-defns]
                 [ifindex2 (build-list (length interface-defns) values)])                 
        (match ifdef2 
          [`(,name2 ,primaddr2 (,primnwa2 ,primnwm2) ,secaddr2 (,secnwa2 ,secnwm2) ,nat-side2)                    
           (if (equal? nat-side2 'outside)
               (string-append "INSERT (0x" nat-dpid "," "1" "," "1" "," primaddr2 ") INTO natconfig;\n"
                              "INSERT (" primaddr2 ", 0x6, 10000) INTO seqpt; // auto \n"
                              "INSERT (" primaddr2 ", 0x11, 10000) INTO seqpt; // auto \n")
               "")]))
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
; Construct the needs-nat template formula per router
; IMPORTANT: this assumes current location is at x-router, not x-nat (hence the use of rnum)
(define (build-per-router-fmla-for-router rnum nnlst)
  `(and (= pkt.locSw ,rnum) (or ,@nnlst)))
(define (build-per-router-fmla-from-hash nn-for-router)
  (define fmla `(or ,@(hash-map nn-for-router build-per-router-fmla-for-router))) 
  (sexpr-to-flowlog fmla #f))
