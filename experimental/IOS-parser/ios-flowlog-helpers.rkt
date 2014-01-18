; Helpers for IOS -> Flowlog conversion
#lang racket

(require racket/string)
(require (only-in srfi/13 string-pad))
(require "ios.ss")

(provide (all-defined-out))

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
  
  (unless (equal? (symbol->string ifaceid) name) (error "extract-ifs"))
  `(,name 
    ,prim-addr ,prim-netw
    ,sec-addr ,sec-netw ,nat-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to produce startup insert tuples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tr-dpid ridx inum ox)
  (string-append (if ox "0x" "") "2" (string-pad (number->string ridx) 2 #\0) "00000000000" (string-pad inum 2 #\0)))         

(define (vals->subnet addr nwa nwm rnum inum ptnum trsw ridx)
  (define gwmac (string-append "ca:fe:00:" (string-pad (number->string ridx) 2 #\0) ":00:" (string-pad inum 2 #\0)))
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

(define (vals->nat nat-dpid rnum)
  (string-append "INSERT (0x" nat-dpid ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (" rnum ", 0x" nat-dpid ") INTO router_nat;\n"))

(define (vals->ifacldefn hostaclnum ridx iidx rname iname)           
  (string-append "INSERT (0x" hostaclnum ") INTO aclDPID;\n"
                 "INSERT (0x" hostaclnum ") INTO switches_without_mac_learning; // auto\n"
                 "INSERT (\"" (symbol->string (build-acl-name rname iname)) "\", 0x" hostaclnum ") INTO routerAlias;\n"))

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
         
