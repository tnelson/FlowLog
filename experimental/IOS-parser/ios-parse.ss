;#lang scheme/base
#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco IOS Configuration Parsing
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

; DEBUG NOTE(tn);
; (hash-map (send y get-ACLs) (lambda (id acl) `(,id ,(map (lambda (ace) (send ace pretty-print)) (send acl get-ACEs)))))

(require scheme/list)
(require scheme/class)
(require srfi/13)
(require "ios.ss")

(provide parse-IOS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define disable-command-warnings #t)
(define disable-keyword-warnings #t)
(define disable-restart-warnings #f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; port boolean -> IOS-config%
(define (parse-IOS input default-ACL-permit)
  (port-count-lines! input)
  (parse-IOS-details input (make-empty-IOS-config default-ACL-permit)))

;; port IOS-config% -> IOS-config%
(define (parse-IOS-details input config)
  ; read a line. each case below re-calls this func for the next line
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (parse-IOS-details-for-line input config line-tokens)))

; This function is called with a line already read (in contrast to parse-IOS-details).
; If a block function (e.g., parse-ip-named-access-list) is expecting something other than what it sees, 
; it will pass the current line to this function, allowing us to support blocks that aren't separated by !.
(define (parse-IOS-details-for-line input config line-tokens)    
    (case (first line-tokens)
      [(access-list) (parse-IOS-details input (parse-access-list (rest line-tokens) input config))]
      [(crypto) (parse-IOS-details input (parse-crypto (rest line-tokens) input config))]
      [(end) config]
      [(hostname) (parse-IOS-details input (parse-hostname (rest line-tokens) input config))]
      [(interface) (parse-IOS-details input (parse-interface (rest line-tokens) input config))]
      [(ip) (parse-IOS-details input (parse-ip (rest line-tokens) input config))]
      [(route-map) (parse-IOS-details input (parse-route-map (rest line-tokens) input config))]      
      [(router) (parse-IOS-details input (parse-router (rest line-tokens) input config))]
      
      ; Comments
      [(!) (parse-IOS-details input config)] ; comment, move on to the next line
      [(||) (parse-IOS-details input config)] ; blank line, move on
      
      ; Warning only, not a fatal error
      [else (unless disable-command-warnings
              (printf "WARNING: Ignoring unsupported command: ~a. Moving on...~n" (first line-tokens)))
            (parse-IOS-details input config)]))

;; string -> (listof any)
(define (tokenize-line line)
  ; If the line is blank, pretend there is a comment character there. 
  ; This will terminate the current block, rather than crashing the parser.
  ; TODO: If someone has a comment or blank line mid-block, this code will fail, possibly silently.
  (cond [(equal? line "") 
         '(!)]
        ; End of file: assume end keyword is present
        [(equal? line eof) 
         '(end)]
        [else (map (lambda (token)
                     (if (regexp-match #px"^\\d+$" token)
                         (string->number token)
                         (string->symbol token)))
                   (regexp-split #rx" +" (string-trim line)))]))

;; symbol -> boolean
(define (single-address? sym)
  (regexp-match #px"\\d+\\.\\d+\\.\\d+\\.\\d+" (symbol->string sym)))

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-hostname line-tokens input config)
  (send config
        set-hostname
        (make-object hostname% (if (symbol? (first line-tokens))
                                   (first line-tokens)
                                   (string->symbol (number->string (first line-tokens)))))))

;; port -> number
; The line has already been read, so subtract 1. (else, off by one)
(define (line-number input)
  (let-values [([line column position] [port-next-location input])]
    (- line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Do not want to error out if we see an unsupported command. Instead, ignore the line and warn the user.

(define (warning-unsupported line token allowed-tokens)
  (unless disable-keyword-warnings
    (printf "WARNING: Ignoring unexpected keyword on line ~a. The parser expected keywords among: ~a. Got ~a. Moving to next line...~n" line allowed-tokens token)))

(define (warning-unsupported/single-address line token)
  (unless disable-keyword-warnings
    (printf "WARNING: Ignoring unexpected keyword on line ~a. The parser expected a single address. Got ~a. Moving to next line...~n" line token)))

(define (warning-unsupported/if-not no-condition line token allowed-tokens)
  (unless disable-keyword-warnings 
    (printf "WARNING: Ignoring unexpected keyword on line ~a. Did not have ~a, so expected keywords among: ~a. Got ~a. Moving to next line...~n" line no-condition allowed-tokens token)))

(define (warning-unsupported/ending-block line token)
  ; Strongly suggest that we leave this type of warning enabled.
  (unless disable-restart-warnings
    (printf "WARNING: Unexpected top-level keyword: ~a on line ~a. Terminating block and moving to next line...~n" token line )))

(define (warning-unsupported/tcp line not-tcp-protocol)
  (unless disable-keyword-warnings
    (printf "WARNING: Line ~a used a command that expected protocol tcp. Got ~a.~n" line not-tcp-protocol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access Control Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-access-list line-tokens input config)
  ;  (printf "In parse-access-list: ~a ~a ~a~n" line-tokens input config)
  (parse-named-access-list (line-number input) (first line-tokens) (rest line-tokens) config))

;; number symbol (listof any) IOS-config% -> IOS-config%
(define (parse-named-access-list line name line-tokens config)
  ;  (printf "In parse-named-access-list: ~a ~a ~a ~a~n" line name line-tokens config)
  (case (first line-tokens)
    [(dynamic) (parse-named-dynamic-access-list line name (rest line-tokens) config)]
    [(permit deny) (parse-named-dispositive-access-list line
                                                        name
                                                        (first line-tokens)
                                                        (rest line-tokens)
                                                        config)]
    [else (warning-unsupported line (first line-tokens) '(dynamic permit deny))
          config]))

;; number symbol (listof any) IOS-config% -> IOS-config%
(define (parse-named-dynamic-access-list line name line-tokens config)
  (case (second line-tokens)
    [(timeout) (parse-named-dispositive-access-list line
                                                    name
                                                    (fourth line-tokens)
                                                    (drop line-tokens 4)
                                                    config)]
    [(permit deny) (parse-named-dispositive-access-list name
                                                        line
                                                        (second line-tokens)
                                                        (drop line-tokens 2)
                                                        config)]
    [else (warning-unsupported line (first line-tokens) '(timeout permit deny))
          config]))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-named-dispositive-access-list line name disposition line-tokens config)
  ; (printf "In parse-named-dispositive-access-list: ~a ~a ~a ~a ~a ~n" line name disposition line-tokens config)
  (cond [(single-address? (first line-tokens))
         (parse-standard-access-list line name disposition (first line-tokens) (rest line-tokens) config)]
        [else (case (first line-tokens)
                [(any) (parse-access-list-any line name disposition (rest line-tokens) config)]
                [(host) (parse-access-list-host line name disposition (rest line-tokens) config)]
                [(icmp) (parse-access-list-ICMP line name disposition (rest line-tokens) config)]
                [(tcp udp) (parse-access-list-TCP/UDP line
                                                      name
                                                      disposition
                                                      (first line-tokens)
                                                      (rest line-tokens)
                                                      config)]
                [(ip) (parse-access-list-IP line name disposition (rest line-tokens) config)]
                [else (warning-unsupported line (first line-tokens) '(any host icmp tcp udp ip))
                      config])]))

;; number symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-standard-access-list line name disposition src-addr line-tokens config)
  (cond [(empty? line-tokens)
         (send config
               insert-ACE
               name
               (make-object standard-ACE%
                 line
                 (eqv? disposition 'permit)
                 (make-object host-address% src-addr)))]
        [(single-address? (first line-tokens))
         (send config
               insert-ACE
               name
               (make-object standard-ACE%
                 line
                 (eqv? disposition 'permit)
                 (make-object network-address% src-addr (first line-tokens) #t)))]
        [else (send config
                    insert-ACE
                    name
                    (make-object standard-ACE%
                      line
                      (eqv? disposition 'permit)
                      (make-object host-address% src-addr)))]))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-any line name disposition line-tokens config)
  (send config
        insert-ACE
        name
        (make-object standard-ACE%
          line
          (eqv? disposition 'permit)
          (make-object network-address% '0.0.0.0 '255.255.255.255 #t))))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-host line name disposition line-tokens config)
  (send config
        insert-ACE
        name
        (make-object standard-ACE%
          line
          (eqv? disposition 'permit)
          (make-object host-address% (first line-tokens)))))

;; number symbol symbol (listof any) IOS-config% -> ios-config
(define (parse-access-list-ICMP line name disposition line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-ICMP2 line
                                    name
                                    disposition
                                    (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                    (rest line-tokens)
                                    config)]
    [(host) (parse-access-list-ICMP2 line
                                     name
                                     disposition
                                     (make-object host-address% (second line-tokens))
                                     (drop line-tokens 2)
                                     config)]
    [else (parse-access-list-ICMP2 line
                                   name
                                   disposition
                                   (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                   (drop line-tokens 2)
                                   config)]))

;; number symbol symbol address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-ICMP2 line name disposition src-addr line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-ICMP3 line
                                    name
                                    disposition
                                    src-addr
                                    (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                    (rest line-tokens)
                                    config)]
    [(host) (parse-access-list-ICMP3 line
                                     name
                                     disposition
                                     src-addr
                                     (make-object host-address% (second line-tokens))
                                     (drop line-tokens 2)
                                     config)]
    [else (parse-access-list-ICMP3 line
                                   name
                                   disposition
                                   src-addr
                                   (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                   (drop line-tokens 2)
                                   config)]))

;; number symbol symbol address<%> address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-ICMP3 line name disposition src-addr dest-addr line-tokens config)
  (send config
        insert-ACE
        name
        (make-object extended-ACE-ICMP%
          line
          (eqv? disposition 'permit)
          src-addr
          (if (empty? line-tokens)
              'ICMPMessage
              (string->symbol (string-append "icmp-" (symbol->string (first line-tokens)))))
          dest-addr)))

;; number symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-TCP/UDP line name disposition prot line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-TCP/UDP2 line
                                       name
                                       disposition
                                       prot
                                       (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                       (rest line-tokens)
                                       config)]
    [(host) (parse-access-list-TCP/UDP2 line
                                        name
                                        disposition
                                        prot
                                        (make-object host-address% (second line-tokens))
                                        (drop line-tokens 2)
                                        config)]
    [else (parse-access-list-TCP/UDP2 line
                                      name
                                      disposition
                                      prot
                                      (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                      (drop line-tokens 2)
                                      config)]))

;; number symbol symbol symbol address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-TCP/UDP2 line name disposition prot src-addr line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-TCP/UDP4 line
                                       name
                                       disposition
                                       prot
                                       src-addr
                                       (make-object port-range% 0 65535)
                                       (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                       (rest line-tokens)
                                       config)]
    [(host) (parse-access-list-TCP/UDP4 line
                                        name
                                        disposition
                                        prot
                                        src-addr
                                        (make-object port-range% 0 65535)
                                        (make-object host-address% (second line-tokens))
                                        (drop line-tokens 2)
                                        config)]
    [(eq) (parse-access-list-TCP/UDP3 line
                                      name
                                      disposition
                                      prot
                                      src-addr
                                      (make-object port% (second line-tokens))
                                      (drop line-tokens 2)
                                      config)]
    [else (cond [(single-address? (first line-tokens))
                 (parse-access-list-TCP/UDP4 line
                                             name
                                             disposition
                                             prot
                                             src-addr
                                             (make-object port-range% 0 65535)
                                             (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                             (drop line-tokens 2)
                                             config)]
                [else (warning-unsupported/single-address line (first line-tokens))
                      config])]))

;; number symbol symbol symbol address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-TCP/UDP3 line name disposition prot src-addr src-port line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-TCP/UDP4 line
                                       name
                                       disposition
                                       prot
                                       src-addr
                                       src-port
                                       (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                       (rest line-tokens)
                                       config)]
    [(host) (parse-access-list-TCP/UDP4 line
                                        name
                                        disposition
                                        prot
                                        src-addr
                                        src-port
                                        (make-object host-address% (second line-tokens))
                                        (drop line-tokens 2)
                                        config)]
    [else (cond [(single-address? (first line-tokens))
                 (parse-access-list-TCP/UDP4 line
                                             name
                                             disposition
                                             prot
                                             src-addr
                                             src-port
                                             (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                             (drop line-tokens 2)
                                             config)]
                [(warning-unsupported/single-address line (first line-tokens))
                 config])]))

;; number symbol symbol symbol address<%> port<%> address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-TCP/UDP4 line name disposition prot src-addr src-port dest-addr line-tokens config)
  (if (not (empty? line-tokens))
      (case (first line-tokens)
        [(eq) (parse-access-list-TCP/UDP5 line
                                          name
                                          disposition
                                          prot
                                          src-addr
                                          src-port
                                          dest-addr
                                          (make-object port% (second line-tokens))
                                          (drop line-tokens 2)
                                          config)]
        ; Approximate gt by treating it as eq:
        [(gt) (parse-access-list-TCP/UDP5 line
                                          name
                                          disposition
                                          prot
                                          src-addr
                                          src-port
                                          dest-addr
                                          (make-object port% (second line-tokens))
                                          (drop line-tokens 2)
                                          config)]
        [else (warning-unsupported/if-not "end of line" line (first line-tokens) '(eq))
              config])
      (parse-access-list-TCP/UDP5 line
                                  name
                                  disposition
                                  prot
                                  src-addr
                                  src-port
                                  dest-addr
                                  (make-object port-range% 0 65535)
                                  line-tokens
                                  config)))

;; number symbol symbol symbol address<%> port<%> address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-TCP/UDP5 line name disposition prot src-addr src-port dest-addr dest-port line-tokens config)
  (send config
        insert-ACE
        name
        (make-object extended-ACE-TCP/UDP%
          line
          (eqv? disposition 'permit)
          src-addr
          (string->symbol (string-append "prot-" (string-upcase (symbol->string prot))))
          src-port
          dest-addr
          dest-port)))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-IP line name disposition line-tokens config)
  ;(printf "In parse-access-list-IP: ~a ~a ~a ~a ~a ~n" line name disposition line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-IP2 line
                                  name
                                  disposition
                                  (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                  (rest line-tokens)
                                  config)]
    [(host) (parse-access-list-IP2 line
                                   name
                                   disposition
                                   (make-object host-address% (second line-tokens))
                                   (drop line-tokens 2)
                                   config)]
    [else (parse-access-list-IP2 line
                                 name
                                 disposition
                                 (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                 (drop line-tokens 2)
                                 config)]))

;; number symbol symbol address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-IP2 line name disposition src-addr line-tokens config)
  ;(printf "In parse-access-list-IP2: ~a ~a ~a ~a ~a ~a ~n" line name disposition src-addr line-tokens config)
  (case (first line-tokens)
    [(any) (parse-access-list-IP3 line
                                  name
                                  disposition
                                  src-addr
                                  (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                  (rest line-tokens)
                                  config)]
    [(host) (parse-access-list-IP3 line
                                   name
                                   disposition
                                   src-addr
                                   (make-object host-address% (second line-tokens))
                                   (drop line-tokens 2)
                                   config)]
    [else (parse-access-list-IP3 line
                                 name
                                 disposition
                                 src-addr
                                 (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                 (drop line-tokens 2)
                                 config)]))

;; number symbol symbol address<%> address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-access-list-IP3 line name disposition src-addr dest-addr line-tokens config)
  ;(printf "In parse-access-list-IP3: ~a ~a ~a ~a ~a ~a ~a ~n" line name disposition src-addr dest-addr line-tokens config)
  ; -TN: TODO: Display error if "ip" access-list contains a port restriction.
  (send config
        insert-ACE
        name
        (make-object extended-ACE-IP%
          line
          (eqv? disposition 'permit)
          src-addr
          dest-addr)))

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-ip-access-list line-tokens input config)
  (case (first line-tokens)
    [(standard extended) (parse-ip-named-access-list (second line-tokens) input config)]
    [else (warning-unsupported (line-number input) (first line-tokens) '(standard extended))
          config]))

;; symbol port IOS-config% -> IOS-config%
(define (parse-ip-named-access-list name input config)
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (case (first line-tokens)
      [(permit deny) (parse-ip-named-access-list name
                                                 input
                                                 (parse-ip-named-access-list2 (line-number input)
                                                                              name
                                                                              (first line-tokens)
                                                                              (rest line-tokens)
                                                                              config))]
      [(evaluate) (parse-ip-named-access-list name
                                              input
                                              (parse-ip-named-access-list-evaluate (line-number input)
                                                                                   name
                                                                                   (second line-tokens)
                                                                                   (drop line-tokens 2)
                                                                                   config))]
      
      [(!) config]
      [(access-list crypto end hostname interface ip route-map router)
       (warning-unsupported/ending-block (line-number input) (first line-tokens))
       (parse-IOS-details-for-line input config line-tokens)]
      [else 
       (warning-unsupported (line-number input) (first line-tokens) '(permit deny evaluate))
       (parse-ip-named-access-list name input config)])))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list2 line name disposition line-tokens config)
  (cond [(single-address? (first line-tokens))
         (send config
               insert-ACE
               name
               (make-object standard-ACE%
                 line
                 (eqv? disposition 'permit)
                 (make-object network-address% (first line-tokens) (second line-tokens) #t)))]
        [else (case (first line-tokens)
                [(ip tcp udp) (parse-ip-named-access-list3 line
                                                           name
                                                           disposition
                                                           (first line-tokens)
                                                           (rest line-tokens)
                                                           config)]
                [(host) (send config
                              insert-ACE
                              name
                              (make-object standard-ACE%
                                line
                                (eqv? disposition 'permit)
                                (make-object host-address% (second line-tokens))))]
                [(any) (send config
                             insert-ACE
                             name
                             (make-object standard-ACE%
                               line
                               (eqv? disposition 'permit)
                               (make-object network-address% '0.0.0.0 '255.255.255.255 #t)))]
                [else (warning-unsupported/if-not "single address" line (first line-tokens) '(ip tcp udp host any))
                      config])]))

;; number symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list3 line name disposition protocol line-tokens config)
  (cond [(single-address? (first line-tokens))
         (parse-ip-named-access-list4 line
                                      name
                                      disposition
                                      protocol
                                      (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                      (drop line-tokens 2)
                                      config)]
        [else (case (first line-tokens)
                [(host) (parse-ip-named-access-list4 line
                                                     name
                                                     disposition
                                                     protocol
                                                     (make-object host-address% (second line-tokens))
                                                     (drop line-tokens 2)
                                                     config)]
                [(any) (parse-ip-named-access-list4 line
                                                    name
                                                    disposition
                                                    protocol
                                                    (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                                    (rest line-tokens)
                                                    config)]
                [else (warning-unsupported/if-not "single address" line (first line-tokens) '(any host))
                      config])]))

;; number symbl symbol symbol address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list4 line name disposition protocol src-addr line-tokens config)
  (cond [(single-address? (first line-tokens))
         (if (eqv? protocol 'ip)
             (send config
                   insert-ACE
                   name
                   (make-object extended-ACE-IP%
                     line
                     (eqv? disposition 'permit)
                     src-addr
                     (make-object network-address% (first line-tokens) (second line-tokens) #t)))
         (parse-ip-named-access-list5 line
                                      name
                                      disposition
                                      protocol
                                      src-addr
                                      (make-object port-range% 0 65535)
                                      (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                      (drop line-tokens 2)
                                      config))]
        [else (case (first line-tokens)
                [(eq) (parse-ip-named-access-list4A line
                                                    name
                                                    disposition
                                                    protocol
                                                    src-addr
                                                    (make-object port% (second line-tokens))
                                                    (drop line-tokens 2)
                                                    config)]
                [(host) (if (eqv? protocol 'ip)
                           (send config
                                 insert-ACE
                                 name
                                 (make-object extended-ACE-IP%
                                   line
                                   (eqv? disposition 'permit)
                                   src-addr
                                   (make-object host-address% (second line-tokens))))
                            (parse-ip-named-access-list5 line
                                                         name
                                                         disposition
                                                         protocol
                                                         src-addr
                                                         (make-object port-range% 0 65535)
                                                         (make-object host-address% (second line-tokens))
                                                         (drop line-tokens 2)
                                                         config))]
                [(any) (if (eqv? protocol 'ip)
                           (send config
                                 insert-ACE
                                 name
                                 (make-object extended-ACE-IP%
                                   line
                                   (eqv? disposition 'permit)
                                   src-addr
                                   (make-object network-address% '0.0.0.0 '255.255.255.255 #t)))
                           (parse-ip-named-access-list5 line
                                                        name
                                                        disposition
                                                        protocol
                                                        src-addr
                                                        (make-object port-range% 0 65535)
                                                        (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                                        (rest line-tokens)
                                                        config))]
                [else (warning-unsupported/if-not "single address" line (first line-tokens) '(any host eq))
                      config])]))

;; number symbol symbol symbol address<%> port<%> (listof symbol) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list4A line name disposition protocol src-addr src-port line-tokens config)
  (cond [(single-address? (first line-tokens))
         (parse-ip-named-access-list5 line
                                      name
                                      disposition
                                      protocol
                                      src-addr
                                      src-port
                                      (make-object network-address% (first line-tokens) (second line-tokens) #t)
                                      (drop line-tokens 2)
                                      config)]
        [else (case (first line-tokens)
                [(host) (parse-ip-named-access-list5 line
                                                     name
                                                     disposition
                                                     protocol
                                                     src-addr
                                                     src-port
                                                     (make-object host-address% (second line-tokens))
                                                     (drop line-tokens 2)
                                                     config)]
                [(any) (parse-ip-named-access-list5 line
                                                    name
                                                    disposition
                                                    protocol
                                                    src-addr
                                                    src-port
                                                    (make-object network-address% '0.0.0.0 '255.255.255.255 #t)
                                                    (rest line-tokens)
                                                    config)]
                [else (warning-unsupported/if-not "single address" line (first line-tokens) '(any host))
                      config])]))

;; number symbol symbol symbol address<%> port<%> address<%> (listof symbol) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list5 line name disposition protocol src-addr src-port dest-addr line-tokens config)
  (cond [(empty? line-tokens)
         (send config
               insert-ACE
               name
               (make-object extended-ACE-TCP/UDP%
                 line
                 (eqv? disposition 'permit)
                 src-addr
                 (string->symbol (string-append "prot-" (string-upcase (symbol->string protocol))))
                 src-port
                 dest-addr
                 (make-object port-range% 0 65535)))]
        [else (case (first line-tokens)
                [(eq) (parse-ip-named-access-list6 line
                                                   name
                                                   disposition
                                                   protocol
                                                   src-addr
                                                   src-port
                                                   dest-addr
                                                   (make-object port% (second line-tokens))
                                                   (drop line-tokens 2)
                                                   config)]
                [(match-any) (if (eqv? protocol 'tcp)
                                 (parse-ip-named-access-list8 line
                                                              name
                                                              disposition
                                                              src-addr
                                                              src-port
                                                              dest-addr
                                                              (make-object port-range% 0 65535)
                                                              (rest line-tokens)
                                                              config)
                                 (warning-unsupported/tcp line protocol))]
                [(match-all) (if (eqv? protocol 'tcp)
                                 (parse-ip-named-access-list9 line
                                                              name
                                                              disposition
                                                              src-addr
                                                              src-port
                                                              dest-addr
                                                              (make-object port-range% 0 65535)
                                                              (rest line-tokens)
                                                              config)
                                 (warning-unsupported/tcp line protocol))]
                [(reflect) (parse-ip-named-access-list7 line
                                                        name
                                                        disposition
                                                        protocol
                                                        src-addr
                                                        src-port
                                                        dest-addr
                                                        (make-object port-range% 0 65535)
                                                        (second line-tokens)
                                                        (drop line-tokens 2)
                                                        config)]
                [else (warning-unsupported line (first line-tokens) '(eq reflect))
                      config])]))

;; number symbol symbol symbol address<%> port<%> address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list6 line name disposition protocol src-addr src-port dest-addr dest-port line-tokens config)
  (cond [(empty? line-tokens)
         (send config
               insert-ACE
               name
               (make-object extended-ACE-TCP/UDP%
                 line
                 (eqv? disposition 'permit)
                 src-addr
                 (string->symbol (string-append "prot-" (string-upcase (symbol->string protocol))))
                 src-port
                 dest-addr
                 dest-port))]
        [else (case (first line-tokens)
                [(reflect) (parse-ip-named-access-list7 line
                                                        name
                                                        disposition
                                                        protocol
                                                        src-addr
                                                        src-port
                                                        dest-addr
                                                        dest-port
                                                        (second line-tokens)
                                                        (drop line-tokens 2)
                                                        config)]
                [(match-any) (if (eqv? protocol 'tcp)
                                 (parse-ip-named-access-list8 line
                                                              name
                                                              disposition
                                                              src-addr
                                                              src-port
                                                              dest-addr
                                                              dest-port
                                                              (rest line-tokens)
                                                              config)
                                 (warning-unsupported/tcp line protocol))]
                [(match-all) (if (eqv? protocol 'tcp)
                                 (parse-ip-named-access-list9 line
                                                              name
                                                              disposition
                                                              src-addr
                                                              src-port
                                                              dest-addr
                                                              dest-port
                                                              (rest line-tokens)
                                                              config)
                                 (warning-unsupported/tcp line protocol))]
                [else (warning-unsupported line (first line-tokens) '(reflect match-any match-all))
                      config])]))

;; number symbol symbol symbol address<%> port<%> address<%> port<%> symbol (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list7 line name disposition protocol src-addr src-port dest-addr dest-port reflect-name line-tokens config)
  (define added-outgoing
    (send config
          insert-ACE
          name
          (make-object extended-ACE-TCP/UDP%
            line
            (eqv? disposition 'permit)
            src-addr
            (string->symbol (string-append "prot-" (string-upcase (symbol->string protocol))))
            src-port
            dest-addr
            dest-port)))
  (define added-incoming-too
    (send added-outgoing
     insert-ACE
     reflect-name
     (make-object extended-reflexive-ACE-TCP/UDP%
       line
       (eqv? disposition 'permit)
       dest-addr
       (string->symbol (string-append "prot-" (string-upcase (symbol->string protocol))))
       dest-port ;src-port
       src-addr
       src-port ;dest-port
       reflect-name
       name)))
  
  
  ;(class* extended-ACE-TCP/UDP% (ACE<%>)
;    (init line-number permit source-addr protocol source-port dest-addr dest-port)
  (send added-incoming-too
     insert-insert-ACE
     reflect-name
     (make-object extended-reflexive-ACE-TCP/UDP%
       line
       (eqv? disposition 'permit)
       src-addr
       (string->symbol (string-append "prot-" (string-upcase (symbol->string protocol))))
       src-port
       dest-addr
       dest-port
       reflect-name
       name)))

;; number symbol symbol address<%> port<%> address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list8 line name disposition src-addr src-port dest-addr dest-port line-tokens config)
  (foldl (λ (flag result-config)
           (send result-config
                 insert-ACE
                 name
                 (make-object extended-ACE-TCP/flags%
                   line
                   (eqv? disposition 'permit)
                   src-addr
                   src-port
                   dest-addr
                   dest-port
                   `(,flag))))
         config
         (match-flags line-tokens)))

;; number symbol symbol address<%> port<%> address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list9 line name disposition src-addr src-port dest-addr dest-port line-tokens config)
  (send config
        insert-ACE
        name
        (make-object extended-ACE-TCP/flags%
          line
          (eqv? disposition 'permit)
          src-addr
          src-port
          dest-addr
          dest-port
          (match-flags line-tokens))))

;; (listof any) -> (listof (listof symbol))
(define (match-flags line-tokens)
  (if (empty? line-tokens)
      ; No flags in the rule, so assert only that flags is a TCPFlags variable (placeholder)
      '(TCPFlags)
      (map (λ (line-token)
             (let* [(line-token-text (symbol->string line-token))
                    (include (char=? (string-ref line-token-text 0) #\+))
                    (flag (string->symbol (string-upcase (substring line-token-text 1))))]
               (if include
                   flag
                   (string->symbol (string-append "!" (symbol->string flag))))))
           line-tokens)))

;; number symbol symbol (listof symbol) IOS-config% -> IOS-config%
(define (parse-ip-named-access-list-evaluate line name reflect-name line-tokens config)
  (send config
        insert-reflexive-ACL
        name
        reflect-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internet Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-ip line-tokens input config)
  (case (first line-tokens)
    [(access-list) (parse-ip-access-list (rest line-tokens) input config)]
    [(nat) (parse-nat (rest line-tokens) input config)]
    [(route) (parse-route (rest line-tokens) input config)]
    [else (warning-unsupported (line-number input) (first line-tokens) '(access-list nat route))
                      config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Address Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-nat line-tokens input config)
  (case (first line-tokens)
    [(inside outside) (parse-nat-translation (line-number input) (first line-tokens) (rest line-tokens) config)]
    [else (warning-unsupported (line-number input) (first line-tokens) '(inside outside))
                      config]))

;; number symbol (listof any) IOS-config% -> IOS-config%
(define (parse-nat-translation line side line-tokens config)
  (case (first line-tokens)
    [(source destination) (parse-nat-translation-side line side (first line-tokens) (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(source destination))
                      config]))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-nat-translation-side line side direction line-tokens config)
  (case (first line-tokens)
    [(list) (parse-list-nat line side direction (second line-tokens) (drop line-tokens 2) config)]
    [(route-map) (parse-route-map-nat line side direction (second line-tokens) (drop line-tokens 2) config)]
    [(static) (parse-static-nat line side direction (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(list route-map static))
                      config]))

;; number symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-list-nat line side direction ACL-ID line-tokens config)
  (case (first line-tokens)
    [(interface) (parse-list-nat-interface line
                                           side
                                           direction
                                           ACL-ID
                                           (second line-tokens)
                                           (drop line-tokens 2)
                                           config)]
    [else (warning-unsupported line (first line-tokens) '(interface))
                      config]))

;; number symbol symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-list-nat-interface line side direction ACL-ID interface-ID line-tokens config)
  (let [(overload (and (not (empty? line-tokens))
                       (eqv? (first line-tokens) 'overload)))]
    (case direction
      [(source) (send config
                      insert-dynamic-NAT
                      (make-object source-list-NAT%
                        line
                        side
                        ACL-ID
                        interface-ID
                        overload))]
      [(destination) (send config
                           insert-dynamic-NAT
                           (make-object destination-list-NAT%
                             line
                             side
                             ACL-ID
                             interface-ID
                             overload))]
    [else (warning-unsupported line direction '(source destination))
                      config])))

;; number symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-nat line side direction route-map line-tokens config)
  (case (first line-tokens)
    [(interface) (parse-route-map-nat-interface line
                                                side
                                                direction
                                                route-map
                                                (second line-tokens)
                                                (drop line-tokens 2)
                                                config)]
    [else (warning-unsupported line (first line-tokens) '(interface))
                      config]))

;; number symbol symbol symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-nat-interface line side direction route-map interface-ID line-tokens config)
  (let [(overload (and (not (empty? line-tokens))
                       (eqv? (first line-tokens) 'overload)))]
    (case direction
      [(source) (send config
                      insert-dynamic-NAT
                      (make-object source-map-NAT%
                        line
                        side
                        route-map
                        interface-ID
                        overload))]
      [(destination) (send config
                           insert-dynamic-NAT
                           (make-object destination-map-NAT%
                             line
                             side
                             route-map
                             interface-ID
                             overload))]
    [else (warning-unsupported line direction '(source destination))
                      config])))

;; number symbol symbol (listof any) IOS-config% -> IOS-config%
(define (parse-static-nat line side direction line-tokens config)
  (case (first line-tokens)
    [(tcp udp) (parse-static-NAT-TCP/UDP line
                                         side
                                         direction
                                         (first line-tokens)
                                         (make-object host-address% (second line-tokens))
                                         (make-object port% (third line-tokens))
                                         (drop line-tokens 3)
                                         config)]
    [else (cond [(single-address? (first line-tokens))
                 (parse-static-NAT-IP line
                                      side
                                      direction
                                      (make-object host-address% (first line-tokens))
                                      (make-object host-address% (second line-tokens))
                                      (drop line-tokens 2)
                                      config)]
                [else (warning-unsupported/if-not "tcp or udp" line (first line-tokens) '(<single-address>))
                      config])]))

;; number symbol symbol symbol address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-static-NAT-TCP/UDP line side direction prot from-address from-port line-tokens config)
  (case (first line-tokens)
    [(interface) (parse-static-NAT-TCP/UDP-interface line
                                                     side
                                                     direction
                                                     prot
                                                     from-address
                                                     from-port
                                                     (first line-tokens)
                                                     (rest line-tokens)
                                                     config)]
    [else (parse-static-NAT-TCP/UDP2 line
                                     side
                                     direction
                                     prot
                                     from-address
                                     from-port
                                     (make-object host-address% (first line-tokens))
                                     (make-object port% (second line-tokens))
                                     (drop line-tokens 2)
                                     config)]))

;; number symbol symbol symbol address<%> port<%> symbol (listof any) IOS-config% -> IOS-config%
(define (parse-static-NAT-TCP/UDP-interface line side direction prot from-address from-port interf line-tokens config)
  (case direction
    [(source)
     (send config
           insert-static-NAT
           (make-object static-source-NAT-TCP/UDP-interface%
             line
             side
             from-address
             from-port
             (string->symbol (string-append "prot-" (string-upcase (symbol->string prot))))
             interf))]
    [(destination)
     (send config
           insert-static-NAT
           (make-object static-destination-NAT-TCP/UDP-interface%
             line
             side
             from-address
             from-port
             (string->symbol (string-append "prot-" (string-upcase (symbol->string prot))))
             interf))]
    [else (warning-unsupported line direction '(source destination))
                      config]))

;; number symbol symbol symbol address<%> port<%> address<%> port<%> (listof any) IOS-config% -> IOS-config%
(define (parse-static-NAT-TCP/UDP2 line side direction prot from-addr from-port to-addr to-port line-tokens config)
  (case direction
    [(source)
     (send config
           insert-static-NAT
           (make-object static-source-NAT-TCP/UDP%
             line
             side
             from-addr
             from-port
             (string->symbol (string-append "prot-" (string-upcase (symbol->string prot))))
             to-addr
             to-port))]
    [(destination)
     (send config
           insert-static-NAT
           (make-object static-destination-NAT-TCP/UDP%
             line
             side
             from-addr
             from-port
             (string->symbol (string-append "prot-" (string-upcase (symbol->string prot))))
             to-addr
             to-port))]
    [else (warning-unsupported line direction '(source destination))
                      config]))

;; number symbol symbol address<%> address<%> (listof any) IOS-config% -> IOS-config%
(define (parse-static-NAT-IP line side direction from-addr to-addr line-tokens config)
  (case direction
    [(source)
     (send config
           insert-static-NAT
           (cond [(and (>= (length line-tokens) 2)
                       (eqv? (first line-tokens) 'route-map))
                  (make-object static-source-map-NAT%
                    line
                    side
                    from-addr
                    to-addr
                    (second line-tokens)
                    #f)]
                 [else (make-object static-source-NAT-IP%
                         line
                         side
                         from-addr
                         to-addr)]))]
    [(destination)
     (send config
           insert-static-NAT
           (cond [(and (>= (length line-tokens) 2)
                       (eqv? (first line-tokens) 'route-map))
                  (make-object static-destination-map-NAT%
                    line
                    side
                    from-addr
                    to-addr
                    (second line-tokens)
                    #f)]
                 [else (make-object static-destination-NAT-IP%
                         line
                         side
                         from-addr
                         to-addr)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-route line-tokens input config)
  (send config
        insert-static-route
        (if (single-address? (third line-tokens))
            (make-object static-route-gateway%
              (line-number input)
              (make-object network-address% (first line-tokens) (second line-tokens) #f)
              (make-object host-address% (third line-tokens)))
            (make-object static-route-interface%
              (line-number input)
              (make-object network-address% (first line-tokens) (second line-tokens) #f)
              (third line-tokens)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-interface line-tokens input config)
  (parse-interface-details  (first line-tokens) input config))

;; symbol port IOS-config% -> IOS-config%
(define (parse-interface-details name input config)
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (case (first line-tokens)
      [(ip) (parse-interface-details name input (parse-interface-IP (line-number input) name (rest line-tokens) config))]
      [(crypto) (parse-interface-details name input (parse-interface-crypto (line-number input) name (rest line-tokens) config))]
      [(switchport) (parse-interface-details name input (parse-interface-switchport (line-number input) name (rest line-tokens) config))]      
      ; For now, terminate multi-line constructs with ! on its own line
      [(!) config]
      [(shutdown)  (parse-interface-details name input (send config set-shutdown name #t))]
      [(access-list crypto end hostname interface ip route-map router)
       (warning-unsupported/ending-block (line-number input) (first line-tokens))
       (parse-IOS-details-for-line input config line-tokens)]
      [else (warning-unsupported (line-number input) (first line-tokens) '(ip crypto))
            (parse-interface-details name input config)])))

;; num symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-switchport line name line-tokens config)  
  (if (empty? line-tokens)
      config ; turn on switchport mode, but expect a mode command
      (case (first line-tokens)
        [(mode) (send config set-switchport-mode name (second line-tokens))] ; access or trunk
        [(trunk) (parse-switchport-trunk line name (rest line-tokens) config)] ; allowed vlan <comma separated list>        
        [(access) (case (second line-tokens) 
                    [(vlan) (send config set-switchport-vlans name (list (third line-tokens)))]
                    [else (warning-unsupported line (second line-tokens) '(vlan))
                          config])] ; access vlan <id>
        [else (warning-unsupported line (first line-tokens) '(cost priority))
              config])))

; works so long as only comma (no whitespace) separate 
(define (csl->list csl)
  ;(printf "csl to list: ~a~n" csl)
  (cond
    [(number? csl) (list (number->string csl))]
    [(symbol? csl) 
     (define str (symbol->string csl))
     (define lst (string-split str ","))
     (append* (map breakout-range lst))]))
    
(define (breakout-range r)  
  (define sp (string-split r "-"))
  ;(printf "breakout: ~a ~a~n" r sp)
  (cond
    [(equal? 1 (length sp)) sp]
    [(equal? 2 (length sp)) 
     ; e.g. '("10" "20")
     (define start (string->number (first sp)))
     (define end (string->number (second sp)))
     (append (range start end) (list end))]
    [else (raise "breakout-range got more than 2 symbols separated by dash")]))

(define (parse-switchport-trunk line name line-tokens config)  
   (case (first line-tokens)
     [(allowed) (parse-switchport-trunk-allowed line name (rest line-tokens) config)]      
     [else (warning-unsupported line (first line-tokens) '(allowed))
           config]))

(define (parse-switchport-trunk-allowed line name line-tokens config)  
   (case (first line-tokens)
     [(vlan) (parse-switchport-trunk-allowed-vlan line name (rest line-tokens) config)]      
     [else (warning-unsupported line (first line-tokens) '(allowed))
           config]))

(define (parse-switchport-trunk-allowed-vlan line name line-tokens config)
  (case (first line-tokens)
     [(add) 
      (when (> (length line-tokens) 2) ; add <csl>
        (printf "WARNING: improper parsing of vlan list: ~a~n" line-tokens))
      (send config add-switchport-vlans name (csl->list (second line-tokens)))]      
     [else 
      (when (> (length line-tokens) 1) ; <csl>
        (printf "WARNING: improper parsing of vlan list: ~a~n" line-tokens))
      (send config set-switchport-vlans name (csl->list (first line-tokens)))]))     

;; num symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-ip-ospf line name line-tokens config)  
   (case (first line-tokens)
     [(cost) (send config set-ospf-cost name (second line-tokens))]
     [(priority) (send config set-ospf-priority name (second line-tokens))]
     [else (warning-unsupported line (first line-tokens) '(cost priority))
           config]))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-IP line name line-tokens config)
  (case (first line-tokens)
    [(access-group) (parse-interface-ACL name (rest line-tokens) config)]
    [(address) (parse-interface-address name (rest line-tokens) config)]
    [(nat) (parse-interface-NAT name (rest line-tokens) config)]
    [(policy) (parse-interface-policy line name (rest line-tokens) config)]
    [(ospf) (parse-interface-ip-ospf line name (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(access-group address nat policy))
                      config]))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-address name line-tokens config)
  (case (first line-tokens)
    [(negotiated) config]
    [else (if (and (>= (length line-tokens) 3)
                   (eqv? (third line-tokens) 'secondary))
              (send config
                    set-secondary-interface-address
                    name
                    (make-object host-address% (first line-tokens))
                    (make-object network-address% (first line-tokens) (second line-tokens) #f))
              (send config
                    set-primary-interface-address
                    name
                    (make-object host-address% (first line-tokens))
                    (make-object network-address% (first line-tokens) (second line-tokens) #f)))]))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-ACL name line-tokens config)
  (send config
        set-interface-ACL
        name
        (first line-tokens)
        (eqv? (second line-tokens) 'in)))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-NAT name line-tokens config)
  (send config
        set-interface-NAT-side
        name
        (first line-tokens)))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-policy line name line-tokens config)
  (case (first line-tokens)
    [(route-map) (parse-interface-policy-route-map name (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(route-map))
                      config]))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-policy-route-map name line-tokens config)
  (send config
        set-interface-policy-route-map-ID
        name
        (first line-tokens)))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-crypto line name line-tokens config)
  (case (first line-tokens)
    [(map) (parse-interface-crypto-map name (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(map))
                      config]))

;; symbol (listof any) IOS-config% -> IOS-config%
(define (parse-interface-crypto-map name line-tokens config)
  (send config
        set-interface-crypto-map-ID
        name
        (first line-tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Route Maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-route-map line-tokens input config)
  (parse-route-map-details (first line-tokens)
                           (if (>= (length line-tokens) 3)
                               (third line-tokens)
                               10)
                           input
                           config))

;; symbol number port IOS-config% -> IOS-config%
(define (parse-route-map-details name sequence-num input config)
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (case (first line-tokens)
      [(match)
       (parse-route-map-details name
                                sequence-num
                                input
                                (parse-route-map-match (line-number input) name sequence-num (rest line-tokens) config))]
      [(set)
       (parse-route-map-details name
                                sequence-num
                                input
                                (parse-route-map-set (line-number input) name sequence-num (rest line-tokens) config))]
      ; For now, terminate multiline construct with !
      [(!) config]
      [(access-list crypto end hostname interface ip route-map router)
       (warning-unsupported/ending-block (line-number input) (first line-tokens))
       (parse-IOS-details-for-line input config line-tokens)]
      [else (warning-unsupported (line-number input) (first line-tokens) '(match set))
            (parse-route-map-details name sequence-num input config)])))
           
;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-match line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(ip) (parse-route-map-match-IP line name sequence-num (rest line-tokens) config)]
    [(length) (parse-route-map-match-length name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(ip length))
                      config]))

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-match-IP line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(address)
     (send config
           insert-route-map-match-ACL-ID
           name
           sequence-num
           (second line-tokens))]
    [else (warning-unsupported line (first line-tokens) '(address))
                      config]))

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-match-length name sequence-num line-tokens config)
  (send config
        insert-route-map-match-length
        name
        sequence-num
        (first line-tokens)))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(default) (parse-route-map-set-default line name sequence-num (rest line-tokens) config)]
    [(ip) (parse-route-map-set-IP line name sequence-num (rest line-tokens) config)]
    [(interface) (parse-route-map-set-interface line name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(default ip interface))
                      config]))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-default line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(interface) (parse-route-map-set-default-interface line name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(interface))
                      config]))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-default-interface line name sequence-num line-tokens config)
  (send config
        set-route-map-default-nexthop
        name
        sequence-num
        (make-object nexthop-interface% line (first line-tokens))))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-IP line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(default) (parse-route-map-set-IP-default line name sequence-num (rest line-tokens) config)]
    [(nexthop) (parse-route-map-set-IP-nexthop line name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(default nexthop))
                      config]))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-IP-default line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(nexthop) (parse-route-map-set-IP-default-nexthop line name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(nexthop))
                      config]))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-IP-default-nexthop line name sequence-num line-tokens config)
  (send config
        set-route-map-default-nexthop
        name
        sequence-num
        (make-object nexthop-gateway% line (make-object host-address% (first line-tokens)))))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-IP-nexthop line name sequence-num line-tokens config)
  (send config
        set-route-map-nexthop
        name
        sequence-num
        (make-object nexthop-gateway% line (make-object host-address% (first line-tokens)))))

;; number symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-route-map-set-interface line name sequence-num line-tokens config)
  (send config
        set-route-map-nexthop
        name
        sequence-num
        (make-object nexthop-interface% line (first line-tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Border Gateway Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-router line-tokens input config)
  (case (first line-tokens)
    [(bgp) (parse-BGP (second line-tokens) input config)]
    [else (warning-unsupported (line-number input) (first line-tokens) '(bgp))
                      config]))

;; symbol port IOS-config% -> IOS-config%
(define (parse-BGP as-num input config)
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (case (first line-tokens)
      [(network) (parse-BGP as-num input (parse-BGP-network (rest line-tokens) config))]
      [(neighbor) (parse-BGP as-num input (parse-BGP-neighbor (line-number input) (rest line-tokens) config))]
      ; for now...
      [(!) config]
      [(access-list crypto end hostname interface ip route-map router)
       (warning-unsupported/ending-block (line-number input) (first line-tokens))
       (parse-IOS-details-for-line input config line-tokens)]
      [else (warning-unsupported (line-number input) (first line-tokens) '(network neighbor))
            (parse-BGP as-num input config)])))
      
;; (listof any) IOS-config% -> IOS-config%
(define (parse-BGP-network line-tokens config)
  (send config
        insert-network
        (make-object host-address% (first line-tokens))))

;; number (listof any) IOS-config% -> IOS-config%
(define (parse-BGP-neighbor line line-tokens config)
  (case (second line-tokens)
    [(remote-as)
     (if (single-address? (first line-tokens))
         (send config
               insert-neighbor
               (make-object neighbor%
                 line
                 (make-object host-address% (first line-tokens))
                 (third line-tokens)))
         config)]
    [else (warning-unsupported line (second line-tokens) '(remote-as))
                      config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtual Private Networking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-crypto line-tokens input config)
  (case (first line-tokens)
    [(isakmp) (parse-crypto-isakmp (line-number input) (rest line-tokens) config)]
    [(map) (parse-crypto-map (rest line-tokens) input config)]
    [else (warning-unsupported (line-number input) (first line-tokens) '(default nexthop))
                      config]))

;; number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-isakmp line line-tokens config)
  (case (first line-tokens)
    [(key) (parse-crypto-isakmp-key line (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(key))
                      config]))

;; number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-isakmp-key line line-tokens config)
  (case (second line-tokens)
    [(address)
     (send config
           insert-endpoint
           (make-object preshared-key-endpoint%
             line
             (first line-tokens)
             (make-object host-address% (third line-tokens))))]
    [else (warning-unsupported line (second line-tokens) '(address))
                      config]))

;; (listof any) port IOS-config% -> IOS-config%
(define (parse-crypto-map line-tokens input config)
  (parse-crypto-map-details (first line-tokens) (second line-tokens) input config))

;; symbol number port IOS-config% -> IOS-config%
(define (parse-crypto-map-details name sequence-num input config)
  (let [(line-tokens (tokenize-line (read-line input 'any)))]
    (case (first line-tokens)
      [(match) (parse-crypto-map-details name
                                         sequence-num
                                         input
                                         (parse-crypto-map-match (line-number input) name sequence-num (rest line-tokens) config))]
      [(set) (parse-crypto-map-details name
                                       sequence-num
                                       input
                                       (parse-crypto-map-set (line-number input) name sequence-num (rest line-tokens) config))]
      ; for now...
      [(!) config]
            [(access-list crypto end hostname interface ip route-map router)
       (warning-unsupported/ending-block (line-number input) (first line-tokens))
       (parse-IOS-details-for-line input config line-tokens)]
      [else 
       (warning-unsupported (line-number input) (first line-tokens) '(match set))
       (parse-crypto-map-details name sequence-num input config)])))
      

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-map-match line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(address) (parse-crypto-map-match-address name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(address))
                      config]))

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-map-match-address name sequence-num line-tokens config)
  (send config
        insert-crypto-map-match-ACL-ID
        name
        sequence-num
        (first line-tokens)))

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-map-set line name sequence-num line-tokens config)
  (case (first line-tokens)
    [(peer) (parse-crypto-map-set-peer name sequence-num (rest line-tokens) config)]
    [else (warning-unsupported line (first line-tokens) '(peer))
                      config]))

;; symbol number (listof any) IOS-config% -> IOS-config%
(define (parse-crypto-map-set-peer name sequence-num line-tokens config)
  (send config
        set-crypto-map-peer-endpoint
        name
        sequence-num
        (make-object host-address% (first line-tokens))))

