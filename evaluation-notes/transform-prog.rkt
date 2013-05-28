#lang racket

(require rackunit
         srfi/1)

(provide dynamic->static-policy)

; This module converts a [DYNAMIC netcore policy] and a [controller state]
; and produces a STATIC netcore policy that folds in the state.

; TN Feb 2013

; Effectively, we do this by removing references to state and replacing them.
; We can't just replace state references with true or false, because that would 
; require bindings to packet-field variables. 
; But we can concretize the references in in the policy, embedding the relevant 
; parts of the current state into the STATIC policy.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I'd love to use typed/racket here but it was too much of a pain using match.
; Also no way to say k-ary tuple? parameterized?
;(define-type Tuple (Listof Symbol))
;(define-type Relation (Listof Tuple))
;(define-type State (HashTable Symbol Relation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; produce-pred: 
; Accept a state, a relation name, and a variable vector and produce a
; corresponding predicate. If there is no way for the predicate to match 
; (i.e. the relation is empty in current state) return the 'False predicate.

; Return the OR of every potential match to (R x y z...) in the state.
; Individual matches have components connected with AND.
; If a component is existentially quantified, don't add a conjunct for it.
(define (produce-pred relname varvec st existentials)  
  (unless (hash-has-key? st relname) 
    (error "bad use of produce-pred"))
  (define (produce-conj tuple)
    `(And ,@(filter-map (lambda (pr) (cond [(member (first pr) existentials) #f]
                                           [else `(= ,(first pr) ,(second pr))]))
                        (zip varvec tuple))))
  (define ands (map produce-conj (hash-ref st relname)))  
  (cond [(empty? ands) 'False]
        [(equal? 1 (length ands)) (first ands)]
        [else `(Or ,@ands)]))

(check-equal? (produce-pred 'R '(pkt.header.srcip pkt.header.srcport) #hash( (R . ())) empty)
              'False)
(check-equal? (produce-pred 'R '(pkt.header.srcip pkt.header.srcport) #hash( (R . ((1 2)))) empty)
              '(And (= pkt.header.srcip 1) (= pkt.header.srcport 2)))
(check-equal? (produce-pred 'R '(pkt.header.srcip pkt.header.srcport) #hash( (R . ((1 2) (3 4))))  empty)
              '(Or (And (= pkt.header.srcip 1) (= pkt.header.srcport 2))
                   (And (= pkt.header.srcip 3) (= pkt.header.srcport 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eval-atom:
; Remove state references from an atomic formula: true, false, or a relational expression.
; The (? symbol? x) bits in the match below make sure this is a base case.
(define (eval-atom atom st #:exists [existentials empty])
  (match atom 
    ['True 'True]
    ['False 'False]    
    [(list (? symbol? relname) 
           (? (lambda (x) (or (symbol? x) (number? x))) vec) ...)
     (cond [(hash-has-key? st relname)                                   
            (cond [(member vec (hash-ref st relname)) 'True]
                  [else (produce-pred relname vec st existentials)])]                                                    
           [else atom])]      
    [else (error (format "Bad atom or predicate: ~v" atom))]))

(check-equal? (eval-atom 'True #hash( (R . ())))
              'True)
(check-equal? (eval-atom 'False #hash( (R . ())))
              'False)

(check-equal? (eval-atom '(R 2 1 3) #hash( (R . ((2 1 3)))))
              'True)
(check-equal? (eval-atom '(R pkt.x pkt.y pkt.z) #hash( (R . ((2 1 3)))))
              '(And (= pkt.x 2) (= pkt.y 1) (= pkt.z 3) ))
(check-equal? (eval-atom '(somethingelse maybe about packet headers) #hash( (R . ((2 1 3)))))
              '(somethingelse maybe about packet headers))

(check-equal? (eval-atom '(R pkt.x foo pkt.z) #hash( (R . ((2 1 3) (10 11 12)))) #:exists '(foo))
              '(Or (And (= pkt.x 2) (= pkt.z 3)) (And (= pkt.x 10) (= pkt.z 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; eval-predicate
; Partially evaluate a policy predicate. Some atoms may involve state, some may not.
; The key is: if the state lets us know whether an atom is true/false, evaluate it. 
; Otherwise, carry over to static time.
; (Note: what NetCore calls a predicate is just a condition formula.)

(define (eval-predicate fmla st)
  (match fmla
    [(list 'And args ...) (define inners (map (lambda (p) (eval-predicate p st)) args))                                 
                          (define non-true-inners (filter (lambda (x) (not (equal? x 'True))) inners))
                          (cond [(member 'False inners) 'False]
                                [(equal? 1 (length non-true-inners)) (first non-true-inners)]
                                [(not (empty? non-true-inners)) `(And ,@non-true-inners)]
                                [else 'True])]
    
    [(list 'Or args ...)  (define inners (map (lambda (p) (eval-predicate p st)) args))                     
                          (define non-false-inners (filter (lambda (x) (not (equal? x 'False))) inners))
                          (cond [(member 'True inners) 'True]
                                [(equal? 1 (length non-false-inners)) (first non-false-inners)]
                                [(not (empty? non-false-inners)) `(Or ,@non-false-inners)]
                                [else 'False])]
        
    ; Existential binding. ONLY VALID if immediately scoping an atomic fmla.
    ; TODO: less restricted use if needed. eval-atom throws an error if not an atomic fmla.
    ; (Yes, only one existential for now...this is a proof of concept)
    [(list 'Exists var atom) (eval-atom atom st #:exists (list var))] 
    
    [(list 'Not arg) (define inner (eval-predicate arg st)) 
                     (cond [(equal? inner 'False) 'True]
                           [(equal? inner 'True) 'False]
                           [else `(Not ,inner)])]
        
    [else (eval-atom fmla st)]))


(check-equal? (eval-predicate '(And (foo x y z) (R 2 1 3)) #hash( (R . ((2 1 3)))))
              '(foo x y z))
(check-equal? (eval-predicate '(Or (foo x y z) (R 2 1 3)) #hash( (R . ((2 1 3)))))
              'True)
(check-equal? (eval-predicate '(And (foo x y z) (R pkt.a pkt.b pkt.c)) #hash( (R . ((2 1 3)))))
              '(And (foo x y z) (And (= pkt.a 2) (= pkt.b 1) (= pkt.c 3))))
(check-equal? (eval-predicate '(Or (foo x y z) (R pkt.a pkt.b pkt.c)) #hash( (R . ((2 1 3)))))
              '(Or (foo x y z) (And (= pkt.a 2) (= pkt.b 1) (= pkt.c 3))))
(check-equal? (eval-predicate '(Or (foo x y z) (R pkt.a pkt.b pkt.c) (= y x)) #hash( (R . ((2 1 3)))))
              '(Or (foo x y z) (And (= pkt.a 2) (= pkt.b 1) (= pkt.c 3)) (= y x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; rule format:
; ( (idbname vec ...) fmla)

; eval-rule
; Remove state references from a rule's preducate
; "Rule" here is ~= a PoBasic in NetCore.
; Return #f to trigger rule removal if the rule never fires
(define (eval-rule rule st)
  (match rule 
    [(list (list idbname vec ...) fmla) 
     (define newpredicate (eval-predicate fmla st))
     (cond [(equal? newpredicate 'False) #f]
           [else `( (,idbname ,@vec) ,newpredicate)])]
     [else (error (format "bad rule sexpr: ~v" rule))]))

; All constants in the vector: true becomes false.
(check-equal? (eval-rule '( (emit op np) (Not (R 2 1 3))) #hash( (R . ((2 1 3)))))
              #f)
; All constants in the vector: true
(check-equal? (eval-rule '( (emit op np) (R 2 1 3)) #hash( (R . ((2 1 3)))))
              '((emit op np) True))
(check-equal? (eval-rule '( (emit op np) (And (R pkt.x pkt.y pkt.z) (= pkt.k 12))) #hash( (R . ((2 1 3)))))
              '((emit op np) (And (And (= pkt.x 2) (= pkt.y 1) (= pkt.z 3)) (= pkt.k 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic->static-policy ruleset st)
  (filter-map (lambda (r) (eval-rule r st)) ruleset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exists and Forall pose an interesting complication. Our goal is to embed valid bindings
; into the static policy, factoring out the state. For normal packet-field variables, this
; is easy: it's just some conjunction and disjunction over(= pktfieldname constant-in-relation).

; For quantified vars it's a bit more complex. At first, I wanted to have a "*" and "_" wildcard
; but that's too weak to support MAC learning. See above: we need linked instances of these vars 
; over multiple atomic fmlas. 

; So how can we compute what's valid? And... where do we embed those statements in the static pol?

; Let's look at a couple examples. 
;'(Exists send-to (And (learned pkt.loc.sw send-to pkt.header.dldst)
;                      (= send-to newpkt.loc.port)
;                      (= pkt.header newpkt.header)
;                      (= pkt.loc.sw newpkt.loc.sw)))

; We need the send-to binding to spread over 2 different atoms. The other two don't NEED to 
; << Wait, why do we need an exists, given the second binding? We do NOT. Which means MAC learning could work with */_.

; _, positive predicate: just the bindings of all constants present. If more than one, or(and...)) them. e.g.
; R = { (1, 5, 2), (2, 5, 3), (1, 2, 3)} would become
; R(_, 5, pktbleh) ---> (or (pktbleh = 2) (pktbleh = 3))
; R(_, 5, _) ---> TRUE.
; R(pktbleh, _, _) ---> (or (pktbleh = 1) (pktbleh = 2))
; 
; what about a negative pred with _? Now we're expressing everything NOT there. Need to negate:
; R = { (1, 5, 2), (2, 5, 3), (1, 2, 3)} would become
; NOT R(_, 5, pktbleh) ---> (not (or (pktbleh = 2) (pktbleh = 3)))
; NOT R(_, 5, _) ---> FALSE.
; NOT R(pktbleh, _, _) ---> (not (or (pktbleh = 1) (pktbleh = 2)))
; so that's just the negation in normal position. Cool.

; Note on semantics. Negative pred with _ does NOT MEAN THIS:
; exists _ not R(_, 5, p)
; which is:
; not forall _ R(_, 5, p)
; (which would raise the domain question)
;
; instead it means:
; not exists _ R(_, 5, p)
; which is:
; forall _ not R(_, 5, p)
;;;;;;;;

; *, positive predicate (note new state example):
; Here we have a problem: forall * R(*, 5, p) -- all _whats_? This
; is the domain problem in Datalog. (TODO: solve)

; *, negative predicate
; This is: NOT forall x R(x, 5, p)
; which is: exists x NOT R(x, 5, p)
; this also raises the domain problem (as noted above). TODO: solve.

; So we can handle in-place exists easy, but forall is harder due to the domain problem.

; But we DONT NEED UNIVERSALS for MAC learning. What needs universals?

; MAC learning: originally we said:
;'(Forall send-to (Not (learned pkt.loc.sw send-to pkt.header.dldst)))
; but it's equivalent to say this (using e.g. a _):
;'(Not (Exists send-to (learned pkt.loc.sw send-to pkt.header.dldst)))

; forall x not L(p1, x, p2)
; not exists x L(p1, x, p2)

