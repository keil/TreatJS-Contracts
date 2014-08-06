#lang racket
(require redex)

#|
 _____             _      _ ___ 
|_   _| _ ___ __ _| |_ _ | / __|
  | || '_/ -_) _` |  _| || \__ \
  |_||_| \___\__,_|\__|\__/|___/
                                 
TreatJS: Higher-Order Contracts for JavaScript
http://proglang.informatik.uni-freiburg.de/treatjs/

Copyright (c) 2014, Proglang, University of Freiburg.
http://proglang.informatik.uni-freiburg.de/
All rights reserved.

Author Matthias Keil
http://www.informatik.uni-freiburg.de/~keilr/

|#

;; language of TreatJS contracts
(define-language contracts
  
  ; non-canonical contarcts (expressions
  ((L M N)
   (flat P)     ; flat contract

   (M → N)      ; function contract
   (x → M)      ; dependent contract

   (X ...)      ; object contract
   
   (M ∪ N)      ; union contract
   (M ∩ N)      ; intersection contract

   (M ∧ N)      ; and contract
   (M ∨ N)      ; or contract
   
   (¬ M)        ; negation
   (with x : M) ; with contract
  )
  
  ; mapping
  (X (x ↦ M))
  
  ; binary operators
  (• α β)
  ; intersection and or
  (α ∩ ∨)
  ; union and and
  (β ∪ ∧)

  ; canonical contracts
  ((C D E)
   I             ; immediate contract 
   Q             ; delayed contarct
   (C ∪ D)       ; top-level union
   (I ∩ Q)       ; top-level intersection
   (C ∧ D)       ; top-level and
   (I ∨ Q)       ; top-level or
  )
  ; immediate contracts
  ((I J)
   (flat P)      ; base/flat contarct
   (X ...)      ; object contract
   (I ∩ J)       ; intersection contract
   (I ∨ J)       ; or contract 
   (¬ I)         ; negation contract
   (with x : I)  ; with contract
  ) 
  ; delayed contracts
  ((Q R)
   (C → D)       ; function contract
   (x → C)       ; dependent contract
   (Q ∩ R)       ; intersection contract
   (Q ∨ R)       ; or contract
   (¬ Q)         ; negation cntract
   (with x : Q)  ; with contract
   )
  
  ; predicates
  (P integer)
  ; variables
  (x variable-not-otherwise-mentioned)
)

;; extended contracts with evaluation context
(define-extended-language Contracts contracts
  ; evaluation context
  (K hole
     (K → M) (C → K) ; function contract
     (x → K)         ; dependent contract
     (K ∩ N) (C ∩ K) ; intersection contract
     (K ∪ N) (C ∪ K) ; union contract
     (K ∨ N) (C ∨ K) ; or contract
     (K ∧ N) (C ∧ K) ; and contract
     (¬ K)           ; negation contract
     (with x : K)    ; with contract
     )
)

;; predicates
(define canonical? (redex-match? Contracts C))
(define delayed? (redex-match? Contracts Q))
(define immediate? (redex-match? Contracts I))

;; contract transformations
(define canonicalize
  (reduction-relation
   Contracts
   
      ;; reverse
   (--> (in-hole K (C α I))
        (in-hole K (I α C))
        (side-condition (not (canonical? (term (C ∩ I)))))
        "Reverse")

   ;; expand right
   (--> (in-hole K (C α (D β E)))
        (in-hole K ((C α D) β (C α E)))
        "Right-Union")
   (--> (in-hole K (C α_1 (I α_2 Q)))
        (in-hole K ((C α_1 I) α_2 (C α_1 Q)))
        "Right-Intersection")
   
   ;; expand left
   (--> (in-hole K ((C β D) α Q))
        (in-hole K ((C α Q) β (D α Q)))
        "Left-Union")

   (--> (in-hole K ((I α_1 Q) α_2 R))
        (in-hole K ((I α_2 R) α_1 (Q α_2 R)))
        "Left-Intersection")
   
   ;; canonicalize negation
   (--> (in-hole K (¬ (C ∪ D)))
        (in-hole K ((¬ C) ∩ (¬ D)))
        "Flatten-NegationOnUnion")
   (--> (in-hole K (¬(I ∩ Q)))
        (in-hole K ((¬ I) ∪ (¬ Q)))
        "Flatten-NgationOnIntersection")
   
    (--> (in-hole K (¬ (C ∧ D)))
        (in-hole K ((¬ C) ∨ (¬ D)))
        "Flatten-NegationOnAnd")
   (--> (in-hole K (¬(I ∨ Q)))
        (in-hole K ((¬ I) ∧ (¬ Q)))
        "Flatten-NgationOnOr")
 
   ;; canonicalize with
   (--> (in-hole K (with x : (C ∪ D)))
        (in-hole K ((with x : C) ∪ (with x : D)))
        "Flatten-WithOnUnion")
   (--> (in-hole K (with x : (I ∩ Q)))
        (in-hole K ((with x : I) ∩ (with x : Q)))
        "Flatten-WithOnIntersection")

   (--> (in-hole K (with x : (C ∧ D)))
        (in-hole K ((with x : C) ∧ (with x : D)))
        "Flatten-WithOnAnd")
   (--> (in-hole K (with x : (I ∨ Q)))
        (in-hole K ((with x : I) ∨ (with x : Q)))
        "Flatten-WithOnOr")
 ) 
)

;; check canonicalize
(define (step? M) (= (length (apply-reduction-relation canonicalize M)) 1)) 
(redex-check Contracts M (if (not (canonical? (term M))) (step? (term M)) #t) #:attempts 100000)

;; tests
(test-results)