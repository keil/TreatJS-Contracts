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
  
  ; non-canonical contarcts (expressions)
  ((L M N)
   (flat P) ; flat contract
   (M → N)  ; function contract
   (x → M)  ; dependent contract
   (M ∪ N)  ; union contract
   (M ∩ N)  ; intersection contract
  )

  ; canonical contracts
  ((C D E) I Q (M ∪ N) (I ∩ C))
  ; immediate contracts
  ((I J) (flat P))
  ; delayed contracts
  ((Q R) (M → N) (x → M) (Q ∩ R))
  
  ; predicates
  (P integer)
  ; variables
  (x variable-not-otherwise-mentioned)
)

;; extended contracts with evaluation context
(define-extended-language Contracts contracts
  ; evaluation context
  (K hole (K ∩ M) (Q ∩ K))
)

;; predicates
(define canonical? (redex-match? Contracts C))
(define delayed? (redex-match? Contracts Q))
(define immediate? (redex-match? Contracts I))

;; contract transformations
(define canonicalize
  (reduction-relation
   Contracts

   ; union
   (--> (in-hole K (M ∪ N))
        ((in-hole K (M)) ∪ (in-hole K (N)))
        "Union")
   
   ; intersection
   (--> ((in-hole K I) ∩ M)
        (I ∩ (in-hole K M))
        "Intersection")
   
   ;; reverse
   (--> (Q ∩ (in-hole K I))
        (I ∩ (in-hole K Q))
        "Reverse")
 )
)

;; check canonicalize
(define (step? M) (= (length (apply-reduction-relation canonicalize M)) 1)) 
(redex-check Contracts M (if (not (canonical? (term M))) (step? (term M)) #t) #:attempts 1000000)

;; tests
(test-results)