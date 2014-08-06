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
  ((C D E) I Q (C ∪ D) (I ∩ Q))
  ; immediate contracts
  ((I J) (flat P) (I ∩ J)) 
  ; delayed contracts
  ((Q R) (C → D) (x → C) (Q ∩ R))
  
  ; predicates
  (P integer)
  ; variables
  (x variable-not-otherwise-mentioned)
)

;; extended contracts with evaluation context
(define-extended-language Contracts contracts
  ; evaluation context
  (K hole (K → M) (C → K) (x → K) (K ∩ N) (C ∩ K) (K ∪ N) (C ∪ K))
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
   (--> (in-hole K (C ∩ I))
        (in-hole K (I ∩ C))
        (side-condition (not (canonical? (term (C ∩ I)))))
        "Reverse")

   ;; expand right
   (--> (in-hole K (C ∩ (D ∪ E)))
        (in-hole K ((C ∩ D) ∪ (C ∩ E)))
        "Right-Union")

   (--> (in-hole K (C ∩ (I ∩ Q)))
        (in-hole K (I ∩ (C ∩ Q)))
        "Right-Intersection")
   
   ;; expand left
   (--> (in-hole K ((C ∪ D) ∩ Q))
        (in-hole K ((C ∩ Q) ∪ (D ∩ Q)))
        "Left-Union")

   (--> (in-hole K ((I ∩ Q) ∩ R))
        (in-hole K (I ∩ (Q ∩ R)))
        "Left-Intersection")
 )
)

;; check canonicalize
(define (step? M) (= (length (apply-reduction-relation canonicalize M)) 1)) 
(redex-check Contracts M (if (not (canonical? (term M))) (step? (term M)) #t) #:attempts 100000)

;; tests
(test-results)