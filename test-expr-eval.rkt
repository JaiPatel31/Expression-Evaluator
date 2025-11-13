#lang racket
(require rackunit)
(require "expression-evaluator-1.rkt") 


;; ============================================================
;; HELPER: extract values & state from success/failure
;; ============================================================

(define (result-val r)
(cond [(success? r) (first (success-val r))]
            [(failure? r) (first (failure-val r))]
            [else (error "Not a result")]))

(define (result-env r)
  (cond [(success? r) (second (success-val r))]
        [(failure? r) (second (failure-val r))]
        [else (error "Not a result")]))


;; ============================================================
;; TESTS BEGIN
;; ============================================================

(module+ test
  (displayln "Running interpreter tests...")

  ;; ------------------------------------------------------------
  ;; 1. define a => creates variable with undefined
  ;; ------------------------------------------------------------
  (define r1 (eval-expr '(define a) empty-env))

  (check-true (success? r1))
  (check-equal? (result-val r1) 'ok)
  (check-equal? (result-env r1) '((a . undefined)))

  ;; ------------------------------------------------------------
  ;; 2. Using undefined variable should fail
  ;; ------------------------------------------------------------
  (define r2 (eval-expr '(id a) (result-env r1)))

  (check-true (failure? r2))
  (check-true (regexp-match? #px"undefined" (result-val r2)))

  ;; ------------------------------------------------------------
  ;; 3. assign a (num 5) => succeeds because a is undefined
  ;; ------------------------------------------------------------
  (define r3 (eval-expr '(assign a (num 5)) (result-env r1)))

  (check-true (success? r3))
  (check-equal? (result-val r3) 5)
  (check-equal? (result-env r3) '((a . 5)))

  ;; ------------------------------------------------------------
  ;; 4. define a again => must fail (already defined)
  ;; ------------------------------------------------------------
  (define r4 (eval-expr '(define a) (result-env r3)))

  (check-true (failure? r4))
  (check-true (regexp-match? #px"already defined" (result-val r4)))

  ;; ------------------------------------------------------------
  ;; 5. define b using expression (add (id a) (num 1))
  ;; ------------------------------------------------------------
  (define r5 (eval-expr '(define b (add (id a) (num 1))) (result-env r3)))

  (check-true (success? r5))
  (check-equal? (result-val r5) 6)
  (check-equal? (result-env r5) '((b . 6) (a . 5)))

  ;; ------------------------------------------------------------
  ;; 6. Using b inside an expression (add (num 5) (id b))
  ;; ------------------------------------------------------------
  (define r6 (eval-expr '(add (num 5) (id b)) (result-env r5)))

  (check-true (success? r6))
  (check-equal? (result-val r6) 11)

  ;; ------------------------------------------------------------
  ;; 7. assign b again => must fail (immutability)
  ;; ------------------------------------------------------------
  (define r7 (eval-expr '(assign b (num 99)) (result-env r5)))

  (check-true (failure? r7))
  (check-true (regexp-match? #px"already has value" (result-val r7)))

  ;; ------------------------------------------------------------
  ;; 8. remove b => should succeed
  ;; ------------------------------------------------------------
  (define r8 (eval-expr '(remove b) (result-env r5)))

  (check-true (success? r8))
  (check-equal? (result-val r8) 'ok)
  (check-equal? (result-env r8) '((a . 5)))

  ;; ------------------------------------------------------------
  ;; 9. remove b again => should FAIL (not defined)
  ;; ------------------------------------------------------------
  (define r9 (eval-expr '(remove b) (result-env r8)))

  (check-true (failure? r9))
  (check-true (regexp-match? #px"identifier not defined" (result-val r9)))

  ;; ------------------------------------------------------------
  ;; 10. add involving removed variable => must fail
  ;; ------------------------------------------------------------
  (define r10 (eval-expr '(add (num 1) (id b)) (result-env r8)))

  (check-true (failure? r10))
  (check-true (regexp-match? #px"id b not defined" (result-val r10)))

  ;; ------------------------------------------------------------
  ;; 11. remove non-existing variable "d"
  ;; ------------------------------------------------------------
  (define r11 (eval-expr '(remove d) (result-env r8)))

  (check-true (failure? r11))
  (check-true (regexp-match? #px"remove d: identifier not defined" (result-val r11)))

  ;; ------------------------------------------------------------
  ;; 12. define c with expression that FAILS inside
  ;; ------------------------------------------------------------
  ;; (define c (id z))   ;; z not defined
  (define r12 (eval-expr '(define c (id z)) (result-env r8)))

  (check-true (failure? r12))
  (check-true (regexp-match? #px"id z not defined" (result-val r12)))

  ;; ------------------------------------------------------------
  ;; 13. Normal arithmetic chain: define, assign, use
  ;; ------------------------------------------------------------
  (define r13 (eval-expr '(define q) empty-env))
  (define r14 (eval-expr '(assign q (num 10)) (result-env r13)))
  (define r15 (eval-expr '(add (id q) (num 5)) (result-env r14)))

  (check-true (success? r15))
  (check-equal? (result-val r15) 15)

  (displayln "All tests passed!")
)
