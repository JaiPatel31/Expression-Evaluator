#lang racket

;; ============================================================
;;   RESULT TYPE (Either-style)
;; ============================================================

(struct success (val) #:transparent)
(struct failure (val) #:transparent)

(define (from-success default r)
  (if (success? r) (success-val r) default))

(define (from-failure default r)
  (if (failure? r) (failure-val r) default))


;; ============================================================
;;   ENVIRONMENT (STATE)
;;   stored as: list of (cons 'id value)
;;   value is number OR 'undefined
;; ============================================================

(define empty-env '())

(define (env-lookup env id)
  (assoc id env)) ; -> (cons id val) or #f

(define (env-defined? env id)
  (and (env-lookup env id) #t))

(define (env-get env id)
  (let ([p (env-lookup env id)])
    (if p (cdr p) 'not-found)))

(define (env-extend env id val)
  (cons (cons id val) env))

(define (env-update env id new-val)
  (cond
    [(null? env) env]
    [(eq? (caar env) id)
     (cons (cons id new-val) (cdr env))]
    [else
     (cons (car env) (env-update (cdr env) id new-val))]))

(define (env-remove env id)
  (cond
    [(null? env) '()]
    [(eq? (caar env) id)
     (cdr env)]
    [else
     (cons (car env) (env-remove (cdr env) id))]))


;; ============================================================
;;   SAFE DIVISION
;; ============================================================

(define (safe-div x y)
  (if (= y 0)
      'div-zero
      (/ x y)))


;; ============================================================
;;   VALID IDENTIFIER?
;; ============================================================

(define (valid-id? s)
  (and (symbol? s)
       (let* ([str (symbol->string s)]
              [first (string-ref str 0)])
         (and (char-alphabetic? first)
              (for/and ([c (in-string str)])
                (or (char-alphabetic? c)
                    (char-numeric? c)
                    (char=? c #\-)
                    (char=? c #\_)))))))


;; ============================================================
;;   MAIN EVALUATOR: eval-expr
;;   Returns (success (list value new-env))
;;        or (failure (list message env))
;; ============================================================

(define (eval-expr expr env)
  (cond
    ;; --------------------------------------------------------
    ;; NUMERIC LITERALS
    ;; --------------------------------------------------------
    [(and (list? expr) (equal? (first expr) 'num))
     (success (list (second expr) env))]

    ;; --------------------------------------------------------
    ;; BINARY OPERATIONS
    ;; --------------------------------------------------------
    [(and (list? expr)
          (member (first expr) '(add sub mult div)))
     (eval-bin-op (first expr) (second expr) (third expr) env)]

    ;; --------------------------------------------------------
    ;; IDENTIFIER LOOKUP: (id x)
    ;; --------------------------------------------------------
    [(and (list? expr) (equal? (first expr) 'id))
     (eval-id (second expr) env)]

    ;; --------------------------------------------------------
    ;; DEFINE / ASSIGN / REMOVE
    ;; --------------------------------------------------------
    [(and (list? expr) (equal? (first expr) 'define))
     (eval-define expr env)]

    [(and (list? expr) (equal? (first expr) 'assign))
     (eval-assign expr env)]

    [(and (list? expr) (equal? (first expr) 'remove))
     (eval-remove expr env)]

    ;; --------------------------------------------------------
    ;; ERROR: UNKNOWN FORM
    ;; --------------------------------------------------------
    [else
     (failure (list (format "Error: unknown expression ~a" expr)
                    env))]))


;; ============================================================
;;   BINARY OPERATORS
;; ============================================================

(define (eval-bin-op op e1 e2 env)
  (let ([r1 (eval-expr e1 env)])
    (if (failure? r1)
        r1
        (let* ([v1 (first (success-val r1))]
               [env1 (second (success-val r1))]
               [r2 (eval-expr e2 env1)])
          (if (failure? r2)
              r2
              (let* ([v2 (first (success-val r2))]
                     [env2 (second (success-val r2))]
                     [op-name op])
                (cond
                  [(eq? op-name 'add)
                   (success (list (+ v1 v2) env2))]

                  [(eq? op-name 'sub)
                   (success (list (- v1 v2) env2))]

                  [(eq? op-name 'mult)
                   (success (list (* v1 v2) env2))]

                  [(eq? op-name 'div)
                   (if (equal? (safe-div v1 v2) 'div-zero)
                       (failure (list "Error: division by zero" env2))
                       (success (list (safe-div v1 v2) env2)))])))))))


;; ============================================================
;;   (id x)
;; ============================================================

(define (eval-id name env)
  (cond
    [(not (valid-id? name))
     (failure (list (format "Error: invalid identifier ~a" name) env))]

    [(not (env-defined? env name))
     (failure (list (format "Error: id ~a not defined" name) env))]

    [(eq? (env-get env name) 'undefined)
     (failure (list (format "Error: id ~a is undefined" name) env))]

    [else
     (success (list (env-get env name) env))]))


;; ============================================================
;;   (define a) OR (define a expr)
;; ============================================================

(define (eval-define expr env)
  (define id (second expr))

  (cond
    [(env-defined? env id)
     (failure (list (format "Error: define ~a: already defined" id)
                    env))]

    ;; (define a)
    [(= (length expr) 2)
     (success (list 'ok (env-extend env id 'undefined)))]

    ;; (define a expr)
    [(= (length expr) 3)
     (let ([r (eval-expr (third expr) env)])
       (if (failure? r)
           r
           (let ([v (first (success-val r))])
             (success (list v (env-extend env id v))))))]

    [else
     (failure (list "Error: define usage incorrect" env))]))


;; ============================================================
;;   (assign a expr)
;; ============================================================

(define (eval-assign expr env)
  (define id (second expr))

  (cond
    [(not (env-defined? env id))
     (failure (list (format "Error: assign ~a: id not defined" id) env))]

    [(not (eq? (env-get env id) 'undefined))
     (failure (list (format "Error: assign ~a: already has value" id) env))]

    [(not (= (length expr) 3))
     (failure (list "Error: assign usage incorrect" env))]

    [else
     (let ([r (eval-expr (third expr) env)])
       (if (failure? r)
           r
           (let ([v (first (success-val r))])
             (success (list v (env-update env id v))))))]))


;; ============================================================
;;   (remove a)
;; ============================================================

(define (eval-remove expr env)
  (define id (second expr))

  (cond
    [(not (env-defined? env id))
     (failure (list (format "Error: remove ~a: identifier not defined, ignoring"
                            id)
                    env))]

    [(not (= (length expr) 2))
     (failure (list "Error: remove usage incorrect" env))]

    [else
     (success (list 'ok (env-remove env id)))]))


;; ============================================================
;;   PRINTING
;; ============================================================

(define (print-result r)
  (cond
    [(success? r)
     (define val (first (success-val r)))
     (define env (second (success-val r)))
     (printf "Success: ~a\n" val)
     (printf "State: ~a\n\n" env)
     env]

    [(failure? r)
     (define msg (first (failure-val r)))
     (define env (second (failure-val r)))
     (printf "~a\n" msg)
     (printf "State: ~a\n\n" env)
     env]))


;; ============================================================
;;   REPL
;; ============================================================

(define (repl env)
  (display "> ")
  (flush-output)
  (define input (read))
  (cond
    [(eof-object? input) (printf "Goodbye.\n")]
    [(eq? input 'quit)  (printf "Goodbye.\n")]
    [else
     (define result (eval-expr input env))
     (define new-env (print-result result))
     (repl new-env)]))

(provide (all-defined-out))

(repl empty-env)
