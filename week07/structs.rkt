#lang racket

(provide (all-defined-out))

(struct foo (bar baz quux) #:transparent)

(struct const (i) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-i (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-i (eval-exp(add-e1 e)))]
                        [v2 (const-i (eval-exp(add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-i (eval-exp(multiply-e1 e)))]
                             [v2 (const-i (eval-exp(multiply-e2 e)))])
                    (const (* v1 v2)))]
        [#t (error "eval-exp expected and exp")]))