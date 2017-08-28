#lang racket

(provide (all-defined-out))

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs)) (funny-sum (cdr xs)))]))

(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

(define (Const-i e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e) (Const (- (Const-i (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-i (eval-exp(Add-e1 e)))]
                        [v2 (Const-i (eval-exp(Add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-i (eval-exp(Multiply-e1 e)))]
                             [v2 (Const-i (eval-exp(Multiply-e2 e)))])
                    (Const (* v1 v2)))]
        [#t (error "eval-exp expected and exp")]))
