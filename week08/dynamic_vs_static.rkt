#lang racket

(provide (all-defined-out))

(define (f y)
  (if (> y 0) (+ y y ) "hi"))

(define (call-f x)
  (let ([ans (f x)])
    (if (number? ans) (number->string ans) ans)))

(define (cube x)
  (if (not (number? x))
      (error "bad arguments")
      (* x x x)))

(define (ff g)
  (cons (g 7) (g #t)))

(define pair-of-pairs
  (ff (lambda (x) (cons x x))))