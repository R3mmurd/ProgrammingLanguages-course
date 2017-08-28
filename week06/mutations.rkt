#lang racket

(provide (all-defined-out))

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define g
  (let ([b b])
    (lambda (x) (* 1 (+ x b)))))
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)
(define a (f 4))