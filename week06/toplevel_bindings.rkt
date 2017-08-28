#lang racket

(provide (all-defined-out))

(define (f x) (+ x (+ x b)))

(define b 3)

(define c (+ b 4))
; (define d (+ e 4))
(define e 5)
;(define f 17)