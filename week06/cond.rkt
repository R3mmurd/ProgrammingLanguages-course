#lang racket

(provide (all-defined-out))

(define (sums xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sums (cdr xs)))]
        [(list? (car xs)) (+ (sums (car xs)) (sums (cdr xs)))]
        [#t 0]))

      