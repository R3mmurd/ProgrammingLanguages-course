#lang racket

(provide (all-defined-out))

(define (sums1 xs)
  (if (null? xs) 0 (+ (car xs) (sums1 (cdr xs)))))

(define (sums2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sums2 (cdr xs)))
          (+ (sums2 (car xs)) (sums2 (cdr xs))))))

(define (sums3 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sums3 (cdr xs)))
          (if (list? (car xs))
          (+ (sums3 (car xs)) (sums3 (cdr xs)))
          0))))