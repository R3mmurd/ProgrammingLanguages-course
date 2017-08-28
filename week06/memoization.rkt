#lang racket

(provide (all-defined-out))

(define (fib1 x)
  (if (or (= x 0) (= x 1))
      x
      (+ (fib1 (- x 1)) (fib1 (- x 2)))))

(define fib2
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (< x 2)
                                         x
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
