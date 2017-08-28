#lang racket

(provide (all-defined-out))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-if-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> (car xs) tlans) (car xs) tlans))]))

(define (silly-double-1 x)
  (let ([x (+ x 2)]
        [y (+ x 3)]) (+ x y -5)))

(define (silly-double-2 x)
  (let* ([x (+ x 2)]
         [y (+ x 3)]) (+ x y -7)))

(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

(define (silly-mod-2 x)
  (letrec ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
           [odd? (lambda(x)(if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))
                  