#lang racket

(provide (all-defined-out))

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define fibonacci
  (letrec ([f (lambda (x y) (cons x (lambda () (f y (+ x y)))))])
    (lambda () (f 0 1))))


