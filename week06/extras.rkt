#lang racket

(provide (all-defined-out))

(require "hw4.rkt")

;; 1

;; 2
(define fibonacci
  (letrec ([f (lambda (x y) (cons x (lambda () (f y (+ x y)))))])
    (lambda () (f 0 1))))

;; 3
(define (stream-until f s)
  (letrec ([loop (lambda (st) (let ([next (st)])
                             (if (f) (loop (cdr next))
                                 (car next))))])
    (loop s)))
                                 
;; 4
(define (stream-map f s)
  (letrec ([g (lambda (st) (let ([next (st)])
                             (cons (f (car next))
                                   (lambda () (g (cdr next))))))])
  (lambda () (g s))))

;; 5
(define (stream-zip s1 s2)
  (letrec ([f (lambda (st1 st2) (let ([n1 (st1)]
                                      [n2 (st2)])
                                  (cons (cons (car n1) (car n2))
                                        (lambda () (f (cdr n1) (cdr n2))))))])
    (lambda () (f s1 s2))))

;; 7
;(define (interleave ss)
;  (letrec ([f (lambda (l)

