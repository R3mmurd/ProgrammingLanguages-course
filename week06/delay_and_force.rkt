#lang racket

(provide (all-defined-out))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; first example, compute slow-add always
; Great if first argument 0
(my-mult 0 (lambda () (slow-add 3 4)))
; Ok if first argument 1
(my-mult 1 (lambda () (slow-add 3 4)))
; Worse otherwise
(my-mult 2 (lambda () (slow-add 3 4)))
(my-mult 3 (lambda () (slow-add 3 4)))
(my-mult 4 (lambda () (slow-add 3 4)))

; Precomputing slow-add, ok in all cases
(define snd-arg (slow-add 4 3))
(my-mult 0 (lambda () snd-arg))
(my-mult 1 (lambda () snd-arg))
(my-mult 2 (lambda () snd-arg))
(my-mult 3 (lambda () snd-arg))
(my-mult 4 (lambda () snd-arg))

; With thunk with promise for second argument
; Great if first argument 0
(my-mult 0 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))
; Ok otherwise
(my-mult 1 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))
(my-mult 2 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))
(my-mult 3 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))
(my-mult 4 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))