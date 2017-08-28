#lang racket

(define longer-strings
  (lambda ()
    (letrec ([f (lambda(s)
                  (cons s (f (string-append "A" s))))])
      (f "A"))))

(define (mystery s)
  (lambda ()
    (let ([pr (s)])
      (if (car pr)
          (cons (car pr) (mystery (cdr pr)))
          ((mystery (cdr pr)))))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))