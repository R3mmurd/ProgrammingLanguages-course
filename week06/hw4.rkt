
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec
                ([nth (lambda (l i) (if (= i 0) (car l) (nth (cdr l) (- i 1))))]
                 [sz (length xs)])
              (nth xs (modulo n sz)))]))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; Problem 5 TODO Review integer negation
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= 0 (modulo x 5)) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
  
;; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= x 0)
                (cons "dan.jpg" (lambda () (f 1)))
                (cons "dog.jpg" (lambda () (f 0)))))])
    (lambda () (f 0))))

;; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (stream) (cons (cons 0 (car (stream)))
                               (lambda () (f (cdr (stream))))))])
    (lambda () (f s))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (l1 l2) (begin
                                (if (null? l1)
                                    (set! l1 xs) #t)
                                (if (null? l2)
                                    (set! l2 ys) #t)
                                (cons (cons (car l1) (car l2))
                                      (lambda () (f (cdr l1) (cdr l2))))))])
    (lambda () (f xs ys))))

;; Problem 9
(define (vector-assoc v vec)
  (let ([sz (vector-length vec)])
    (letrec ([f (lambda (i) (if (= i sz)
                                #f
                                (let ([item (vector-ref vec i)])
                                  (if (and (pair? item) (equal? (car item) v))
                                      item
                                      (f (+ i 1))))))])
(f 0))))


;; Problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
           [nume 0]
           [f (lambda (v) (let ([ans (vector-assoc v memo)])
                            (if ans
                                ans
                                (let ([new-ans (assoc v xs)])
                                  (if (not new-ans)
                                      #f
                                      (begin
                                        (vector-set! memo nume new-ans)
                                        (set! nume (modulo (+ nume 1) n))
                                        new-ans))))))])
    f ))
                            
;; Challenge problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e1res e1]
              [f (lambda () (let ([e2res e2])
                              (if (< e2res e1res)
                                  (f)
                                  #t)))])
       (f))]))
