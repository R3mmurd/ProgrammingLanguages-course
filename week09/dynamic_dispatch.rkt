#lang racket

(struct obj (fields methods))

(define (assoc-m v xs)
  (cond [(null? xs) #f]
        [(equal? v (mcar (car xs))) (car xs)]
        [#t (assoc-m v (cdr xs))]))

(define (get obj fld)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
        (mcdr pr)
        (error "field not found"))))

(define (set obj fld v)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
        (set-mcdr! pr v)
        (error "field not found"))))

(define (send obj msg . args)
  (let ([pr (assoc-m msg (obj-methods obj))])
    (if pr
        ((mcdr pr) obj args)
        (error "method not found" msg))))

(define (make_point _x _y)
  (obj
   (list (mcons 'x _x)
         (mcons 'y _y))
   (list (mcons 'get-x (lambda (self args) (get self 'x)))
         (mcons 'get-y (lambda (self args) (get self 'y)))
         (mcons 'set-x (lambda (self args) (set self 'x (car args))))
         (mcons 'set-y (lambda (self args) (set self 'y (car args))))
         (mcons 'distToOrigin
                (lambda (self args)
                  (let ([a (send self 'get-x)]
                        [b (send self 'get-y)])
                    (sqrt (+ (* a a) (* b b)))))))))

(define (make_color_point _x _y _c)
  (let ([pt (make_point _x _y)])
    (obj
     (cons (mcons 'color _c)
           (obj-fields pt))
     (append (list
              (mcons 'get-color (lambda (self args) (get self 'color)))
              (mcons 'set-color (lambda (self args) (set self 'color (car args)))))
              (obj-methods pt)))))

(define (make_polar_point _r _th)
  (let ([pt (make_point 0 0)])
    (obj
     (list (mcons 'r _r)
           (mcons 'theta _th))
     (append
      (list (mcons 'set-r-theta
                   (lambda (self args)
                     (begin
                       (set self 'r (car args))
                       (set self 'theta (cadr args)))))
            (mcons 'get-x
                   (lambda (self args)
                     (let ([a (get self 'r)]
                           [b (get self 'theta)])
                       (* a (cos b)))))
            (mcons 'get-y
                   (lambda (self args)
                     (let ([a (get self 'r)]
                           [b (get self 'theta)])
                       (* a (sin b)))))
            (mcons 'set-x
                   (lambda (self args)
                     (let* ([a (car args)]
                            [b (send self 'get-y)]
                            [theta (atan (/ b a))]
                            [r (sqrt (+ (* a a) (* b b)))])
                       (send self 'set-r-theta r theta))))
            (mcons 'set-y
                   (lambda (self args)
                     (let* ([b (car args)]
                            [a (send self 'get-y)]
                            [theta (atan (/ b a))]
                            [r (sqrt (+ (* a a) (* b b)))])
                       (send self 'set-r-theta r theta)))))
            (obj-methods pt)))))
  
  
(define p1 (make_point -4 0))
(send p1 'get-x)
(send p1 'get-y)
(send p1 'distToOrigin)
(send p1 'set-y 3)
(send p1 'distToOrigin)

(define p2 (make_color_point -4 0 "red"))
(send p2 'get-x)
(send p2 'get-y)
(send p2 'distToOrigin)
(send p2 'set-y 3)
(send p2 'distToOrigin)

(define p3 (make_polar_point 4 pi))
(send p3 'get-x)
(send p3 'get-y)
(send p3 'distToOrigin)
(send p2 'set-y 3)
(send p2 'distToOrigin)
   