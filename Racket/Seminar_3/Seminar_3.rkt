#lang racket

(define (f x y)
  (λ (a b)
    (+ (* x a a) (* y b))
    (+ 1 (* x y))
    (+ x y)))

(f 1 2)

