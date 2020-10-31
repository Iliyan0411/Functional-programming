#lang racket


;Problem_1

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (f x) (* 2 x x))

(define (derive2 g eps)
  (derive (derive g eps) eps))

(define g-s (derive2 f 0.000001))

(g-s 5);

;Problem_2

(define (g x) (* x x))

(define (derive-n f n eps)
  (if (> n 0)
      (derive-n (derive f eps) (- n 1) eps)
      f))

(define f-n (derive-n g 2 0.0001))

(f-n 3);




