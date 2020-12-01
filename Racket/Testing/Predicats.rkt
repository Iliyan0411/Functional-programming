#lang racket

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))


(define (f x) (+ (* x x)  (* 3 x)))


(define f-first
  (derive f 0.0001))


(define (derive-2 f eps)
  (derive (derive f eps) eps))


(define f-second
  (derive-2 f 0.00001))


(define (derive-n f n eps)
  (if (= n 0)
      f
      (derive (derive-n f (- n 1) eps) eps)))
  

(define (repeated f n)
  (λ (x)
    (if (= n 0)
        x
        (f ((repeated f (- n 1)) x)))))

(define (compose f g)
  (λ (x) (f (g x))))

((compose (λ (x) x) (λ (x) (* 2 x))) 1)


