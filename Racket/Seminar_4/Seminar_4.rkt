#lang racket

;Zadacha_1
(define (count-digits n)
  (define (helper count k)
    (if (< k 10)
        count
        (helper (+ count 1) (/ k 10))))
  (helper 1 n))

(define (substr? a b)
    (cond [(or (> a b) (= b 0)) #f]
          [(= a (remainder b (expt 10 (count-digits a)))) #t]
          [else (substr? a (quotient b 10))]))

(substr? 123 4521234);

;Zadacha_2

(define (my-identity x) x); a)

(define (my-compose f g)
  (lambda (x) (f (g x))))

((my-compose (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 2)

