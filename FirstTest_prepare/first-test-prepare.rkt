#lang racket

;Problem 1
(define (to-low? n)
    (cond [(< n 10) #t]
          [(> (remainder n 10) (remainder (quotient n 10) 10)) #f]
          [else (to-low? (quotient n 10))]))

(define (sum-numbers a b)
  (cond [(> a b) 0]
        [(to-low? a) (+ a (sum-numbers (+ a 1) b))]
        [else (sum-numbers (+ a 1) b)]))