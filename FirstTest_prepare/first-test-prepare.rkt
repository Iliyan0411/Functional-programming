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

;Problem 2
(define (bigger-elements el l)
  (define (helper counter l)
    (cond [(null? l) (list el counter)]
          [(< el (car l)) (helper (+ counter 1) (cdr l))]
          [else (helper counter (cdr l))]))
  (helper 0 l))

(define (num-bigger-elements l)
  (define (helper l result)
    (if (null? l)
        (reverse result)
        (helper (cdr l) (cons (bigger-elements (car l) l) result))))
  (helper l '()))

;Problem 4
(define (repeater str)
  (λ (count glue)
    (define (helper n result)
      (if (= n 0)
          result
          (helper (- n 1) (string-append result str glue))))
    (helper count "")))


;Problem 5
(define (kratna-sum? n d)
    (define (helper sum n)
      (cond [(and (= n 0) (= (remainder sum d) 0)) #t]
            [(> n 0) (helper (+ sum (remainder n 10)) (quotient n 10))]
            [else #f]))
    (helper 0 n))

(define (sum-digits a b k)
  (define (helper sum a)
    (cond [(> a b) sum]
          [(kratna-sum? a k) (helper (+ sum a) (+ a 1))]
          [else (helper sum (+ a 1))]))
  (helper 0 a))















