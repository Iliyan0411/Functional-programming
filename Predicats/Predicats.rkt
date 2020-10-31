#lang racket

(define (fib n)
  (define (helper n a b)
  (cond [(or (= n 0)(= n 1)) b]
        [else (helper (- n 1) b (+ a b))]))
  (helper n 1 1))

(fib 100);

(define (maxdivisor x)
  (define (helper d x)
    (if (= (remainder x d) 0)
        d
        (helper (- d 1) x)))
  (helper (- x 1) x))

(maxdivisor 18);

(define (sum-odds a b)
  (define (helper sum a b)
    (cond [(> a b) sum]
          [(= (remainder a 2) 0) (helper sum (+ a 1) b)]
          [(not (= (remainder a 2) 0)) (helper (+ sum a) (+ a 2) b)]))
  (helper 0 a b))

(sum-odds 1 1);

(define (prime? n)
  (define (helper temp n)
    (cond [(= temp n) "It is prime."]
          [(= (remainder n temp) 0) "It is not prime."]
          [else (helper (+ temp 1) n)]))
  (helper 2 n))

(prime? 27);

(define (count-divisors n)
  (define (helper count d n)
    (cond [(= d 0) count]
          [(= (remainder n d) 0) (helper (+ count 1) (- d 1) n)]
          [else (helper count (- d 1) n)]))
  (helper 0 n n))

(count-divisors 9)
  
