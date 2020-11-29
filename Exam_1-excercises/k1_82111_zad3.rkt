#lang racket


(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))


(define (shuffle xs)
  (define lenght (len xs))
   (define (helper l1 l2)
    (cond [(not (= (remainder lenght 2) 0)) "Invalid list!"]
          [(null? l1) '()]
          [else (cons (car l1) (cons (car l2) (helper (cdr l1) (cdr l2))))]))
  (helper (take xs (/ lenght 2)) (drop xs (/ lenght 2))))


(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)