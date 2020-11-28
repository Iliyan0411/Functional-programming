#lang racket


(define (kth-max-min xs)
  (λ (k)
    (define (helper sorted-xs count prev)
      (cond [(null? sorted-xs) "No such number!"]
            [(and (= count k) (< (car sorted-xs) 0) (not (= (car sorted-xs) prev))) (car sorted-xs)]
            [(and (< (car sorted-xs) 0) (not (= (car sorted-xs) prev))) (helper (cdr sorted-xs) (+ count 1) (car sorted-xs))]
            [else (helper (cdr sorted-xs) count prev)]))
    (helper (sort xs >) 1 1)))


((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) ; -> -2
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; -> No such number
((kth-max-min '(5 6 3 -4 -8 2 3 -1 5 5)) 2) ; -> -4