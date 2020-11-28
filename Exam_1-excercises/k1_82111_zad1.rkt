#lang racket


(define (count-numbers n)
  (if (= n 0)
      0
      (+ 1 (count-numbers (quotient n 10)))))

(define (good-number res n)
  (define (helper k)
    (cond [(> (* n k) res) -1]
          [(< (* n k) res) (helper (+ k 1))]
          [else k]))
  (helper 1))


(define (dig-pow n p)
  (define (helper num-digits t res)
    (if (= t 0)
        (good-number res n)
        (helper (- num-digits 1) (quotient t 10) (+ res (expt (remainder t 10) (+ p num-digits))))))
  (helper (- (count-numbers n) 1) n 0))


(dig-pow 89 1)
(dig-pow 92 1)
(dig-pow 695 2)
(dig-pow 46288 3)





