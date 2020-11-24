#lang racket


;Problem_1
(display "\n ========= Problem_1_tests ========= \n")

(define (cartesian-product l1 l2)
  (define (helper l1 l-temp)
    (cond [(null? l1) '()]
          [(not (null? l-temp)) (cons (cons (car l1) (car l-temp)) (helper l1 (cdr l-temp)))]
          [else (helper (cdr l1) l2)]))
  (helper l1 l2))

(cartesian-product '(1 2) '(3 4)) ;test_1
(cartesian-product '(1 2 3 4 5) '(6 7 8)) ;test_2



;Problem_2
(display "\n ========= Problem_2_tests ========= \n")

(define (factorize n)
  (define (helper list i n)
    (cond [(= n 1) (reverse list)]
          [(= (remainder n i) 0) (helper (cons i list) 2 (/ n i))]
          [else (helper list (+ i 1) n)]))
  (helper '() 2 n))

(factorize 6) ; test_3
(factorize 13) ; test_4
(factorize 123) ; test_5
(factorize 152) ; test_6
(factorize 144) ; test_7

