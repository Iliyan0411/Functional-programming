#lang racket



(define (deep-delete l)
  (define (helper level l)
    (cond [(null? l) '()]
          [(pair? (car l)) (cons (helper (+ level 1) (car l)) (helper level (cdr l)))]
          [(< (car l) level) (helper level (cdr l))]
          [else (cons (car l) (helper level (cdr l)))]))
  (helper 1 l))

(deep-delete '(1 (2 (2 4) 1) 0 (3 (1))))


(define (lenght l)
  (if (null? l)
      0
      (+ 1 (lenght (cdr l)))))


(define (has-matching-lenghts l1 l2)
  (define div (- (lenght (car l1)) (lenght (car l2))))
  (define (helper d l1 l2)
    (cond [(and (null? l1) (null? l2)) #t]
          [(not (= (- (lenght (car l1)) (lenght (car l2))) d)) #f]
          [else (helper d (cdr l1) (cdr l2))]))
  (helper div (cdr l1) (cdr l2)))

(has-matching-lenghts '((1 2) (3 4 5) (6 7)) '((1) (3 4) (6)))





