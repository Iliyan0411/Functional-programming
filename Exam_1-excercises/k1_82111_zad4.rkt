#lang racket

(define (corr-pos? l count)
  (cond [(= count 0) #t]
        [(= (car l) 0) (corr-pos? (cdr l) (- count 1))]
        [else #f]))


(define (triangular? mat)
  (define (helper mat count corr-pos?)
    (cond [(null? mat) #t]
          [(corr-pos? (car mat) count) (helper (cdr mat) (+ count 1) corr-pos?)]
          [else #f]))
  (helper mat 0 corr-pos?))


(triangular? '((1 2 3)
               (0 5 6)
               (0 0 9))) ; -> #t

(triangular? '((0 2 3)
               (0 0 6)
               (1 0 0))) ; -> #f

(triangular? '((1 2 3)
               (1 5 6)
               (0 0 9))) ; -> #f

(triangular? '((1 2 3 4)
               (0 5 6 7)
               (0 0 8 9)
               (0 0 0 9))) ; -> #t
