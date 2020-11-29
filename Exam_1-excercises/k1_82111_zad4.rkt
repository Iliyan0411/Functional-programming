#lang racket

(define (lenght l)
  (if (null? l)
      0
      (+ 1 (lenght (cdr l)))))


(define (triangular? mat)
  (define (helper mat count)
    (cond [(null? mat) #t]
          [(= (lenght (filter (λ (x) (= x 0)) (take (car mat) count))) count) (helper (cdr mat) (+ count 1))]
          [else #f]))
  (helper mat 0))



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
