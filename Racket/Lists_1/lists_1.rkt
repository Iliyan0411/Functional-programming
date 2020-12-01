#lang racket

;Problem 1
(define (erase-all lst x)
  (cond [(null? lst) '()]
        [(= x (car lst)) (erase-all (cdr lst) x)]
        [else (cons (car lst) (erase-all (cdr lst) x))]))


(define (removeDuplicates lst)
  (if (null? lst)
      '()
      (cons (car lst) (removeDuplicates (erase-all (cdr lst) (car lst))))))


;Problem 2
(define (sublistBetween start end xs)
  (take (drop xs start) (+ (- end start) 1)))


;Problem 3
(define (lenght xs)
  (define (helper xs n)
    (if (null? xs)
        n
        (helper (cdr xs) (+ n 1))))
  (helper xs 0))

(define (countOccurencess subxs xs)
  (define (helper n xs)
    (cond [(or (null? xs) (> (lenght subxs) (lenght xs))) n]
          [(equal? subxs (take (drop xs 0) (lenght subxs))) (helper (+ n 1) (cdr xs))]
          [else (helper n (cdr xs))]))
  (helper 0 xs))

;Problem 4
(define (ordered? xs pred)
  (cond [(null? (cdr xs)) #t]
        [(pred (car xs) (cadr xs)) (ordered? (cdr xs) pred)]
        [else #f]))

;Problem 5

;;????


;Problem 6

(define (flatten xs)
  (cond [(null? xs) '()]
        [(list? (car xs)) (flatten (append (car xs) (cdr xs)))]
        [else (cons (car xs) (flatten (cdr xs)))]))

;(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))


;Problem 7

;;???




























