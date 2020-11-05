#lang racket


;Problem 1:

(define (contain? list x)
  (cond [(null? list) #f]
        [(= x (car list)) #t]
        [else (contain? (cdr list) x)]))


;Problem 2:

(define (add-at list x index)
  (if (= index 0)
      (cons x list)
      (cons (car list) (add-at (cdr list) x (- index 1)))))


;Problem 3:

(define (find-min lst)
  (define (helper min lst)
    (cond [(null? lst) min]
          [(< (car lst) min) (helper (car lst) (cdr lst))]
          [else (helper min (cdr lst))]))
  (helper (car lst) (cdr lst)))


;Problem 4:

(define (delete-el x lst)
  (cond [(null? lst) `()]
        [(= x (car lst)) (cdr lst)]
        [else (cons (car lst) (delete-el x (cdr lst)))]))


;Problem 5:

(define (delete-all x lst)
  (cond [(null? lst) '()]
        [(= x (car lst)) (delete-all x (cdr lst))]
        [else (cons (car lst) (delete-all x (cdr lst)))]))


;Problem 6:

(define (concat lst1 lst2)
(if (null? lst1)
lst2
(cons (car lst1) (concat (cdr lst1) lst2))))


;Problem 7:

(define (reverse lst)
  (define (helper lst rlst)
    (if (null? lst)
        rlst
        (helper (cdr lst) (cons (car lst) rlst))))
  (helper lst '()))


;List-size

(define (size l)
  (define (helper i l)
    (if (null? l)
        i
        (helper (+ i 1) (cdr l))))
  (helper 0 l))







