#lang racket

;Problem_1
"Examples from Problem_1:"

(define (count-digits n) ; funkcia za broene na cifrite v dadeno chislo
  (define (helper k counter)
    (if (< k 10)
        counter
        (helper (/ k 10) (+ counter 1))))
  (helper n 1))

(define (automorphic? n) ; iskanata ot zadacha 1 funkcia
  (= n (remainder (* n n) (expt 10 (count-digits n)))))

(automorphic? 5); example_1
(automorphic? 25); example_2
(automorphic? 36); example_3
(automorphic? 1); example_4
(automorphic? 890625); example_5
(automorphic? 6); example_6


;Problem_2
"Examples from Problem_2:"

(define (cube-dif m k) ; namira razlikata na dve chisla povdignati na treta stepen
  (- (expt m 3) (expt k 3)))

(define (prime? n) ; proverqva dali dadeno chislo e prosto
  (define (helper d)
    (cond [(= d 1) #t]
          [(= (remainder n d) 0) #f]
          [else (helper (- d 1))]))
  (helper (- n 1)))

(define (nth-cuban n) ; iskanata ot zadacha 2 funkcia
  (define (find m k counter)
    (cond [(= counter 0) (cube-dif (- m 1) (- k 1))]
          [(prime? (cube-dif m k)) (find (+ m 1) (+ k 1) (- counter 1))]
          [else (find (+ m 1) (+ k 1) counter)]))
  (find 2 1 n))

(nth-cuban 1); example_1
(nth-cuban 2); example_2
(nth-cuban 4); example_3
(nth-cuban 50); example_4
(nth-cuban 100); example_5


