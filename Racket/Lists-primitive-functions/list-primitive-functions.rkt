#lang racket

;Tochkova dvoika?
(pair? '(1.2))

;List?
(list? '(1 2 3))

;number?
(number? 5)

;string?
(string? "abc")

;symbol?
(symbol? "a")

;list create
(list 1 2 3 4)

;(car (cdr '(1 2 3 4)))
(cadr '(1 2 3 4))

;(car (cdr (cdr '(1 2 3 4))))
(caddr '(1 2 3 4))

;concat elements/lists in one list
(append '(1 2) '(3 4))


;reverse list
(reverse '(1 2 3 4))

; Ако s1 & s2 са един и същи томни символи -> #t
(eq? 5 5)
(eq? '(1 2) '(1 2))

; Ако обектите са еквиавалентни -> #t
(equal? '(1 2 3) '(1 2 3))
(equal? "a" "a")

;Връща дали някой елемент се съдържа в списък в смисъла на eq?
;(memq 5 '(1 2 3 4 5 6)) --> '(5 6)
;(memq 8 '(1 2 3 4)) --> #f
;(memq '(a b) '((a b) c d)) --> #f

;ръща дали някой елемент се съдържа в списък в смисъла на equal?
(member 6 '(1 2 3 4 5 6 7 8))
(member '(1 2) '((1 2) 3 5))










