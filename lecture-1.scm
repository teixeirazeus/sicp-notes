; A
(+ 2 3)

; definição de função
(define (square x) (* x x))

; definição de função com lambda
; a mesma coisa da anterior
(define square (lambda (x) (* x x)))
(square 10)


(define (average x y) (/ (+ x y) 2))
(define (mean-square x y) (average (square x) (square y)))
(mean-square 3 4)


;def abs_val(x):
;    if x < 0:
;        return -x
;    elif x == 0:
;        return 0
;    else:
;        return x

(define (abs x) (cond ((< x 0) (- x))
                        ((= x 0) 0)
                        ((> x 0) x)))

(define (abs x) 
    (if (< x 0) 
        (- x) 
        x))

; to find an approximation to sqrt(x)
; make a guess G
; improve the guess by averaging G and X/G
; keep improving the guess until it is good enough
; use 1 as the initial guess
(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (try guess x)
    (if (good-enough? guess x)
        guess
        (try (improve guess x) x)))

(define (sqrt x) (try 1.0 x))

; com definições internas
; block structure
(define (sqrt x)
    (define (improve guess x)
        (average guess (/ x guess)))

    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))

    (define (try guess x)
        (if (good-enough? guess x)
            guess
            (try (improve guess x) x)))
    (try 1.0 x))

; B
(define (square x)
    (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))
; (sum-of-squares 3 4) -> 25

; kinds of expressions
; numbers
; symbols
; lambda-expressions
; definitions
; conditionals
; combinations

; iterativo
; time O(x)
; space O(1)
(define (+ x y
    (if (= x 0)
        y
        (+ (- x 1) (+ 1 y)))))

; recursivo
; time O(n)
; space O(n)
(define (+ x y
    (if (= x 0)
        y
        (+ (+ (- x 1) y) 1)
        )))

; exemplo equação diferencial
(define (circle x y)
    (plot x y)
    (circle (- x (* y dt)
            (+ y (* x dt)))))

; time O(fib (n))
(define (fib n)
    if (< n 2)
        n 
        (+ (fib (- n 1)) 
            (fib (- n 2))))

; hanoi
(define (move n from to spare)
    (cond ((= n 0) "Done")
        (else 
            move (- n 1) from spare to)
        (print-move from to)
        (move (- n 1) spare to from)))

;(move 4 1 3 2)
;(move 3 1 3 2)
;(move 2 1 2 3)
;(move 1 1 3 2)
