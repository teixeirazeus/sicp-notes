(define (sum-int a b)
    (if (> a b)
        0
        (+ a (sum-int (+ a 1) b))))

(define (square x) (* x x))
(define (sum-cube a b)
    (if (> a b)
        0
        (+ (square a) (sum-cube (+ a 1) b))))

(define (pim-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pim-sum (+ a 4) b))))

;(define (<name> a b)
;    (if (> a b)
;        0
;        (+ (<term> a) (<name> (<next> a) b))))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(define (sum-it a b)
    (define (identity x) x)
    (sum identity a 1+ b))

(define (sum-sq a b)
    (sum square a 1+ b))

(define (pi-sum a b)
    (sum (lambda(i) (/ 1 (* i (+ 1 2))))
    a 
    (lambda(i) (+ a 4) b)))

(define (sum term a next)
    (define (iter j ans)
        (if (> j b)
        ans 
        (iter (next j)
            (+ (term j) ans))))
    (iter a 0))

(define (sqrt x)
    (define tolerance 0.00001)
    (define (good-enuf? y)
        (< (abs (- (* y y) x)) tolerance))
    (define (average a b)
        (/ (+ a b) 2))
    (define (improve y)
        (average (/ x y) y))
    (define (try y)
        (if (good-enuf? y)
            y
            (try (improve y))))
    (try 1))

(define (sqrt x)
    (fixed-point
        (lambda (y) (average (/ x y) y))
        1))

(define (fixed-point f start)
    (define tolerance 0.0001)
    (define (close-enuf? u v)
        (< (abs (- u v)) tolerance))
    (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
    (iter start (f start)))

(define (sqrt x)
    (fixed-point
        (average-damp (lambda(y) (/ x y)))
        1))

(define average-damp
    (lambda (f) 
        (lambda (x) (average (f x) x))))

(define (average-damp f)
        (lambda (x) (average (f x) x)))

(define (average-damp f)
    (define (foo x)
        (average (f x) x))
    foo)

(define (sqrt x)
    (newton (lambda (y) (- x (square y)))
            1))

(define (newton f guess)
    (define df (deriv f))
    (fixed-point
        (lambda (x) (- x (/ f x (df x))))
        guess))

(define deriv
    (lambda (f)
        (lambda (x)
            (/ (- (f (+ x dx))
                (f x))
                dx))))

; B
; 1/2 + 1/4 = 3/4
; 3/4 * 2/3 = 1/2
; n1/d1 + n2/d2 = n1d2+n2d1/d1d2
; n1/d1 * n2/d2 = n1n2/d1d2

(define (+rat x y)
    (make-rat
        (+ (* (numer x)(denom y))
            (* (number y)(denom y)))
        (* (denom x denom y))))

(define (*rat x y)
    (make-rat
        (* (numer x)(numer y))
        (* (denom x)(denom y))))

; (x+y) * (s+t)
(*rat (+rat x y) (+rat s t))

; list structure
; pair

(cons x y)
; constructs a pair whose first part
; is x and whose second part is y

(car p)
; selects the first part of the pair p

(cdr p)
; selects the second part of the pair p

(cons 2 3)
; [2, 3] box and pointer notation

(car (cons x y)); is x
(cdr (cons x y)); is y

(define (make-rat n d)
    (cons n d))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

; 1/2 + 1/4 = 3/4
(define a (make-rat 1 2))
(define b (make-rate 1 4))
(define ans (+ rat a b))
(numer ans); 6
(denom ans); 8
; 3/4 !== 6/8

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g)
                (/ d g))))

; defining +rat without data abstration
(define (+rat x y)
    (cons (+ (* (car x) (cdr y))
            (* (car y) (cdr x)))
        (* (cdr x) (cdr y))))

;pergunta: Tem um axima de fazer todo o design antes do código
;Esse axima é de uma pessoa que não parce ter feitos grandes sistemas
;Falam que ciência da computação é igual magia, 
;e essa é a parte boa da ciencia da computação
;tem uma parte que parece mais uma religião
;Em geral, eu acredito que pessoas que acham que você deve fazer
;o design de tudo antes de implementar. 
;Basicamante são pessoas que não
;fizeram de design de muitas coisas
;O verdadeiro poder é você fingir que fez a decisão 
;(abstração de software)
;e então depois, avaliar as decisões que você fez.
;Quando você fizer isso, terá o melhor dos dois mundos

; exemplo de definição local
(let (z 10)
    (+ z z)); 20

z; ?

; representing vectors in the plane
(define (make-vector x y) (cons x y))
(define (xcord p) (car p))
(define (ycord p) (cdr p))

; representing line segments
; p----q
(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

(define (midpoint s)
    (let ((a (seg-start s))
        (b (seg-end s)))
        (make-vector
        (average (xcor a) (xcor b))
        (average (ycor a) (ycor b)))))

(define (length s)
    (let
    ((dx (- (xcor (seg-end s))
            (xcor (seg-start s))))
    (dy (- (ycor (seg-end s))
            (ycor (seg-start s)))))
    (sqrt (+ (square dx)
            (square dy)))))

; cons, car, cdr definition
(define (cons a b)
    (lambda (pick)
        (cond  ((= pick 1) a)
                ((= pick 2) b))))
(define (car x) (x 1))
(define (cdr x) (x 2))
