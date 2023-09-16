(define (+vect v1 v2)
    (make-vector
        (+ (xcor v1) (xcor v2))
        (+ (ycord v1 (ycord v2)))))

(define (scale s v)
    (make-vector (* s (xcor v))
                (* s (ycor v))))

(define make-vector cons)
(define xcor car)
(define ycor cdr)

;(2,3) -> (5,1)
(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(make-seg (make-vector 2 3)
            (make-vector 5 1))

;closure

;list
(cons 1
    (cons 2
        (const 3
            (const 4 nil))))

(list (1 2 3 4))

(define 1-to-4 (list 1 2 3 4))

(car (cdr 1-to-4)); 2
(car (cdr (cdr 1-to-4))); 3

(scale-list 10 1-to-4); (10 20 30 40)

(define (scale-list s 1)
    (if (null? 1)
    nil
    (cons (* (car 1) s)
    (scale-list s (cdr 1)))))

(define (scale-list s 1)
    (map (lambda (item) (* item s)) 1))

(map square 1-to-4);(1 4 9 16)
(map (lambda(x) (+ x 10)) 1-to-4); (11 14 19 26)

; dont create list, just apply function
(define (for-each proc list)
    (cond ((null? list) "done")
        (else (proc (car list))
            (for-each proc
                (cdr list)))))

(define (make-picture seglist)
    (lambda (rect)
        (for-each 
            (lambda (s)
                (drawline
                ((coord-map rect) (seg-start s))
                ((coord-map rect) (seg-end s))))
                seglist)))

(define r (make-rect ....))
(define g (make-picture ....))
(g r); draw g inside r

(define (beside p1 p2 a)
    (lambda (rect)
        (p1 (make-rect
            (origin rect)
            (scale a (horiz rect))
            (vert rect)))
        (p2 (make-rect
            (+vect (origin rect)
                (scale a (horiz rect)))
            (scale (- 1 a) (horiz rect))
            (vert rect)))))

(define (rotate90 pict)
    (lambda (rect)
        (pict (make-rect
            (*vect (origin react)
                    (horiz rect))
            (vert rect)
            (scale -1 (horiz rect))))))

(define (right-push p n a)
    (if (= n 0)
        p
        (beside p (right-push p (- n 1) a)
        a)))

; construção de sistemas robustos
; um pequena variação no problema, 
; deve causar uma pequena variação na solução
; resolva problemas pequenos, resolvendo uma classe de problemas
; criando uma abstração para você se expressar

(define (deriv exp var)
    (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
        ((product? exp) (make-sum
                            (make-product (multiplier exp)
                                            (deriv (multiplicand exp) var))
                            (make-product (deriv (multiplier exp) var)
                                            (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (constant? exp var)
    (and (atom? exp) (not (eq? exp var))))

(define (same-var? exp var)
    (and (atom? exp) (eq? exp var)))

(define (sum? exp)
    (and (not (atom? exp))
    (eq (car exp) '+)))

(define (make-sum a1 a2)
    (list '+ a1 a2))

(define a1 cadr)
(define a2 caddr)

(define (product? exp)
    (and (not (atom? exp))
    (eq? (car exp) '*)))

(define (makeproduct m1 m2)
    (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)

(define (make-sum a1 a2)
    (cond ((and (number? a1)
            (number? a2))
            (+ a1 a2))
            ((and (number? a1) (= a1 0))
            a2 
            ((and (number? a2) (= a2 0))
            a1)
            (else (list '+ a1 a2)))))
