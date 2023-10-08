; pattern matching

; foo matches excakly foo
(f a b); matches a list whos first element is f
    ; second '' is a a
    ; third ' is a b
(? x); matches anything, call it x
(?c x); matches constant, call it x
(?v x); match a varible, call it x

;skeletons
;foo instanciate itself
(f a b); instanciate to a 3-list
    ; result of instanciating each of
    ;f, a, b
(: x); instanciate to the value of x
    ;in the match pattern

(define algebra-rules *(
    ( ((? op) (?c e1) (?c e2))
    (: (op e1 e2))  )

    ( ((? op) (? e1) (?c e2))
        ((: op) (: e2) (: e1))  )
    ( (+ 0 (? e))   (: e))
    ( (* 1 (? e))   (: e))
    ( (* 0 (? e))   0)
))

;match
(+ (* (?x) (?y)) (?y))
(+ (* 3 x) x)
;x=3 and y=x

((atom? pat)
    (if (atom? exp)
        (if (eq? pat exp)
            dict 
            'failed)
        'failed))

(define (simplifier the-rules)
    (define (simplify-exp exp)
        ***)
    (define (simplify-parts exp)
        ***)
    (define (try-rules exp)
        ***)
    simplify-exp)

(define (scan rules)
    (if (null? rules)
        exp
        (let ((dict 
            (match (pattern (car rules))
                exp 
                (empty-dictionary))))
        (if (eq? dict 'failed)
            (scan (cdr rules))
            (simplify-exp
                (instantiate
                    (skeleton (car rules))
                    dict))))))

;B
;Data abstration

;complex numbers
(define (+c z1 z2) ...)
(define (-c z1 z2) ...)
(define (*c z1 z2) ...)
(define (/c z1 z2) ...)

(define (+c z1 z2)
    (make-rectangular
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
    (make-rectangular
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
    (make-polar
    (* (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

(define (/c z1 z2)
    (make-polar
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

;represeting complex number
(define (make-rectangular x y)
    (cons x y))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

