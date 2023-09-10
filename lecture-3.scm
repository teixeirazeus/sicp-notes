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











