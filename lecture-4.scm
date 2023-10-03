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