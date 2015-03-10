; SICP Exercise Solutions

; Ex. 1.1
10
(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
  b
  a
  )
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)
   )

; Ex. 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Ex. 1.3
(define (square x) (* x x))
(define (sum-of-larger-squares a b c)
  (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
        ((and (< b a) (< b c)) (+ (square a) (square c)))
        (else (+ (square a) (square b)))
        )
  )

; Ex. 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)
  )

; Ex. 1.5
; Applicative-order evaluation evaluates the procedure's arguments first.
; This would result in an error. Normal-order evaluation lazily evaluates
; arguments, so the procedures would yield 0.

; Ex. 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)
        )
  )

; Ex. 1.7
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )
(define (new-good-enough? guess x)
  (< (abs (- 1 (/ (improve guess x) guess) )) 0.01)
  )
(define (average x y)
  (/ (+ x y) 2)
  )
(define (improve guess x)
  (average guess (/ x guess))
  )
(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x
               )
    )
  )
(define (sqrt x)
  (sqrt-iter 1.0 x)
  )
(sqrt 100000000000000000000000000)

; Ex. 1.8
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )
(define (new-good-enough? guess x)
  (< (abs (- 1 (/ (improve guess x) guess) )) 0.01)
  )
(define (average x y)
  (/ (+ x y) 2)
  )
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
  )
(define (cbrt-iter guess x)
  (if (new-good-enough? guess x)
    guess
    (cbrt-iter (improve guess x)
               x
               )
    )
  )
(define (cbrt x)
  (cbrt-iter 1.0 x)
  )
(cbrt 125)

; Ex. 1.9
(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))
    )
  )
; recursive procedure and recursive process, procedure that calls itself,
; and evolves by growing then contracting.

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))
    )
  )
; recursive procedure that calls itself, iterative process, the program
; variables contain the complete state of the process

; Ex. 1.10 Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0)  (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2^n
(define (h n) (A 2 n)) ; 2^2^2...^2 (n times)

