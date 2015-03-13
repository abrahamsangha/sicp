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

; Example: Counting Change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (count-change 10) => (cc 10 5)
; amount = 10, kinds-of-coins = 5
; (cc 10 5) = (+ (cc 10 4) (cc (- 10 50) 5))
; (cc 10 4) = (+ (cc 10 3) (cc (- 10 25) 4))
; (cc 10 3) = (+ (cc 10 2) (cc (- 10 10) 3))
; (cc 10 2) = (+ (cc 10 1) (cc (- 10 5) 2))
; (cc 10 1) = (+ (cc 10 0) (cc (- 10 1) 1))

; Ex. 1.11
; f(n) = n if n < 3
;        f(n - 1) + 2f(n-2) +3f(n - 3) if n >= 3

; recursive process
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

; iterative process
; (f 4) => (+ f(3) 2f(2) 3f(1))
; (f 5) => (+ f(4) 2f(3) 3f(2))
; 0  1  2  3  4   5   6   7
; 0, 1, 2, 4, 11, 25, 59, 142,...
; (f 3) => 2 + 2*1 + 3*0 = 2 + 2 + 0
; (f 4) => 4 + 2*2 + 3*1 = 4 + 4 + 3
; (f 5) => 11 + 2*4 + 3*2= 11 + 8 + 6
; (f 6) => 25 + 2*11 + 3*4
; a = -1, b = 0, c = 1,      n = 2, f(n) = 2
; a = 0, b = 1, c = 2        n = 3, f(n) = 4
; a = 1, b = 2, c = 4        n = 4, f(n) = 11
; iter a b c n
;    if n < 3
;    n
;    iter b c c+2b+3a n-1

(define (f n)
  (if (< n 3)
      n
      (iter 0 1 2 n)))
(define (iter a b c n)
  (if (< n 3)
    c
    (iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))

; Ex. 1.12
;              1
;             1 1
;            1 2 1
;           1 3 3 1
;          1 4 6 4 1

(define (pascal row elem)
  (cond ((or (= elem 1)(= elem row)) 1)
        (else (+ (pascal (- row 1) (- elem 1)) (pascal (- row 1) elem)))
        )
  )
