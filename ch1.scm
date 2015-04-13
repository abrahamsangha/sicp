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

; Ex 1.13

; sqrt procedure
(define (new-good-enough? guess x)
  (< (abs (- 1 (/ (improve guess x) guess) )) 0.001)
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
;
; exp proc
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;
; fib procedure
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;
; phi proc
(define phi
  (/ (+ 1 (sqrt 5)) 2)
  )
;
;psi proc
(define psi
  (/ (- 1 (sqrt 5)) 2)
  )
;
;fib approximation proc
(define (fibx n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5))
  )
;
; closest integer proc
(define (close n approx)
  (< (abs (- (/ approx n) 1)) 0.01)
  )

; Ex 1.14
;
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

; 11 cents
; (count-change 11)
; (cc 11 5)
; (cc 11 4) + (cc -39 5)
; (cc 11 3) + (cc -14 4)
; (cc 11 2) + (cc 1 3)
; (cc 11 1) + (cc 6 2) + (cc 1 2) + (cc -4 2)
;
; tldr...the number of steps grow linearly O(n) but the space grows O(n^5)
; it's a binary recursive process, and when n is >50,
; all 5 coins are in play. For n>1, the space is O(n). For n>5, the space
; becomes O(n^2), since the kinds of coins is 2 and the tree will have 2 trees
; O(n) trees. For n>10, the tree will have 3 O(n) subtrees, or O(n^3) space.
; As the kinds of coins in play
; grows, the space grows by a factor of n. So the max order of space growth is
; O(n^5).

; Ex. 1.16
; fast-expt is a linear iterative process
; faster-expt is too, but with successive squaring
; the key is to keep a * b ^ n invariant
(define (fast-expt b n)
  (expt-iter b n 1))
(define (expt-iter b n a)
  (if (= n 0)
    a
    (expt-iter b (- n 1) (* a b))
    )
  )

(define (faster-expt b n)
  (expt-iter2 b n 1))
(define (expt-iter2 b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter2 b (- n 2) (* a b b)))
        (else (* b (expt-iter2 b (- n 1) a)))
        )
  )

; correct solution, returns a with correct value
; one key was to resist mutating a when n was even
(define (faster-expt2 b n)
  (expt-iter3 b n 1))
(define (expt-iter3 b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter3 (* b b) (/ n 2) a))
        (else (expt-iter3 b (- n 1) (* a b)))
        )
  )

; Ex 1.17
(define (times a b)
  (if (= b 0)
    0
    (+ a (times a (- b 1)))))

(define (double n)
  (* n 2))
(define (halve n)
  (if (even? n)
    (/ n 2)))
(define (fast-times a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-times (double a) (halve b)))
        (else (+ a (fast-times a (- b 1))))))

; Ex 1.18
; why doesn't the following work for the last line?
; (else (times-iter b (- n 1) b))))

(define (double n)
  (* n 2))
(define (halve n)
  (if (even? n)
    (/ n 2)))
(define (faster-times b n)
  (times-iter b n 0)
  )
(define (times-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (times-iter (double b) (halve n) a))
        (else (times-iter b (- n 1) (+ b a)))))

; Ex 1.19
; substitution to reach logarithmic Fibonacci procedure

; Ex 1.20
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
; Normal-order evaluation (substitution)
; expand, then reduce
;
; Applicative-order evaluation
;
; (gcd 206 40)
; (remainder 206 40) = 6
; (gcd 40 6)
; (remainder 40 6) = 4
; (gcd 6 4)
; (remainder 6 4) = 2
; (gcd 4 2)
; (remainder 4 2) = 0
; (gcd 2 0) = 2
;
; remainder is called 4 times

; Ex 1.21
;
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
; (smallest-divisor 199) = 199
; (smallest-divisor 1999) = 1999
; (smallest-divisor 19999) = 7
;

; Ex 1.22
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes start end)
  (if (even? start)
    (search-for-primes (+ start 1) end)
    (cond ((< start end) (timed-prime-test start) (search-for-primes (+ start 1) end)))))
; first thought was a procedure that found 3 primes within given range, but that
; proved too difficult. The above will work with trial and error.
; Three smallest primes above 1000 (with their times):
; 1009 1
; 1013 2
; 1019 2
; Three smallest primes above 10000 (with their times):
; 10007 7
; 10009 7
; 10037 7
; Three smallest primes above 100000 (with their times):
; 100003 17
; 100019 17
; 100043 17
; Since the order of growth for prime? is n^(1/2), we'd expect 10^(1/2) growth ~ 3.16.
; 7/2 is close, but 17/7 is not. We probably need to test larger values, but
; I'm already sick of this exercise

; Ex 1.23
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes start end)
  (if (even? start)
    (search-for-primes (+ start 1) end)
    (cond ((< start end) (timed-prime-test start) (search-for-primes (+ start 1) end)))))
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

; Three smallest primes above 1000 (with their times):
; 1009 2
; 1013 2
; 1019 2
; Three smallest primes above 10000 (with their times):
; 10007 4
; 10009 4
; 10037 5
; Three smallest primes above 100000 (with their times):
; 100003 10
; 100019 10
; 100043 10
; Now the order of growth is about 2, not exactly half of 3.16.
; This seems to be due to the if n = 2 step in next.

; Ex 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)  (fast-prime? n (- times 1)))
        (else false)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes start end)
  (if (even? start)
    (search-for-primes (+ start 1) end)
    (cond ((< start end) (timed-prime-test start) (search-for-primes (+ start 1) end)))))

; Three smallest primes above 1000 (with their times):
; 1009 176
; 1013 186
; 1019 191
; Three smallest primes above 10000 (with their times):
; 10007 167
; 10009 162
; 10037 228
; Three smallest primes above 100000 (with their times):
; 100003 192
; 100019 199
; 100043 199
;
; 1000003 223
; 1000033 225
; 1000037 229
;
; Since we are using fast-prime?, which uses the Fermat method and has a O(log n),
; the times for n near 1 million are around 0.8 greater than those around 1,000.

; Ex 1.25
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)  (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (expmod base exp m)
    (remainder (fast-expt base exp) m))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; Alyssa is wrong because the original expmod procedure reduces the exponent with
; successive squaring...
; http://www.billthelizard.com/2010/02/sicp-exercise-125-closer-look-at-expmod.html


; Ex 1.26
; Louis Reasoner is calling expmod twice, which creates two subtrees, increasing the number of steps to grow at O(n).

; Ex 1.29

(define (simpson f a b n)
  (define (inc x) (+ x 1))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((odd? k) (* 4 (y k)))
          ((even? k) (* 2 (y k)))))
  (* (/ h 3) (sum term a inc n))
  )
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (cube x) (* x x x))
; (simpson cube 0 1 100.0) = 0.24999999999
; (simpson cube 0 1 1000.0) = 0.2500000000

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (integral f a b dx)
  (define (add-dx x)  (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
            dx))

; Ex 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0)
  )
(define (simpson f a b n)
  (define (inc x) (+ x 1))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((odd? k) (* 4 (y k)))
          ((even? k) (* 2 (y k)))))
  (* (/ h 3) (sum term a inc n))
  )
(define (cube x) (* x x x))
