;; 1.1 The elements of programming

;; 1.1.1 Expressions

(+ (* 3
      (+ (* 2 4)
	 (+ 3 5)))
   (+ (- 10 7)
      6))

;; 1.1.2 Naming and the environment

(define size 2)
size
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumference (* 2 pi radius))
circumference

;; 1.1.4 Compound Procedures

(define (square x) (* x x))
(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;; 1.1.6 Conditional expressions and predicates

(define  (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (abs_ x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs__ x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (alt_>= x y)
  (not (< x y)))

;; Exercise 1.1

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
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; Exercise 1.2

(/ (+ 5
      4
      (- 2
	 (- 3
	    (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3

(define (g a b c)
  (if (> a b)
      (if (> b c)
	  (sum-of-squares a b)
	  (sum-of-squares a c))
      (if (> a c)
	  (sum-of-squares a b)
	  (sum-of-squares b c))))
(g 1 2 3)
(g 6 5 4)
(g 1 1 1)

;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; If (> b 0) then the operator is + and the result is (+ a b), else the operator is - and the result is (- a b).
;; The function adds a with the absolute value of b.

;; Exercise 1.5

;; Applicative-order evaluation results in:
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; ...
;; an infinite loop.

;; Normal-order evaluation results in:

;; (test 0 (p))
;; (if (= 0 0)
;;     0
;;     (p))
;; 0

;; Example: Square roots by Newton's method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (new-sqrt-iter (improve guess x)
			 x)))
;; (new-sqrt-iter 1.0 9) ;Aborting!: maximum recursion depth exceeded

;; new-if is not a special form, so it uses applicative-order
;; application, and evaluates both the then-clause and the else-clause
;; every time. Thus we get an infinite recursion.

;; Exercise 1.7

(sqrt 0.0001) ;Value: .03230844833048122
;; 0.001 is very large compared to 0.0001, thus the good-enough? check fails.

;; Exercise 1.8

(define (cube-improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (cube-good-enough? guess x)
  (< (/ (abs (- guess
		(cube-improve guess x)))
	guess)
     (/ 1 1000000)))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x) x)))

(define (cube-root x)
  (exact->inexact (cube-root-iter 1 x)))

(cube-root 8)

;; 1.1.8 Procedures as black box abstractions

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
