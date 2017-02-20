(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (alternative-abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs-with-if x)
  (if (< x 0)
      (- x)
      x))

;; Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))
   )

;; Exercise 1.3
(define (exercise-1-3 x y z)
  (if (> x y)
      (if (> y z)
          (sum-of-squares x y)
          (sum-of-squares x z))
      (if (> x z)
          (sum-of-squares x y)
          (sum-of-squares y z))))

;; Exercise 1.4
;; adds a and the absolute value of b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5
;; applicative order
;; (test 0 (p))
;; (test 0 (p)) <- infinite loop
;;
;; normal order
;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t 0 (p))
;; 0

;; 1.1.7 Newton's method

(define (sqrt-iter guess x)
  (if (improved-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6
;; new-if will always evaluate both its arguments, even if (good-enough? guess x) is true => infinite loop.

;; Exercise 1.7

;; will terminate because of computer's limited precision
(define (improved-good-enough? guess x)
  (= guess (improve guess x)))
