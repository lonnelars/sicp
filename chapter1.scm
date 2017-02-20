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

