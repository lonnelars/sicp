(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-v2 n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product)
                   (+ counter 1))))
  (fact-iter 1 1))

;; exercise 1.9
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; recursive process

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; iterative process

;; exercise 1.10
;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)                                ;1024
(A 2 4)                                 ;65536
(A 3 3)                                 ;65536

(define (f n) (A 0 n))                  ;(f n) is 2*n
(define (g n) (A 1 n))                  ;(g n) is 2^n
(define (h n) (A 2 n))                  ;(h n) is a "power tower"; 2^2^...^2, n times. See http://mathworld.wolfram.com/AckermannFunction.html


