;; 1.2 Procedures and the processes they create

;; recursive process
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; iterative process
(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
	product
	(fact-iter (* counter product)
		   (+ counter 1)
		   max-count)))
  (fact-iter 1 1 n))

;; exercise 1.9

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))
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
;; this is a recursive process

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; this is an iterative process

;; 1.2.2 Tree recursion

(define (fib1 n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	((else (+ (fib (- n 1))
		  (fib (- n 2)))))))

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; Example: counting change

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

(count-change 100)

;; Exercise 1.11

(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

(define (f-iter n)
  (define (loop a b c n)
    (if (= n 0)
	c
	(loop (+ (* 3 c) (* 2 b) a)
	      a
	      b
	      (- n 1))))
  (loop 2 1 0 n))
