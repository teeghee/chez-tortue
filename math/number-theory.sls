#!r6rs

;; number-theory
;;
;; --- a chez scheme r6rs module for some elementary number theory
;; manipulations.



(library (math number-theory)

  (export
   Jacobi
   prime? next-prime previous-prime
   factorise)

  (import (rnrs base)
	  (only (rnrs lists)
		exists member)
	  (only (rnrs control)
		case-lambda)
	  (only (chezscheme)
		expt-mod iota nonnegative?))

  ;; Vector of primes less than 100
  (define small-primes
    '(2 3 5 7 11 13 17
	19 23 29 31 37 41
	43 47 53 59 61 67
	71 73 79 83 89 97))

  ;; (small-trial-division-prime? n)
  ;; 
  ;; Tests whether the given number n is divisible by one of the
  ;; primes in 'small-primes'.

  (define small-trial-division-prime?
    (case-lambda
      ((n)
       (cond ((< n 0)
	      (small-trial-division-prime? (- 0 n)))
	     ((<= n 1)
	      #f)
	     ((<= n 100)
	      (member n small-primes))
	     (else
	      (small-trial-division-prime? n small-primes))))
      ((n lst)
       (cond ((null? lst)
	      #t)
	     ((zero? (mod n (car lst)))
	      #f)
	     (else
	      (small-trial-division-prime? n (cdr lst)))))))

  ;; (exact-order n d)
  ;;
  ;; Returns the smallest integer k such that d^k divides n but
  ;; d^(k+1) does not.
  
  (define exact-order
    (lambda (n d)
      (assert (integer? n))
      (assert (integer? d))
      (assert (not (zero? d)))
      (assert (not (= (abs d) 1)))
      (cond ((< n 0) (exact-order (- 0 n) d))
	    ((zero? n) +inf.0)
	    ((< d 0) (exact-order n (- 0 d)))
	    ((> d n) 0)
	    (else
	     (let loop ((q (div n d))
			(r (mod n d))
			(exponent 0))
	       (if (zero? r)
		   (loop (div q d) (mod q d) (+ exponent 1))
		   exponent))))))

  ;; (strong-Fermat-probable-prime? n)
  ;;
  ;; Does strong Fermat probable prime test with base 2 and returns #t
  ;; if and only if n is a strong Fermat probable prime.  Input must
  ;; be an odd positive integer greater than or equal to 3.  If a
  ;; number passes this test, then it is called a strong Fermat
  ;; probable prime.  It is almost a prime number, but there are
  ;; non-primes that pass this test (strong Fermat pseudoprimes to
  ;; base 2), e.g. 2047, 3277, 4033, 4681, 8321, 15841, 29341, 42799,
  ;; 49141, and 52633 (sequence A001262 in the OEIS).

  (define strong-Fermat-probable-prime?
    (lambda (n)
      (assert (integer? n))
      (assert (odd? n))
      (assert (>= n 3))
      (let* ((delta (- n 1))
	     (s (exact-order delta 2))
	     (d (/ delta (expt 2 s)))
	     (geomseries (map (lambda (x) (expt 2 x)) (iota s)))
	     (two^d (expt-mod 2 d n))
	     (l (map (lambda (x) (expt-mod two^d x n))
		     geomseries)))
	(or (= two^d 1)
	    (exists (lambda (x) (= x delta)) l)))))

  ;; (perfect-square? n)
  ;;
  ;; Returns #t if n is a perfect square; #f otherwise.
  
  (define perfect-square?
    (lambda (n)
      (assert (integer? n))
      (let-values (((_ r) (exact-integer-sqrt n)))
	(zero? r))))

  ;; (coprime? args)
  ;;
  ;; Returns #t if and only if the integer args are relatively prime.
  
  (define coprime?
    (lambda x
      (= (apply gcd x) 1)))

  ;; (Jacobi a n)
  ;;
  ;; Computes the Jacobi symbol (a/n).  Input: n must be positive odd
  ;; integer, a is any integer.
  (define Jacobi
    (lambda (a n)
      (assert (integer? n))
      (assert (integer? a))
      (assert (> n 0))
      (assert (odd? n))
      (cond ((= n 1)
	     1)
	    ((not (= (mod a n) a))
	     (Jacobi (mod a n) n))
	    ((zero? a)
	     0)
	    ((= a 1)
	     1)
	    ((not (coprime? a n))
	     0)
	    ((= a 2)
	     (if (or (= (mod n 8) 1)
		     (= (mod n 8) 7))
		 1
		 -1))
	    ((even? a)
	     (* (Jacobi 2 n) (Jacobi (/ a 2) n)))
	    (else
	     (* (Jacobi n a)
		(if (and (= (mod n 4) 3)
			 (= (mod a 4) 3))
		    -1
		    1))))))

  ;; (signum x)
  ;;
  ;; The signum function:
  ;;     (signum 0) ==>  0
  ;;     (signum x) ==>  1 if x is positive,
  ;;                    -1 if x is negative,
  ;; for real number x.

  (define signum
    (lambda (x)
      (assert (real? x))
      (cond ((= x 0) 0)
	    ((> x 0) 1)
	    (else -1))))

  ;; (suitable-D n args)
  ;;
  ;; Finds a suitable D for the strong Lucas probable prime test.  The
  ;; argument n must be a positive *non-square* odd integer >= 3, but
  ;; suitable-D does not check this, because it is just a helper
  ;; function and the assertion must be checked explicitly or
  ;; implicitly when calling this procedure.
  
  (define suitable-D
    (case-lambda
      ((n)
       (suitable-D n 5))
      ((n candidate)
       (assert (not (perfect-square? n)))
       (let ((sig (signum candidate)))
	 (if (= (Jacobi candidate n) -1)
	     candidate
	     (suitable-D n (* -1 sig (+ (abs candidate) 2))))))))

  ;; (binary-expression n)
  ;;
  ;; Finds a binary expression (list of 0 and 1) of a given number.
  ;; It is another helper function for the strong Lucas primality
  ;; test.
  
  (define binary-expression
    (lambda (n)
      (assert (integer? n))
      (assert (nonnegative? n))
      (if (zero? n)
	  '()
	  (let-values (((q r) (div-and-mod n 2)))
	    (if (zero? r)
		(append (binary-expression q) '(0))
		(append (binary-expression q) '(1)))))))

  ;; (strong-Lucas-probable-prime? n)
  ;;
  ;; Tests whether the given number n is a strong Lucas probable
  ;; prime.
  
  (define strong-Lucas-probable-prime?
    (lambda (n)
      (assert (integer? n))
      (assert (odd? n))
      (assert (>= n 3))
      (let* ((D (suitable-D n))
	     (P 1)
	     (Q (/ (- 1 D) 4))
	     (delta (+ n 1))
	     (s (exact-order delta 2))
	     (d (/ delta (expt 2 s))))
	(define bin->lucas
	  (case-lambda
	    ((binexp)
	     (if (null? binexp)
		 '(0 0)
		 (bin->lucas (cdr binexp) 1 1)))
	    ((binexp u v)
	     (cond ((null? binexp)
		    `(,u ,v))
		   ((zero? (car binexp))
		    (let* ((next-u (mod (* u v) n))
			   (v2+du2 (mod (+ (* v v) (* D u u)) n))
			   (next-v (if (even? v2+du2)
				       (/ v2+du2 2)
				       (/ (+ v2+du2 n) 2))))
		      (bin->lucas (cdr binexp) next-u next-v)))
		   (else
		    (let* ((interm-u (mod (* u v) n))
			   (v2+du2 (mod (+ (* v v) (* D u u)) n))
			   (interm-v (if (even? v2+du2)
					 (/ v2+du2 2)
					 (/ (+ v2+du2 n) 2)))
			   (pu+v (mod (+ (* P interm-u) interm-v) n))
			   (du+pv (mod (+ (* D interm-u)
					  (* P interm-v)) n))
			   (next-u (if (even? pu+v)
				       (/ pu+v 2)
				       (/ (+ pu+v n) 2)))
			   (next-v (if (even? du+pv)
				       (/ du+pv 2)
				       (/ (+ du+pv n) 2))))
		      (bin->lucas (cdr binexp) next-u next-v)))))))
	(define d-lucas (bin->lucas (binary-expression d)))
	(define ud (car d-lucas))
	(define vd (cadr d-lucas))
	(if (zero? ud)
	    #t
	    (let rest-lucas ((i 0) (u ud) (v vd))
	      (cond ((= i s)
		     #f)
		    ((zero? v)
		     #t)
		    (else
		     (let* ((next-lucas (bin->lucas '(0) u v))
			    (next-u (car next-lucas))
			    (next-v (cadr next-lucas)))
		       (rest-lucas (+ i 1) next-u next-v)))))))))

  ;; (prime? n)
  ;;
  ;; Bailie--PSW primality test.  If this test returns #f for a
  ;; number, then the number is definitely composite.  Up until now
  ;; (April 2019), conversely, there are no known non-prime number
  ;; that passes this test.  Moreover, for numbers < 2^64, it is
  ;; certain that this test is deterministic, i.e. always gives the
  ;; correct answer.
  
  (define prime?
    (lambda (n)
      (assert (integer? n))
      (cond ((< n 0) (prime? (- 0 n)))
	    ((<= n 1) #f)
	    ((= n 2) #t)
	    (else
	     (and (small-trial-division-prime? n)
		  (strong-Fermat-probable-prime? n)
		  (not (perfect-square? n))
		  (strong-Lucas-probable-prime? n))))))
  ;; next-prime n
  ;;
  ;; Returns the smallest prime exceeding the given number.
  
  (define next-prime
    (lambda (n)
      (assert (integer? n))
      (cond ((negative? n)
	     (- (previous-prime (- n))))
	    ((<= n 1)
	     2)
	    ((= n 2)
	     3)
	    (else
	     (let ((candidate (if (even? n)
				  (+ n 1)
				  (+ n 2))))
	       (if (prime? candidate)
		   candidate
		   (next-prime candidate)))))))

  ;; previous-prime n
  ;;
  ;; Returns the biggest prime less than the given number.
  
  (define previous-prime
    (lambda (n)
      (assert (integer? n))
      (cond ((negative? n)
	     (- (next-prime (- n))))
	    ((<= n 2)
	     -2)
	    ((= n 3)
	     2)
	    (else
	     (let ((candidate (if (even? n)
				  (- n 1)
				  (- n 2))))
	       (if (prime? candidate)
		   candidate
		   (next-prime candidate)))))))

  ;; factorise n
  ;;
  ;; Returns the list of prime factors of the number n.

  (define factorise
    (lambda (n)
      (assert (integer? n))
      (assert (not (zero? n)))
      (let keep-factorise
	  ((num n)
	   (divisor 2)
	   (factor-list '()))
	(if (> divisor num)
	    factor-list
	    (let-values (((q r) (div-and-mod num divisor)))
	      (if (zero? r)
		  (keep-factorise q divisor (append factor-list `(,divisor)))
		  (keep-factorise num (next-prime divisor) factor-list)))))))  
  

  )
