#!r6rs

(library (vector mergesort)

  (export
   mergesort
   mergesort-and-count-inversion)

  (import
    (rnrs base)
    (only (rnrs control) do)
    (only (vector base) vector-take vector-drop))

  (define merge
    (lambda (method vec1 vec2)
      (let* ((len1 (vector-length vec1))
	     (len2 (vector-length vec2))
	     (total-len (+ len1 len2))
	     (merged (make-vector total-len)))
	(do ((k 0 (+ k 1))
	     (i 0)
	     (j 0))
	    ((= k total-len)
	     merged)
	  (cond ((= i len1)
		 (vector-set! merged k (vector-ref vec2 j))
		 (set! j (+ j 1)))
		((= j len2)
		 (vector-set! merged k (vector-ref vec1 i))
		 (set! i (+ i 1)))
		((method (vector-ref vec1 i) (vector-ref vec2 j))
		 (vector-set! merged k (vector-ref vec1 i))
		 (set! i (+ i 1)))
		(else
		 (vector-set! merged k (vector-ref vec2 j))
		 (set! j (+ j 1))))))))

  (define merge-and-count-inversion
    (lambda (method vec1 c1 vec2 c2)
      (let* ((len1 (vector-length vec1))
	     (len2 (vector-length vec2))
	     (total-len (+ len1 len2))
	     (merged (make-vector total-len)))
	(do ((k 0 (+ k 1))
	     (i 0)
	     (j 0)
	     (c 0))
	    ((= k total-len)
	     (values merged (+ c1 c2 c)))
	  (cond ((= i len1)
		 (vector-set! merged k (vector-ref vec2 j))
		 (set! j (+ j 1)))
		((= j len2)
		 (vector-set! merged k (vector-ref vec1 i))
		 (set! i (+ i 1))
		 (set! c (+ c len2)))
		((method (vector-ref vec1 i) (vector-ref vec2 j))
		 (vector-set! merged k (vector-ref vec1 i))
		 (set! i (+ i 1))
		 (set! c (+ c j)))
		(else
		 (vector-set! merged k (vector-ref vec2 j))
		 (set! j (+ j 1))))))))

  (define mergesort
    (lambda (vec)
      (assert (vector? vec))
      (let* ((len (vector-length vec))
	     (len/2 (div len 2)))
	(if (<= len 1)
	    vec
	    (let ((first-half (mergesort (vector-take vec len/2)))
		  (second-half (mergesort (vector-drop vec len/2))))
	      (merge <= first-half second-half))))))

  (define mergesort-and-count-inversion
    (lambda (vec)
      (assert (vector? vec))
      (let* ((len (vector-length vec))
	     (len/2 (div len 2)))
	(if (<= len 1)
	    (values vec 0)
	    (let-values (((vec1 c1)
			  (mergesort-and-count-inversion
			   (vector-take vec len/2)))
			 ((vec2 c2)
			  (mergesort-and-count-inversion
			   (vector-drop vec len/2))))
	      (merge-and-count-inversion <= vec1 c1 vec2 c2))))))



  )
