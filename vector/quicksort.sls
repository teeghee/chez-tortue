#!r6rs



(library (vector quicksort)

  (export quicksort!
	  quicksort!-and-count-comparisons)

  (import
    (rnrs base)
    (only (rnrs control) case-lambda do unless)
    (only (chezscheme) random)
    (vector base))

  ;; (quicksort! <vector>) -> unspecified
  ;; (quicksort! <vector> <start-index> <end-index>) -> unspecified
  ;;
  ;; Do quicksort on VECTOR.  Note that if no START-INDEX and
  ;; END-INDEX are specified, then it defaults to the entire VECTOR.
  
  (define quicksort!
    (case-lambda
      ((vec)
       (assert (vector? vec))
       (quicksort! vec 0 (- (vector-length vec) 1)))
      ((vec start end)
       (assert (vector? vec))
       (if (>= start end)
	   (values)
	   (let* ((len (+ (- end start) 1))
		  (pivot-index (+ (random len) start))
		  (pivot (vector-ref vec pivot-index)))
	     (vector-swap! vec start pivot-index)
	     (do ((i (+ start 1) (+ i 1))
		  (j (+ start 1)))
		 ((= i (+ end 1))
		  (begin 
		    (vector-swap! vec (- j 1) start)
		    (quicksort! vec start (- j 2))
		    (quicksort! vec j end)))
	       (unless (>= (vector-ref vec i)
			   pivot)
		 (vector-swap! vec i j)
		 (set! j (+ j 1)))))))))

  ;; (quicksort!-and-count-comparisons <vector>) -> count
  ;; (quicksort!-and-count-comparisons
  ;;   <vector> <start-index> <end-index>) -> count
  ;;
  ;; Same as in QUICKSORT!; but it also counts the number of
  ;; comparisons made in the process.
  
  (define quicksort!-and-count-comparisons
    (case-lambda
      ((vec)
       (assert (vector? vec))
       (quicksort!-and-count-comparisons
	vec 0 (- (vector-length vec) 1)))
      ((vec start end)
       (assert (vector? vec))
       (if (>= start end)
	   0
	   (let* ((len (+ (- end start) 1))
		  (pivot-index (+ (random len) start))
		  (pivot (vector-ref vec pivot-index)))
	     (vector-swap! vec start pivot-index)
	     (do ((i (+ start 1) (+ i 1))
		  (j (+ start 1)))
		 ((= i (+ end 1))
		  (begin 
		    (vector-swap! vec (- j 1) start)
		    (+ (- end start)
		       (quicksort!-and-count-comparisons
			vec start (- j 2))
		       (quicksort!-and-count-comparisons
			vec j end))))
	       (unless (>= (vector-ref vec i)
			   pivot)
		 (vector-swap! vec i j)
		 (set! j (+ j 1)))))))))



  )
