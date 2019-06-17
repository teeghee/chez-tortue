#!r6rs

(library (vector randomized-selection)

  (export randomized-selection)

  (import
    (rnrs base)
    (only (rnrs control) do)
    (only (chezscheme) random)
    (vector base))

  ;; (randomized-selection <vector> <index>) -> element
  ;;
  ;; Use randomized selection algorithm to find INDEX-th order
  ;; statistic in VECTOR.  Here VECTOR must be a non-empty vector, and
  ;; INDEX must be in between 0 and the length of the vector minus 1
  ;; (i.e. must be usual index of VECTOR).
  
  (define randomized-selection
    (lambda (vec index)
      (assert (vector? vec))
      (assert (not (vector-empty? vec)))
      (vector-check-index vec index randomized-selection)
      (let ((len (vector-length vec)))
	(if (= len 1)
	    (vector-ref vec 0)
	    (let* ((pivot-index (random len))
		   (pivot (vector-ref vec pivot-index))
		   (vec (vector-swap vec 0 pivot-index)))
	      (do ((k 1 (+ k 1))
		   (smaller '#())
		   (bigger  '#()))
		  ((= k len)
		   (let ((pivot-index (vector-length smaller)))
		     (cond ((> pivot-index index)
			    (randomized-selection smaller index))
			   ((< pivot-index index)
			    (randomized-selection
			     bigger
			     (- index pivot-index 1)))
			   (else
			    pivot))))
		(let ((element (vector-ref vec k)))
		  (if (< element pivot)
		      (set! smaller
			(vector-append
			 smaller (vector element)))
		      (set! bigger
			(vector-append
			 bigger
			 (vector element)))))))))))





  )
