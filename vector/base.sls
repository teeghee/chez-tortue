#!r6rs


(library (vector base)
  
  (export
   (rename (check-index vector-check-index))
   vector-empty?
   vector-append
   vector-take vector-drop
   vector-swap vector-swap!)
  
  (import
    (rnrs base)
    (only (rnrs control) case-lambda do))

  ;; (check-index <vector> <index> <callee>) -> index
  ;;
  ;; Checks that the INDEX is valid into VECTOR.
  
  (define check-index
    (lambda (vec index callee)
      (cond ((< index 0)
	     (assertion-violation
	      callee
	      "Too small index"
	      index))
	    ((>= index (vector-length vec))
	     (assertion-violation
	      callee
	      "Too large index"
	      index))
	    (else
	     index))))

  ;; (vector-empty? <vector>) -> boolean
  ;;
  ;; Returns #t if VECTOR has nothing as its elements; #f otherwise.
  
  (define vector-empty?
    (lambda (vec)
      (assert (vector? vec))
      (zero? (vector-length vec))))

  ;; (vector-append <vector> ...) -> vector
  ;;
  ;; Appends argument vectors.

  (define vector-append
    (case-lambda
      ((vec)
       (assert (vector? vec))
       vec)
      ((vec1 vec2 . rest)
       (assert (vector? vec1))
       (assert (vector? vec2))
       (cond ((vector-empty? vec1)
	      (apply vector-append vec2 rest))
	     ((vector-empty? vec2)
	      (apply vector-append vec1 rest))
	     (else
	      (let* ((len1 (vector-length vec1))
		     (len2 (vector-length vec2))
		     (appended (make-vector (+ len1 len2))))
		(do ((i 0 (+ i 1)))
		    ((= i len1)
		     (values))
		  (vector-set! appended i (vector-ref vec1 i)))
		(do ((i 0 (+ i 1)))
		    ((= i len2)
		     (values))
		  (vector-set! appended (+ len1 i)
			       (vector-ref vec2 i)))
		(apply vector-append appended rest)))))))

  ;; (vector-take <vector> <number>) -> vector
  ;;
  ;; Take NUMBER elements from VECTOR from the left.
  
  (define vector-take
    (lambda (vec n)
      (let ((len (vector-length vec)))
	(cond ((<= n 0)
	       '#())
	      ((>= n len)
	       vec)
	      (else
	       (do ((i 0 (+ i 1))
		    (result (make-vector n)))
		   ((= i n)
		    result)
		 (vector-set! result i (vector-ref vec i))))))))

  ;; (vector-drop <vector> <number>) -> vector
  ;;
  ;; Drop NUMBER elements from VECTOR from the left.
  
  (define vector-drop
    (lambda (vec n)
      (let ((len (vector-length vec)))
	(cond ((<= n 0)
	       vec)
	      ((>= n len)
	       '#())
	      (else
	       (do ((i n (+ i 1))
		    (result (make-vector (- len n))))
		   ((= i len)
		    result)
		 (vector-set! result (- i n)
			      (vector-ref vec i))))))))
  
  ;; (vector-swap <vector> <index1> <index2>) -> vector
  ;;
  ;; Returns the vector which is the same as VECTOR, with contents at
  ;; INDEX1 and INDEX2 swapped.

  (define vector-swap
    (lambda (vec i j)
      (assert (vector? vec))
      (check-index vec i vector-swap!)
      (check-index vec j vector-swap!)
      (let ((len (vector-length vec)))
	(do ((k 0 (+ k 1))
	     (result (make-vector len)))
	    ((= k len)
	     result)
	  (cond ((= k i)
		 (vector-set! result k (vector-ref vec j)))
		((= k j)
		 (vector-set! result k (vector-ref vec i)))
		(else
		 (vector-set! result k (vector-ref vec k))))))))

  ;; (vector-swap! <vector> <index1> <index2>) -> unspecified
  ;;
  ;; Swaps the values in the locations at INDEX1 and INDEX2.
  
  (define vector-swap!
    (lambda (vec i j)
      (assert (vector? vec))
      (check-index vec i vector-swap!)
      (check-index vec j vector-swap!)
      (let ((x (vector-ref vec i)))
	(vector-set! vec i (vector-ref vec j))
	(vector-set! vec j x))))


)
