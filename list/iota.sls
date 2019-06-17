#!r6rs

(library (list iota)

  (export iota)

  (import
    (rnrs base)
    (only (rnrs control) case-lambda)
    (rename (only (chezscheme) iota)
	    (iota original-iota)))

  ;; (iota <number> <start> <step>) -> list
  ;;
  ;; Make a list of numbers of size NUMBER, starting from START and
  ;; increasing/decreasing by STEP.  If STEP is not specified, it
  ;; defaults to 1, furthermore, if START is not specified, it
  ;; defaluts to 0.
  
  (define iota
    (case-lambda
      ((num)
       (original-iota num))
      ((num start)
       (map (lambda (n) (+ n start)) (iota num)))
      ((num start step)
       (map (lambda (n) (+ (* n step) start)) (iota num)))))


)
