#!r6rs

;; Queue Data Structure
;;
;; SICP 3.3.2

(library (structure queue)
  
  (export
   make-queue
   empty-queue? queue-front
   queue-insert! queue-delete!
   queue-copy
   queue->list)

  (import (rnrs base)
	  (only (rnrs mutable-pairs)
		set-car! set-cdr!)
	  (only (chezscheme)
		last-pair list-copy))
  
  (define front-ptr
    (lambda (queue) (car queue)))

  (define rear-ptr
    (lambda (queue) (cdr queue)))

  (define set-front-ptr!
    (lambda (queue item) (set-car! queue item)))

  (define set-rear-ptr!
    (lambda (queue item) (set-cdr! queue item)))

  (define empty-queue?
    (lambda (queue) (null? (front-ptr queue))))

  (define make-queue
    (lambda () (cons '() '())))

  (define queue-front
    (lambda (queue)
      (if (empty-queue? queue)
	  (error 'queue-front
		 "called with an empty queue" queue)
	  (car (front-ptr queue)))))

  (define queue-insert!
    (lambda (queue item)
      (let ([new-pair (cons item '())])
	(cond [(empty-queue? queue)
	       (set-front-ptr! queue new-pair)
	       (set-rear-ptr! queue new-pair)
	       queue]
	      [else
	       (set-cdr! (rear-ptr queue) new-pair)
	       (set-rear-ptr! queue new-pair)
	       queue]))))

  (define queue-delete!
    (lambda (queue)
      (cond [(empty-queue? queue)
	     (error 'queue-delete!
		    "called with an empty queue" queue)]
	    [else
	     (set-front-ptr! queue (cdr (front-ptr queue)))
	     queue])))

  (define queue-copy
    (lambda (queue)
      (let* ([newqueue (make-queue)]
	     [front (list-copy (front-ptr queue))]
	     [rear (last-pair front)])
	(set-front-ptr! newqueue front)
	(set-rear-ptr! newqueue rear)
	newqueue)))

  (define queue->list
    (lambda (queue)
      (list-copy (front-ptr queue))))






  )
