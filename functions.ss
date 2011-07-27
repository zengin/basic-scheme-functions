;;Depth function
;;returns the depth of given list
(define (depth lst)
	(cond
		((atom? lst) 0)
		((null? lst) 1)
		((list? (car lst)) (max ( + 1 (depth (car lst)))(depth (cdr lst))))
		(else (depth (cdr lst)))
	)
)

;;Depth helper function
(define atom?
	(lambda( lst) (not ( or (null? lst) (list? lst))))
)

;;count-elm function
;;returns the counts each element in the given list
(define (count-elm lst) (count-elm2 lst '()))

;;count-elm helper functions
(define (count-elm2 lst lst2)
	(cond
		((null? lst) '())
		((not (member lst2 (car lst)))
			(cons 
				(cons 
					(car lst) 
					(list (count lst (car lst)))
				)
				(count-elm2 	(cdr lst) 
						(cons	(car lst) lst2)
				)
			) 
		)
		(else (count-elm2 (cdr lst) lst2))
	)
)

(define (count lst at)
	(cond
		((null? lst) 0)
		((eq? at (car lst)) (+ 1 (count (cdr lst) at)))
		(else (count (cdr lst) at))
	)
)

(define (member lst at)
	(cond
		((null? lst) #f)
		((list? (car lst)) (or (member (car lst) at) (member (cdr lst) at)))
		((eq? (car lst) at) #t)
		(else (member (cdr lst) at))
	)
)

(define (size lst)
	(cond
		((null? lst) 0)
		(else ( + 1 (size (cdr lst))))
	)
)

;;Sublists function
;;returns all sublists of a given list
(define (sublists lst)
	(cond
		((null? lst) '(()))
		(else (append (add-element-to-all (car lst) (sublists (cdr lst))) (sublists (cdr lst))) )
	)
)

;;Sublists helper function
(define (add-element-to-all at lst)
	(cond
		((null? lst) '())
		(else (cons (cons at (car lst)) (add-element-to-all at (cdr lst))))
	)
)
