;;  portability.scm jpt4 UTC20151108 
;;  Scheme functions not necessarily present in all implementations.

;;  guile
(define (atom? a)
	(not (pair? a)))
