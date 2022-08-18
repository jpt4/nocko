;;  test-suite.rkt  
;;  20220811Z
;;  jpt4 
;;  nocko test suite
;;  Racket v8.5
;;  (test) syntax from 
;;  github.com/webyrd/miniKanren-with-symbolic-constraints/test-check.scm

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

;;  By default, recognition of valid nocko expressions is tested.
;;  Expression generation may differ across miniKanren implementations.
(define (test-suite op)
  (case op
    [all (begin (test-recognize) (test-generate))]
    [rec (test-recognize)]
    [gen (test-generate)]
    [else (recognize)]))

(define (test-recognize)
  (test "recognize nocko zero"
    (run* (q) (num?o '(0)))
    '(_.0))

  (test "don't recognize miniKanren zero"
    (run* (q) (num?o '()))
    '())

  (test "recognize nocko numbers > 0"
    (run* (q) (num?o '(1)) (num?o '(0 1)) (num?o '(1 1)) (num?o '(0 0 1))
		  		    (num?o '(1 0 1)) (num?o '(0 1 1)) (num?o '(1 1 1))
              (num?o '(0 0 0 1)) (num?o '(0 1 0 1)) (num?o '(1 0 1 0 1 0 1))
              (num?o '(0 0 0 0 0 0 0 1)) (num?o '(1 1 1 1 1 1 1 1 1)))
    '(_.0))

  (test "recognize nocko atoms"
    (run* (q) (atom?o '(num (0))) (atom?o '(num (0 1))) (atom?o '(num (1 1)))
              (atom?o '(num (0 0 1))) (atom?o '(num (1 0 1))) 
              (atom?o '(num (0 1 1))) (atom?o '(num (1 1 1))) 
              (atom?o '(num (0 0 0 1))) (atom?o '(num (0 1 0 1))) 
              (atom?o '(num (1 0 1 0 1 0 1))) (atom?o '(num (0 0 0 0 0 0 0 1)))
              (atom?o '(num (1 1 1 1 1 1 1 1 1))))
    '(_.0))

  (test "recognize nocko cells"
    (run* (q) (cell?o '[(num (0 1)) (num (1 0 1))])
              (cell?o '[(num (0 1)) [(num (0 1 1)) (num (1 1 1))]])
              (cell?o '[[(num (0 0 1)) (num (1 0 1))] (num (1 1))])
              (cell?o '[[(num (0 0 1)) (num (1 0 1))] 
                        [(num (0 1 1)) (num (1 1 1))]])
              (cell?o '[(num (0 1)) [[(num (0)) (num (1))] (num (1 1))]])
              (cell?o '[[[[(num (0)) (num (0))]
			  (num (0))] (num (0))]
			[[(num (0)) [(num (0)) [(num (0)) (num (0))]]]
			 [(num (0)) (num (0))]]]))
    '(_.0))

  (test "recognize nouns"
	(run* (q) (noun?o '(num (0))) 
              (noun?o '(num (0 1 0 1 1)))
              (noun?o '[(num (0)) (num (1))]) 
              (noun?o '[(num (0)) [(num (1)) (num (0 1))]])
              (noun?o '[[(num (0)) [(num (0)) (num (0))]] (num (0))])
              (noun?o '[[[[(num (0)) (num (0))]
			  (num (0))] [[(num (0)) (num (0))] (num (0))]]
			[[(num (0)) [(num (0)) [(num (0)) (num (0))]]]
			 [(num (0)) (num (0))]]]))
	'(_.0)))

(define (test-generate)
  (test "generate nocko numbers"
	(run 20 (q) (num?o q))
	'((0) (1) (0 1) (1 1) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 0 1)
	  (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (1 0 1 1) (0 1 1 1)
		       (1 1 1 1) (0 0 0 0 1) (1 0 0 0 1) (0 1 0 0 1) (1 1 0 0 1)))

  (test "generate nocko atoms"
    (run 20 (q) (atom?o q))
    '((num (0)) (num (1)) (num (0 1)) (num (1 1)) (num (0 0 1))
	  	(num (1 0 1)) (num (0 1 1)) (num (1 1 1)) (num (0 0 0 1))
		  (num (1 0 0 1)) (num (0 1 0 1)) (num (1 1 0 1))
  		(num (0 0 1 1)) (num (1 0 1 1)) (num (0 1 1 1))
	  	(num (1 1 1 1)) (num (0 0 0 0 1)) (num (1 0 0 0 1))
		  (num (0 1 0 0 1)) (num (1 1 0 0 1))))

  (test "generate cells"
    (run 20 (q) (cell?o q))
    '(((num (0)) (num (0))) ((num (0)) (num (1))) ((num (1)) (num (0)))
	  	((num (0)) (num (0 1))) ((num (1)) (num (1)))
		  ((num (0)) (num (1 1))) ((num (0)) (num (0 0 1)))
  		((num (1)) (num (0 1))) ((num (0)) (num (1 0 1)))
	  	((num (0)) (num (0 1 1))) ((num (1)) (num (1 1)))
  		((num (0)) (num (1 1 1))) ((num (0)) ((num (0)) (num (0))))
	  	((num (0 1)) (num (0))) ((num (0)) ((num (0)) (num (1))))
		  ((num (0 1)) (num (1))) ((num (0)) (num (0 0 0 1)))
  		((num (1)) (num (0 0 1))) ((num (0)) (num (1 0 0 1)))
	  	((num (0)) ((num (1)) (num (0))))))
)
#|

(test "don't recognize broken atom")

(define (not-atom?o i)
  (conde
    [

"don't recognize broken"
"cell"
"noun"
|#

#|
(define (multi-test ver arg)
    (define (outer ver1 arg1)
      (cond
        [(null? ver1) 'exit]
        [else (let ([cur (symbol->string (car ver1))])
                (begin (load cur)
                  (inner (cdr ver1) arg)))]))
    (define (inner ver2 arg2)
      (cond
        [(null? arg2) (outer (cdr ver2) arg)]
        [else (time
                (begin (eval (car arg2))
                  (inner ver2 (cdr arg2))))]))
    (outer ver arg))

|#
