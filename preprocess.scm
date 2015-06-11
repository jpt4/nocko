;;  process.scm
;;  jpt4
;;  UTC20150611

;;  Convert decimal nock to reverse binary nocko.

(define (preprocess nexp)
  (cond
    [(null? nexp) '()]
    [(atom? nexp) (cons 'num (cons (build-num nexp) '()))]
    [(pair? (car nexp)) (cons 
                          (cons (preprocess (caar nexp)) (preprocess (cdar nexp))) 
                          (preprocess (cdr nexp)))]
    [else (cons (cons 'num (cons (build-num (car nexp)) '())) (preprocess (cdr nexp)))]))

;;  Convert reverse binary nocko to decimal nock.

(define (postprocess nexpo)
  (cond
    [(equal? (car nexpo) 'num) (build-dec (cdr nexpo))]))

(define (build-dec-aux n e)
  (cond
	  [(null? n) 0]
	  [(zero? (car n)) 0]
    [else (+ (exp 2 e) (build-dec-aux (cdr n) (add1 e)))]))
