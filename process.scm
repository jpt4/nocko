;;  process.scm  jpt4  UTC20150611

;;  Convert decimal nock code to reverse binary nocko code.
(define (preprocess nexp)
  (cond
    [(null? nexp) '()]
    [(atom? nexp) (build-nocko-num nexp)]
    [(pair? (car nexp)) (cons 
                          (cons (preprocess (caar nexp)) 
                                (preprocess (cdar nexp))) 
                          (preprocess (cdr nexp)))]
    [else (cons (preprocess (car nexp)) (preprocess (cdr nexp)))]))

;;  Reverse binary nocko numbers from decimal.
(define (build-nocko-num n)
  (if (zero? n) '(num (0))
    `(num ,(build-nocko-num-aux n))))

(define (build-nocko-num-aux n)
  (cond
    [(zero? n) '()]
    [(odd? n) (cons 1 (build-nocko-num-aux (/ (- n 1) 2)))]
    [(and (> n 1) (even? n)) (cons 0 (build-nocko-num-aux (/ n 2)))]))


;;  Convert reverse binary nocko code to decimal nock code.
(define (postprocess nexpo)
  (cond
    [(null? nexpo) '()]
    [(equal? (car nexpo) 'num) (build-dec (cadr nexpo))]
    [(and (not (equal? (caar nexpo) 'num)) 
       (pair? (car nexpo)))
     (cons 
       (cons (postprocess (caar nexpo)) 
             (postprocess (cdar nexpo))) 
       (postprocess (cdr nexpo)))]
    [else (cons (postprocess (car nexpo)) (postprocess (cdr nexpo)))]))

;;  Decimal from reverse binary nocko numbers.
(define (build-dec n)
  (build-dec-aux n 0))

(define (build-dec-aux n e)
  (cond
	  [(null? n) 0]
	  [(zero? (car n)) (build-dec-aux (cdr n) (add1 e))]
    [else (+ (expt 2 e) (build-dec-aux (cdr n) (add1 e)))]))
