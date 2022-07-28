;;  num-convert.scm  jpt4  UTC20150611

;;  Convert decimal nock code to reverse binary nocko code.
(define (dec-to-rbin nexp)
  (cond
    [(null? nexp) '()]
    [(atom? nexp) (build-nocko-num nexp)]
    [(pair? (car nexp)) (cons 
                          (cons (dec-to-rbin (caar nexp)) 
                                (dec-to-rbin (cdar nexp))) 
                          (dec-to-rbin (cdr nexp)))]
    [else (cons (dec-to-rbin (car nexp)) (dec-to-rbin (cdr nexp)))]))

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
(define (rbin-to-dec nexpo)
  (cond
    [(null? nexpo) '()]
    [(equal? (car nexpo) 'num) (build-dec (cadr nexpo))]
    [(and (not (equal? (caar nexpo) 'num)) 
       (pair? (car nexpo)))
     (cons 
       (cons (rbin-to-dec (caar nexpo)) 
             (rbin-to-dec (cdar nexpo))) 
       (rbin-to-dec (cdr nexpo)))]
    [else (cons (rbin-to-dec (car nexpo)) (rbin-to-dec (cdr nexpo)))]))

;;  Decimal from reverse binary nocko numbers.
(define (build-dec n)
  (build-dec-aux n 0))

(define (build-dec-aux n e)
  (cond
	  [(null? n) 0]
	  [(zero? (car n)) (build-dec-aux (cdr n) (add1 e))]
    [else (+ (expt 2 e) (build-dec-aux (cdr n) (add1 e)))]))
