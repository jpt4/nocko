;;  convert.rkt  
;;  20220805Z
;;  jpt4  
;;  decimal nock <-> reverse binary nocko
;;  Racket v8.5

(module convert racket
  (provide dec->rbin
	   rbin->dec)

;;  Convert decimal nock code to reverse binary nocko code.
(define (dec->rbin nexp)
  (cond
   [(null? nexp) '()]
   [(number? nexp) (build-nocko-num nexp)]
   [(pair? (car nexp)) (cons 
                        (cons (dec->rbin (caar nexp)) 
                              (dec->rbin (cdar nexp))) 
                        (dec->rbin (cdr nexp)))]
   [else (cons (dec->rbin (car nexp)) (dec->rbin (cdr nexp)))]))

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
(define (rbin->dec nexpo)
  (cond
   [(null? nexpo) '()]
   [(member (car nexpo) '(? = + / * |#|)) 
    (cons (car nexpo) (rbin->dec (cdr nexpo)))]
   [(equal? (car nexpo) 'num) (build-dec (cadr nexpo))]
   #;[(and (not (equal? (caar nexpo) 'num)) 
	 (pair? (car nexpo)))
    (cons 
     (cons (rbin->dec (caar nexpo)) 
           (rbin->dec (cdar nexpo))) 
     (rbin->dec (cdr nexpo)))]
   [else (cons (rbin->dec (car nexpo)) (rbin->dec (cdr nexpo)))]))

;;  Decimal from reverse binary nocko numbers.
(define (build-dec n)
  (build-dec-aux n 0))

(define (build-dec-aux n e)
  (cond
   [(null? n) 0]
   [(zero? (car n)) (build-dec-aux (cdr n) (add1 e))]
   [else (+ (expt 2 e) (build-dec-aux (cdr n) (add1 e)))]))
)
