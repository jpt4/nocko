;;  nevalo-wrapper.scm  jpt4  UTC02250625
;;  The spec specifies its own entry point.

;;  wrap call to (nevalo) with (nocko)
(define (nocko i o)
  (cond
    [(and (null? i) (null? o))
		 (begin (run 1 (q r s)	(nevalo r s) (== `[exp=,r res=,s] q)))]
	  [(null? i) (let ([out (dec-to-rbin o)])
								(run 1 (q)
                  (fresh (r s)
									  (== s out)
										(nevalo `(nock ,r) s)
										(== `[exp=,r res=,s] q))))]
    [(null? o) (let ([in (dec-to-rbin i)])
								(run 1 (q)
								  (fresh (r s)
                    (== r in)
									  (nevalo `(nock ,r) s)
									  (== `[exp=,r res=,s] q))))]
    [else (let ([in (dec-to-rbin i)]
                [out (dec-to-rbin o)])
						(run 1 (q)
              (fresh (r s)
							  (== r in)
								(== s out)
								(nevalo `(nock ,r) s)
								(== `[exp=,r res=,s] q))))]))
