(load "convert.scm")
(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")
(load "hof-nevalo.scm")
(load "clever-meta-nocko.scm")
(load "dec.scm")
(load "quine.scm")
(load "hof-nevalo.scm")
(load "trw-nevalo.scm")

(load "nock.scm")


(define (nocko i o)
  (cond
    [(and (null? i) (null? o))
		 (begin (run 1 (q r s)	(nevalo r s) (== `[exp=,r res=,s] q)))]
	  [(null? i) (let ([out (dec-to-rbin o)])
								(run 1 (q r s)
										 (== s out)
										 (nevalo `(nock ,r) s)
										 (== `[exp=,r res=,s] q)))]
    [(null? o) (let ([in (dec-to-rbin i)])
								(run 1 (q r s)
										 (== r in)
										 (nevalo `(nock ,r) s)
										 (== `[exp=,r res=,s] q)))]
    [else (let ([in (dec-to-rbin i)]
                [out (dec-to-rbin o)])
						(run 1 (q r s)
								 (== r in)
								 (== s out)
								 (nevalo `(nock ,r) s)
								 (== `[exp=,r res=,s] q)))]))
