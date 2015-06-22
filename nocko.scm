;;  nocko.scm  jpt4  UTC20150621
;;  nocko, miniKanren relational nock interpreter

(define (nocko i o)
  (cond
    [(and (null? i) (null? o))
		 (run 1 (q r s)	(nevalo r s) (== `[exp=,r res=,s] q))]
	  [(null? i) (let ([out (dec-to-rbin o)])
								(run 1 (q r s)
										 (== s out)
										 (nevalo r s)
										 (== `[exp=,r res=,s] q)))]
    [(null? o) (let ([in (dec-to-rbin o)])
								(run 1 (q r s)
										 (== r in)
										 (nevalo r s)
										 (== `[exp=,r res=,s] q)))]
    [else (let ([in (dec-to-rbin i)]
                [out (dec-to-rbin o)])
						(run 1 (q r s)
								 (== r in)
								 (== s out)
								 (nevalo r s)
								 (== `[exp=,r res=,s] q)))]))	 

;;  set-up nocko execution environment, with specializations
(define (nconfig op)
  (case op
    ['all  (maximalist)]            ;;  hof-nevalo.scm + everything
    ['aux  (utilities) (examples)]  ;;  extras only
    ['hof  (higher-of-nevalo)]      ;;  (nevalo) using hof-nevalo.scm
    ['min  (core)]                  ;;  hof-nevalo.scm + mk dependencies
    ['trw  (term-rw-nevalo)]        ;;  (nevalo) using trw-nevalo.scm
    [else  (maximalist)]))          ;;  default to 'all
    
(define (maximalist)
  (core)
  (utilities)
  (examples))

(define (core)
  (load "miniKanren-with-symbolic-constraints/mk.scm")
  (load "miniKanren-with-symbolic-constraints/numbers.scm")
  (load "hof-nevalo.scm"))

(define (examples)
  (load "clever-meta-nocko.scm")
  (load "dec.scm")
  (load "quine.scm"))

(define (higher-of-nevalo)
  (load "hof-nevalo.scm"))

(define (term-rw-nevalo)
  (load "trw-nevalo.scm"))

(define (utilities)
  (load "convert.scm")
  (load "nock.scm"))
