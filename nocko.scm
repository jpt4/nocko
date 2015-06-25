;;  nocko.scm  jpt4  UTC20150621
;;  nocko, miniKanren relational nock interpreter



;;  set-up nocko execution environment, with specializations
(define (nconfig op)
  (case op
    ['all  (maximalist)]                    ;;  hof-nevalo.scm, everything
    ['aux  (begin (utilities) (examples))]  ;;  extras only
    ['hof  (higher-of-nevalo)]              ;;  (nevalo) using hof-nevalo.scm
    ['min  (core)]                          ;;  hof-nevalo.scm, mk dependencies
    ['trw  (term-rw-nevalo)]                ;;  (nevalo) using trw-nevalo.scm
    [else  (maximalist)]))                  ;;  default to 'all
    
(define (maximalist)
  (begin
	  (core)
    (utilities)
    (examples)))

(define (core)
  (begin
    (load "miniKanren-with-symbolic-constraints/mk.scm")
    (load "miniKanren-with-symbolic-constraints/numbers.scm")
    (load "hof-nevalo.scm")))

(define (examples)
  (begin
    (load "clever-meta-nocko.scm")
    (load "dec.scm")
    (load "quine.scm")))

(define (higher-of-nevalo)
  (load "hof-nevalo.scm"))

(define (term-rw-nevalo)
  (load "trw-nevalo.scm"))

(define (utilities)
  (begin
    (load "convert.scm")
    (load "nock.scm")))

;;  (nocko), nevalo wrapper
;;  WHY: If (define (nocko ...)...) written into REPL directly, (run) is 
;;  detected. Loaded from nocko.scm, "variable run is not bound"
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
