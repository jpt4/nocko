;;  nocko.scm  jpt4  UTC20150621
;;  nocko, miniKanren relational nock interpreter
;;  set-up nocko execution environment, with specializations

(define (nocko op)
  (case op
    ['all  (maximalist)]
    ['aux  (utilities) (examples)]
    ['hof  (higher-of-nevalo)]
		['min  (core)]
		['trw  (term-rw-nevalo)]
		[else  (maximalist)]))

		
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


