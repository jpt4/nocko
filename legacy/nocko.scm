;;  nocko.scm  jpt4  UTC20150621
;;  nocko, miniKanren relational nock interpreter

;;  set-up nocko execution environment, with specializations
(define (nconfig op)
  (case op
    ['all  (maximalist)]                    ;;  hof-nevalo.scm, everything
    ['aux  (utilities)]                     ;;  nocko auxiliary programs
    ['hof  (higher-of-nevalo)]              ;;  (nevalo) using hof-nevalo.scm
    ['min  (core)]                          ;;  hof-nevalo.scm, mk dependencies
    ['trw  (term-rw-nevalo)]                ;;  (nevalo) using trw-nevalo.scm
    ['zum  (examples)]                      ;;  example nocko programs
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
		(load "portability.scm")
    (load "hof-nevalo.scm")
		(load "nevalo-wrapper.scm")))

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
    (load "nock.scm")
    (load "test-suite.scm")))
