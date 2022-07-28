;;  meta-nock.scm  jpt4  UTC20150612
;;  (meta-nock), meta-circular evaluator for nock expressions, written in 
;;  nock, interpreted by (nocko)

;;  [b a] ncond - eval set b of [test consequence] pairs against a, eval as 
;;  nested redex 6 in order.
;(define ncond `[9 b [8

(define rasf '[1 1])
(define rast `[1 0])

(define meta-nock
`[6 [3 0 2] ,rast ,rasf])

;;  TODO: IMPLEMENT MINIKANREN DECIMAL NUMERALS
(define meta-nocko (dec-to-rbin meta-nock))
