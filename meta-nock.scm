;;  meta-nock.scm  jpt4  UTC20150612
;;  (meta-nock), meta-circular evaluator for nock expressions, written in 
;;  nock, interpreted by (nocko)

;;  TODO: IMPLEMENT MINIKANREN DECIMAL NUMERALS


(define rasf '[1 1])
(define rast `[1 0])

(define meta-nock
`[6 [3 0 2] ,rast ,rasf])

(define meta-nocko (preprocess meta-nock))
