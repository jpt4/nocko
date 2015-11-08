;;  nock.scm  jpt4  UTC20150612
;;  non-relational use of (nevalo), wraps miniKanren (run) interface

;;  nexp is a noun
(define (nock nexp)
  (let* ([nexpo (car (run 1 (q) (raso (dec-to-rbin nexp) q)))]
         [res (car (run 1 (q) (nevalo `(nock ,nexpo) q)))])
    (rbin-to-dec res)))
