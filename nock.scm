;;  nock.scm
;;  non-relational use of (nevalo), wraps miniKanren (run) interface
;;  jpt4
;;  UTC20150612

;;  nexp is a noun
(define (nock nexp)
  (let* ([nexpo (dec-to-rbin nexp)]
         [res (car (run 1 (q) (nevalo `(* ,nexpo) q)))])
    (rbin-to-dec res)))
        
