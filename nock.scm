;;  nock.scm
;;  non-relational use of (nocko), wraps miniKanren (run) interface
;;  jpt4
;;  UTC20150612

;;  nexp is a noun
(define (nock nexp)
  (let* ([nexpo (preprocess nexp)]
         [res (car (run 1 (q) (nocko `(* ,nexpo) q)))])
    (postprocess res)))
        
