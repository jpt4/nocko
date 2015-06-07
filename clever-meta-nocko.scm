;;  Courtesy Gavin Whelan
;;  UTC20150607

;;  Destructures indices two and three of subject, rebuilds and
;;  evaluates. Reminiscent of using (eval) in one's meta-circular
;;  Scheme interpreter.
(define clever-meta-nocko
  '[(num (0 1)) [[(num (0)) (num (0 1))] [(num (0)) (num (1 1))]]])
