;;  diag-nocko.scm  jpt4  UTC20150612
;;  nocko, miniKanren relational Nock interpreter
;;  reserved for diagnostics, eventually tracing

;;  dependencies
(load "/home/jpt4/miniKanren-with-symbolic-constraints/mk.scm")
(load "/home/jpt4/miniKanren-with-symbolic-constraints/numbers.scm")

;;  primitive datatype - atom
;;  ex: 6 -> (0 1 1) -> (num (0 1 1))
(define (atom?o i)
  (conde
    [(fresh (d)
       (== `(num . (,d)) i)
       (num?o d))]
))
;;  mk reverse binary numbers, except explicitly numerical
;;  and with exposed zero '(0).
(define (num?o i)
  (conde
    [(== '(0) i)]
    [(== '(1) i)]
    [(fresh (d)
       (== `(0 . ,d) i)
       (=/= '(0) d)
       (num?o d))]
	  [(fresh (d)
       (== `(1 . ,d) i) 
       (=/= '(0) d)
       (num?o d))]
))
;;  composite datatype - cell
(define (cell?o i)
	(conde
    [(fresh (a d)
       (== `[,a ,d] i)
       (noun?o a)
       (noun?o d))]
))
;;  general datatype - noun
(define (noun?o i)
  (conde
    [(atom?o i)]
    [(cell?o i)]
))
;;  internal datatype - operation
(define (oper?o i)
  (fresh (a)  
    (conde
      [(== `(* ,a) i) (houn?o a)]
      [(== `(? ,a) i) (houn?o a)]
      [(== `(+ ,a) i) (houn?o a)]
      [(== `(= ,a) i) (houn?o a)]
      [(== `(/ ,a) i) (houn?o a)]
)))
;;  hybrid cell - hell
(define (hell?o i)
  (fresh (a d)
    (== `(,a ,d) i)
    (houn?o a) (houn?o d)
))
;;  hybrid noun - houn
(define (houn?o i)
  (conde
    [(atom?o i)]
    [(hell?o i)]
    [(oper?o i)]
))    
;;  (nocko) interprets a quoted nock expression.
;;  prefix operators scope selves and operands with pel/per.
;;  sel/ser delineates cell boundaries only.
(define (nocko i o)
  (fresh (a aa ad adr b c d ra res1 res2 res3 res4 res5 res6 rest res)
    (conde
;;  nock(a) -> (nock a) -> `(nock ,a) -> (run* (q) (nocko `(nock ,a) q))
      [(== `(nock ,a) i) (raso a ra) (nocko `(* ,ra) res)
       (== res o)]
;;  ras redex - ideally redundant, that which is raso'd on entry should stay so
      [(not-ras?o i) (raso i res) (nocko res o)]
;;  wut redexes
      [(== `(? ,a) i) (hell?o a) (== '(num (0)) o)]
      [(== `(? ,a) i) (atom?o a) (== '(num (1)) o)]
;;  lus redexes
;;  degenerate add one
      [(== `(+ ,a) i) (hell?o a) (== i o)]
;;  applicable add one
      [(== `(+ ,a) i) (atom?o a) (+1o a res) (== res o)]
;;  tis redexes
      [(== `(= [,a ,a]) i) (== '(num (0)) o)]
      [(== `(= ,a) i) (hell?o a) (== `[,aa ,ad] a)
       (=/= aa ad) (== '(num (1)) o)]
;;  degenerate compare
      [(== `(= ,a) i) (atom?o a) (== i o)]
;;  fas redexes
;;  parent, left, right addresses - contents should be of pure datatype
      [(== `(/ [(num (1)) ,a]) i) (noun?o a) (== a o)]
      [(== `(/ [(num (0 1)) [,a ,b]]) i) (noun?o a) (== a o)]
      [(== `(/ [(num (1 1)) [,a ,b]]) i) (noun?o b) (== b o)]
;;  deep even addresses
      [(== `(/ [,adr ,b]) i) (gtheo adr '(num (0 0 1))) (+o a a adr) 
       (nocko `(/ [,a ,b]) res1)
       (nocko `(/ [(num (0 1)) ,res1]) res2) (== res2 o)]
;;  deep odd addresses
      [(== `(/ [,adr ,b]) i) (gtheo adr '(num (1 1))) 
       (+o a a res1) (+1o res1 adr) (nocko `(/ [,a ,b]) res2)
       (nocko `(/ [(num (1 1)) ,res2]) res3) (== res3 o)]
;;  degenerate address expression
      [(== `(/ ,a) i) (atom?o a) (== i o)]
;;  composite application redex
      [(== `(* [,a [[,b ,c] ,d]]) i) (nocko `(* [,a [,b ,c]]) res1)
       (nocko `(* [,a ,d]) res2) (== `[,res1 ,res2] res3) (== res3 o)]
;;  redex zero
      [(== `(* [,a [(num (0)) ,b]]) i) (nocko `(/ [,b ,a]) res) (== res o)]
       ;(== 'diag0 o)]
;;  redex one
      [(== `(* [,a [(num (1)) ,b]]) i) 
       (houn?o a) (houn?o b)
       (== b o)]
;;  redex two *[a 2 b c]=>*[*[a b] *[a c]]
      [(== `(* [,a [(num (0 1)) [,b ,c]]]) i)
       (nocko `(* [,a ,b]) res1) 
       (nocko `(* [,a ,c]) res2)
       (nocko `(* [,res1 ,res2]) res3) 
       (== res3 o)]
       ;(== 'diag o)]
;;  redex three
      [(== `(* [,a [(num (1 1)) ,b]]) i)
       (nocko `(* [,a ,b]) res1)
       (nocko `(? ,res1) res2) (== res2 o)]
;;  redex four
      [(== `(* [,a [(num (0 0 1)) ,b]]) i)
       (nocko `(* [,a ,b]) res1) 
       (nocko `(+ ,res1) res2) (== res2 o)]
;;  redex five
      [(== `(* [,a [(num (1 0 1)) ,b]]) i)
       (nocko `(* [,a ,b]) res1)
       (nocko `(= ,res1) res2) (== res2 o)]
;;  synthetic redexes
;;  redex six  *[a 6 b c d]=>*[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]
;;                         =>*[a *[[c d] [0 *[[2 3] [0 ++*[a b]]]]]]
      [(== `(* [,a [(num (0 1 1)) [,b [,c ,d]]]]) i)
       (nocko `(* [,a ,b]) res1) 
       (nocko `(+ ,res1) res2)
       (nocko `(+ ,res2) res3)
       (nocko `(* [[(num (0 1)) (num (1 1))] [(num (0)) ,res3]]) res4)
       (nocko `(* [[,c ,d] [(num (0)) ,res4]]) res5) 
       (nocko `(* [,a ,res5]) res6)
       (== res6 o)]
;;  redex seven  *[a 7 b c]=>*[a 2 b 1 c]=>*[*[a b] c]
      [(== `(* [,a [(num (1 1 1)) [,b ,c]]]) i)
       (nocko `(* [,a ,b]) res1) (nocko `(* [,res1 ,c]) rest)
       (== rest o)]
;;  redex eight  *[a 8 b c]=>*[a 7 [[7 [0 1] b] 0 1] c]=>*[[*[a b] a] c]
      [(== `(* [,a [(num (0 0 0 1)) [,b ,c]]]) i)
       (nocko `(* [,a ,b]) res1) (nocko `(* [[,res1 ,a] ,c]) rest)
       (== rest o)]
;;  redex nine  *[a 9 b c]=>*[a 7 c 2 [0 1] 0 b]=>*[*[a c] /[b *[a c]]]
      [(== `(* [,a [(num (1 0 0 1)) [,b ,c]]]) i)
       (nocko `(* [,a ,c]) res1) 
       (nocko `(/ [,b ,res1]) res2)
       (nocko `(* [,res1 ,res2]) rest)
       (== rest o)]
;;  redex ten cell  *[a 10 [b c] d]=>*[a 8 c 7 [0 3] d]=>*[/[3 [*[a c] a]] d]
;;  in which nock reveals its strict evaluation
      [(== `(* [,a [(num (0 1 0 1)) [[,b ,c] ,d]]]) i)
       (hell?o `[,b ,c])       
       (nocko `(* [,a ,c]) res1) (nocko `(/ [(num (1 1)) [,res1 ,a]]) res2)
       (nocko `(* [,res2 ,d]) rest)
       (== rest o)]
;;  redex ten atom *[a 10 b c]=>*[a c]
      [(== `(* [,a [(num (0 1 0 1)) [,b ,c]]]) i)
       (atom?o b)
       (nocko `(* [,a ,c]) res)
       (== res o)]
;;  (nocko `(* (num ...)) q) -> (* (num ...))
      [(== `(* ,a) i) (atom?o a) (== `(* ,a) o)]
)))
;;  define right associativity
;;  converts non-ra tuples (potentially deep) into ra
;;  recurses until fundamental datatype found - these assumed ra
(define (raso i o)
  (conde
    [(fresh (a b c d resa resb resc resd rest diag)
       (== `[,a ,b ,c . ,d] i)
       (raso a resa) (raso b resb) (raso c resc)
       (fresh (da dd)
         (conde
;;  If the tail is complex, its first element must be whole, and its second
;;  non-trivial. Currently enforces the former via explicit not-tag?o check.
           [(== `[,da . ,dd] d) (=/= '() dd) (not-tag?o da) (raso d resd)
            (== `[,resa [,resb [,resc ,resd]]] rest) 
            (== rest o)]
;;  if the tail is simple, extract the single element
           [(== `[,da . ()] d) (raso da resd)
            (== `[,resa [,resb [,resc ,resd]]] rest) 
            (== rest o)]
         )))]
;;  three houn tuple
    [(fresh (a b c resa resb resc rest) 
       (== `[,a ,b ,c] i) (raso a resa) (raso b resb) (raso c resc)
			 (== `[,resa [,resb ,resc]] rest)
			 (== rest o))]
;;  two houn tuple
    [(fresh (a b resa resb rest)
       (== `[,a ,b] i) (raso a resa) (raso b resb) (== `[,resa ,resb] rest)
       (== rest o))]
;;  clean operation
    [(oper?o i) (== i o)]
;;  clean hell
    [(hell?o i) (== i o)]
;;  bare atom
    [(atom?o i) (== i o)]
))
;;  test right-associativity
(define (ras?o i)
  (fresh (a)
    (raso a i)
))
;;  test non-right-associativity
(define (not-ras?o i)
  (fresh (a)
    (raso i a) (=/= i a)
))
;;  test if not data tag
(define (not-tag?o i)
  (conde
    [(=/= 'num i) (=/= '* i) (=/= '? i) (=/= '+ i) (=/= '= i) (=/= '/ i)]
))
;;  nocko specific arithmetic, produces only numeric output
(define (+o p q r)
  (fresh (a b c)
    (== `(num ,a) p) (== `(num ,b) q) (== `(num ,c) r)
    (num?o a) (num?o b) (num?o c)
  (conde
    [(== '(0) a) (== q r)]
    [(== '(0) b) (== p r)]
    [(=/= '(0) a) (=/= '(0) b) (pluso a b c)]
)))
;;  nocko (add1)
(define (+1o i o)
  (+o i '(num (1)) o))
;;  comparators
;;  p >= q - without negatives, greater-than-or-equal-to is primitive
(define (gtheo p q)
  (fresh (a)
	  (+o q a p)))
;;  p > q - and strictly greater-than is composite. Else, p >= 0 lacks p = 0
(define (gtho p q)
  (fresh (a)
    (+1o q a) (gtheo p a)))
;;  p <= q
(define (ltheo p q)  ;;  lOSEtheoS
  (gtheo q p))
;;  p < q
(define (ltho p q)
  (gtho q p))
;;  mk-lengtho - number of top-level elements in list
(define (mk-lengtho i o)
  (conde
    [(== '() i) (== '() o)]
    [(fresh (a d res)
       (== `(,a . ,d) i) (mk-lengtho d res) (pluso '(1) res o))]))
;;  nocko specific length
(define (lengtho i o)
  (fresh (res)
    (mk-lengtho i res)
    (== `(num ,res) o)))
