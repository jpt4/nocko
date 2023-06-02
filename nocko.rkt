;;  nocko.rkt
;;  20220718Z
;;  jpt4
;;  Nock 4K in miniKanren
;;  Racket v8.5+

#lang racket

(require minikanren)
(require minikanren/numbers)
(require "mk-prelude.rkt")
(require "convert.rkt") 

;;  general datatype - noun
(define (noun?o i)
  (conde
   [(atom?o i)]
   [(cell?o i)]))

;;  primitive datatype - atom
;;  ex: 6 -> (0 1 1) -> (num (0 1 1))
(define (atom?o i)
  (conde
   [(fresh (d)
	   (== `(num ,d) i)
	   (num?o d))]))

;;  mk reverse binary numbers, except explicitly numerical and with
;;  exposed zero '(0).
;;  TEST: standard mk numbers
#;(define (num?o i) 
  (conde
   [(== '() i)]
   [(== '(1) i)]
   [(fresh (a d)
	   (== `(0 . ,d) i)
	   (=/= '(0) d)
           (=/= '() d)
	   (num?o d))]
   [(fresh (a d)
	   (== `(1 . ,d) i)
	   (=/= '(0) d)
           (=/= '() d)
	   (num?o d))]))

(define (num?o i)
  #;(conde
   [  (=/= '() i)
      (== '(0) i)]
   [  (=/= '() i)
      (== '(1) i)]
   [  (=/= '() i)
      (fresh (d)
	   (== `(0 . ,d) i)
	   (=/= '(0) d)
	   (num?o d))]
   [  (=/= '() i)
      (fresh (d)
	   (== `(1 . ,d) i) 
	   (=/= '(0) d)
	   (num?o d))])
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
	   (num?o d))]))

;;  composite datatype - cell
(define (cell?o i)
  (conde
   [(fresh (a d)
	   (== `[,a ,d] i)
	   (noun?o a)
	   (noun?o d))]))
;;  wut redexes
(define (wuto i o)
  (conde
   [(cell?o i) (== '(num (0)) o)]
   [(atom?o i) (== '(num (1)) o)]
   ))
;;  lus redexes
(define (luso i o)
  (conde
   [(cell?o i) (== `[+ ,i] o)]
   [(atom?o i) (+1o i o)]
   ))
;;  tis redexes
(define (tiso i o)
  (fresh (a aa ad)
	 (conde
	  [(== `[,a ,a] i) (== '(num (0)) o)]
	  [(cell?o i) (== `[,aa ,ad] i) (=/= aa ad) (== '(num (1)) o)]
	  ;;  degenerate comparison expression
          [(atom?o i) (== `[= ,i] o)] 
	  )))
;;  fas redexes
(define (faso i o)
  (fresh (a b adr res1 res2)
	 (conde
	  ;;  parent, left, right addresses - contents should be of pure datatype
	  [(== `[(num (1)) ,a] i) (noun?o a) (== a o)]
	  [(== `[(num (0 1)) [,a ,b]] i) (noun?o a) (== a o)]
	  [(== `[(num (1 1)) [,a ,b]] i) (noun?o b) (== b o)]
	  ;;  deep even addresses
	  [(== `[,adr ,b] i) (gtheo adr '(num (0 0 1))) (+o a a adr) 
	   (faso `[,a ,b] res1)
	   (faso `[(num (0 1)) ,res1] o)]
	  ;;  deep odd addresses
	  [(== `[,adr ,b] i) (gtheo adr '(num (1 1))) (+o a a res1)
	   (+1o res1 adr) (faso `[,a ,b] res2)
	   (faso `[(num (1 1)) ,res2] o)]
	  ;;  degenerate address expression
          ;;  Derivation of failure cases: 0. noun?o i 0. atom?o i 1. cell?o
          [(atom?o i) (== `(/ ,i) o)]
          [(== `[,a ,b] i) (cell?o a) (== `(/ ,i) o)]
          [(== `[,a ,b] i) (== '(num (0)) a) (== `(/ ,i) o)]
	  )))
;;  hax redexes
(define (haxo i o)
  (fresh (a b c adr res1 res2 res3 res4)
         (conde
          [(== `[(num (1)) [,a ,b]] i) (== a o)]
          [(== `[,adr [,b ,c]] i) (gtheo a '(num (1))) (+o a a adr)
           (+1o adr res1) (faso `[,res1 ,c] res2) 
           (haxo `[,a [[,b ,res2] ,c]] res3) (== res3 o)]
          [(== `[,adr [,b ,c]] i) (gtheo a '(num (1))) (+o a a res1)
           (+1o res1 adr) (+o a a res2) (faso `[,res2 ,c] res3) 
           (haxo `[,a [[,res2 ,b] ,c]] res4) (== res4 o)]
          ;;  degenerate hax expression
          ;;  |#| because # alone is a reserved symbol in Racket
          [(atom?o i) (== `(|#| ,i) o)]
          [(== `[,a ,b] i) (cell?o a) (== `(|#| ,i) o)]
          [(== `[,a ,b] i) (== '(num (0)) a) (== `(|#| ,i) o)]
          )))
;;  tar redexes
(define (taro i o)
  (fresh (a b c d res1 res2 res3 res4 res5)
	 (conde
	  ;;  composite application redex 
	  [(== `[,a [[,b ,c] ,d]] i)
           (noun?o a) (noun?o b) (noun?o c) (noun?o d) (=/= 'num b)
           #;(== `(i=,i a=,a b=,b c=,c d=,d cons=,(cons c d)) o)
           #;(== `(abc= [,a [,b ,c]]) o)
           #;(taro `[,a [,b ,c]] o) 
           (taro `[,a [,b ,c]] res1)
	   #;(== `[a=,a d=,d] o)
           (taro `[,a ,d] res2) 
           (== `[,res1 ,res2] o)]
	  ;;  redex zero
	  [(== `[,a [(num (0)) ,b]] i) 
           #;(== `[a=,a b=,b] o)
           (faso `[,b ,a] o)]
	  ;;  redex one
	  [(== `[,a [(num (1)) ,b]] i) (noun?o a) (noun?o b)
	   (== b o)]
	  ;;  redex two *[a 2 b c]=>*[*[a b] *[a c]]
	  [(== `[,a [(num (0 1)) [,b ,c]]] i)
           #;(== `(r2 a=,a b=,b c=,c res1=,res1 res2=,res2) o)
           (taro `[,a ,b] res1) 
	   (taro `[,a ,c] res2)
	   (taro `[,res1 ,res2] o)]
	  ;;  redex three
          [(== `[,a [(num (1 1)) ,b]] i)
	   (taro `[,a ,b] res1)
	   (wuto res1 o)]
	  ;;  redex four
	  [(== `[,a [(num (0 0 1)) ,b]] i)
	   (taro `[,a ,b] res1) 
	   (luso res1 o)]
	  ;;  redex five
	  [(== `[,a [(num (1 0 1)) ,b]] i)
	   (taro `[,a ,b] res1)
	   (tiso res1 o)]
	  ;;  synthetic redexes
	  ;;  redex six  *[a 6 b c d]=>*[a *[[c d] [0 *[[2 3] [0 *[a 4 4 b]]]]]]
          ;;                         =>*[a *[[c d] [0 *[[2 3] [0 ++*[a b]]]]]]
	  [(== `[,a [(num (0 1 1)) [,b [,c ,d]]]] i)
	   (taro `[,a ,b] res1) 
	   (luso res1 res2)
	   (luso res2 res3)
	   (taro `[[(num (0 1)) (num (1 1))] [(num (0)) ,res3]] res4)
	   (taro `[[,c ,d] [(num (0)) ,res4]] res5) 
	   (taro `[,a ,res5] o)]
	  ;;  redex seven  *[a 7 b c]=>*[a 2 b 1 c]
          ;;                         =>*[*[a b] c]
	  [(== `[,a [(num (1 1 1)) [,b ,c]]] i)
	   (taro `[,a ,b] res1) (taro `[,res1 ,c] o)]
	  ;;  redex eight  *[a 8 b c]=>*[a 7 [[7 [0 1] b] 0 1] c]
          ;;                         =>*[[*[a b] a] c]
	  [(== `[,a [(num (0 0 0 1)) [,b ,c]]] i)
	   (taro `[,a ,b] res1) (taro `[[,res1 ,a] ,c] o)]
	  ;;  redex nine  *[a 9 b c]=>*[*[a c] 2 [0 1] 0 b]
          ;;                        =>*[*[*[*[a c] [0 1]] *[*[a c] [0 b]]]]
          ;;                        =>*[*[/[1 *[a c]] /[b *[a c]]]]
          ;;                        =>*[*[*[a c] /[b *[a c]]]]
          [(== `[,a [(num (1 0 0 1)) [,b ,c]]] i)
           #;(== `(r9 a=,a b=,b c=,c ac=,`[,a ,c] res1=,res1 res2=,res2) o)
           (taro `[,a ,c] res1)
           #;(taro `[,res1 [(num (0 1)) [[(num (0)) (num (1))] [(num (0)) ,b]]]] res2)
           (taro `[,res1 [(num (0 1)) [[(num (0)) (num (1))] [(num (0)) ,b]]]] o)]
	  #;[(== `[,a [(num (1 0 0 1)) [,b ,c]]] i)
	   (taro `[,a ,c] res1) 
	   (faso `[,b ,res1] res2)
	   (taro `[,res1 ,res2] o)]
          ;;  redex 10  *[a 10 [b c] d]=>#[b *[a c] *[a d]]
          [(== `[,a [(num (0 1 0 1)) [[,b ,c] ,d]]] i)
           (taro `[,a ,c] res1) (taro `[,a ,d] res2)
           (haxo `[,b [,res1 ,res2]] o)]          
	  ;;  redex eleven cell  *[a 11 [b c] d]=>*[[*[a c] *[a d]] 0 3]
          ;;                                    =>/[3 [*[a c] *[a d]]]
          ;;                                    =>*[a d]
          ;;  In which the evaluation strategy of nock becomes
          ;;  relevant: by a plain language interpretation of "Reduce
          ;;  by the first matching pattern", outer reduction should
          ;;  select *[a d] within /[3 ...] without previously
          ;;  matching to the evaluation of *[a c]. This is comparable
          ;;  to a lazy evaluation strategy. An interpreter supporting
          ;;  an inner reduction strategy, comparable to
          ;;  strict/applicative order strategy, will evaluate *[a c]
          ;;  and *[a d] prior to the outer fas reduction. If a spec
          ;;  compliant nock implementation _must_ evaluate *[a c],
          ;;  e.g. to allow for the production of side-effectful
          ;;  signals processed by the runtime, then the natural
          ;;  language reduction order instructions must be elaborated
          ;;  upon.
            ;;  outer reduction
          [(== `[,a [(num (0 1 0 1)) [[,b ,c] ,d]]] i)
	   (cell?o `[,b ,c]) 
           (taro `[,a ,d] o)]
            ;;  inner reduction
          #;[(== `[,a [(num (1 1 0 1)) [[,b ,c] ,d]]] i)
           (cell?o `[,b ,c])       
           (taro `[,a ,c] res1) (taro `[,a ,d] res2) 
           (faso `[(num (1 1)) [,res1 ,res2]] o)]
	  ;;  redex eleven atom *[a 11 b c]=>*[a c]
	  [(== `[,a [(num (1 1 0 1)) [,b ,c]]] i)
	   (atom?o b)
	   (taro `[,a ,c] o)]
          ;;  degenerate tar expression *a=>*a
            ;;  a = atom
            ;;  (nocko `(* (num ...)) q) -> (* (num ...))
          [(atom?o i) (== `(*-1 ,i) o)]
            ;;  a = cell, second noun an atom
          [(== `[,a ,b] i) (atom?o b) 
                           #;(== `(i=,i a=,a b=,b) o)
                           (== `(*-2 ,i) o)
                           #;(== `(* ,i) o)]
            ;;  a = cell, second noun a cell, first noun thereof an atom > 11
          [(== `[,a ,b] i) (== `[,c ,d] b) (cell?o b)
           (atom?o c) 
           #;(=/= '(num (0)) c)
           (gtheo c '(num (0 0 1 1))) (== `(*-3 i=,i a=,a b=,b c=,c d=,d) o)]
            ;;  a = cell, second noun a cell, first noun thereof a cell /\/ No, by composite application.
          ;[(== `[,a ,b] i) (cell?o b) (== `[,c ,d] b) (cell?o c) (== `(* ,i) o)]
	  )))
;;  (nevalo) interprets a quoted nock expression.
;;  sel/ser delineates cell boundaries only.
(define (nevalo i o)
  (fresh (a ra)
	 (conde
	  ;;  nock(a) -> (nock a) -> `(nock ,a) -> 
          ;;  (run* (q) (nevalo `(nock ,a) q))
	  [(== `(nock ,a) i) (raso a ra) (taro ra o)]
	  ;;  ras redex - ideally redundant, that which is raso'd on
	  ;;  entry should stay so
	  [(not-ras?o i) (raso i ra) (taro ra o)]
	  )))
;;  TODO: Revise for non-term rewriting evaluation - current version
;;  introduces unnecessary search tree duplication.
;;  define right associativity
;;  converts non-ra tuples (potentially deep) into ra
;;  recurses until fundamental datatype found - these assumed ra
(define (raso i o)
  #;(fresh (a b c d resa resb resc resd rest diag)
         (conde
          [(atom?o i) (== i o)]
          [(cell?o i) (== i o)]
          [(== `[,a . ,b] i)
           (=/= 'num a) (=/= '() b)
           ;(== 1 o)
           #;(== `(a=,a b=,b resa=,resa resb=,resb) o)
           (raso a resa) 
           (raso b resb)
           (== `[,resa . [,resb]] o)]
          [(== `[,a . ,b] i)
           (== '() b)
           ;(== `(a=,a b=,b resa=,resa resb=,resb) o)
           (raso a resa) 
           (== resa o)]
          ))
  (conde
   [(fresh (a b c d resa resb resc resd rest diag)
	   (== `[,a ,b ,c . ,d] i)
           (raso a resa) (raso b resb) (raso c resc)
	   (fresh (da dd)
		  (conde
		   ;;  If the tail is complex, its first element must
		   ;;  be whole, and its second non-trivial. Currently
		   ;;  enforces the former via explicit not-tag?o
		   ;;  check.
		   [(== `[,da . ,dd] d) (=/= '() dd) 
                                        (not-tag?o da) (raso d resd)
                                        (== `[,resa [,resb [,resc ,resd]]] rest)
                                        (== rest o)]
		   ;;  if the tail is simple, extract the single
		   ;;  element
		   [(== `[,da . ()] d) (raso da resd)
                                       (== `[,resa [,resb [,resc ,resd]]] rest)
                    (== rest o)]
		   )))]
   ;;  three noun tuple
   [(fresh (a b c resa resb resc rest) 
	   (== `[,a ,b ,c] i) (raso a resa) (raso b resb) (raso c resc)
	   (== `[,resa [,resb ,resc]] rest)
	   (== rest o))]
   ;;  two noun tuple
   [(fresh (a b resa resb rest)
	   (== `[,a ,b] i) (raso a resa) (raso b resb) (== `[,resa ,resb] rest)
	   (== rest o))]
   ;;  clean cell
   [(cell?o i) (== i o)]
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
	 (raso i a) (=/= i a)))
;;  TODO: correlate with (raso) TODO
;;  test if not data tag
(define (not-tag?o i)
  (conde
   [(=/= 'num i) (=/= '* i) (=/= '? i) (=/= '+ i) (=/= '= i) (=/= '/ i) 
                 (=/= '|#| i)]))
;;  nocko specific arithmetic, produces only numeric output
#;(define (+o p q r)
  (fresh (a b c)
	 (== `(num ,a) p) (== `(num ,b) q) (== `(num ,c) r)
         (pluso a b c)))
(define (+o p q r)
#;  (fresh (a b c)
         ;(num?o a) (num?o b) (num?o c)
	 (conde
          ;[(== '(0) a) (== '(0) b) (== '(0) c)]
	  [(== '(0) a) (== q r)]
	  [(== '(0) b) (== p r)]
	  [(== `(num ,a) p) (== `(num ,b) q) (== `(num ,c) r)
                            (=/= '(num (0)) p) (=/= '(num (0)) q) (=/= '(num (0)) r) (<=o '() c) (pluso a b c)]
	  ))
  (fresh (a b c)
	 (== `(num ,a) p) (== `(num ,b) q) (== `(num ,c) r)
	 ;(num?o a) (num?o b) (num?o c)
	 (conde
          ;[(== '(0) a) (== '(0) b) (== '(0) c)]
	  [(== '(0) a) (== q r)]
	  [(== '(0) b) (== p r)]
	  [(=/= '(0) a) (=/= '(0) b) (=/= '(0) c) (<=o '() c) (pluso a b c)]
	  )))
;;  nocko (add1)
(define (+1o i o)
  (+o i '(num (1)) o))
;;  comparators
;;  p >= q - without negatives, greater-than-or-equal-to is primitive
(define (gtheo p q)
  (fresh (n m)
         (== `(num ,n) p)
         (conde
          [(== '(0) n) (== '(num (0)) q)]
          [(=/= '(0) n) (<=o m n) (== `(num ,m) q)]))
  #;(fresh (a)
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
(define (nk-lengtho i o)
  (fresh (res)
	 (mk-lengtho i res)
	 (== `(num ,res) o)))
;; notes
#|
0. Unexpected divergence:

Ex. 1:

(run 1 (a b c)  (pluso a b c) (== '(1) a) (== '() c))
<diverges>
(run 1 (a b c)  (pluso a b c) (== '() c) (== '(1) a))
<diverges>
(run 1 (a b c)  (== '(1) a) (pluso a b c) (== '() c))
<diverges>
(run 1 (a b c)  (== '() c) (pluso a b c) (== '(1) a))
'()
(run 1 (a b c)  (== '(1) a) (== '() c) (pluso a b c))
'()
(run 1 (a b c)  (== '() c) (== '(1) a) (pluso a b c))
'()

Ex. 2

(run 1 (a b c)  (minuso c b a) (== '(1) a) (== '() c))
<diverges>
(run 1 (a b c)  (minuso c b a) (== '() c) (== '(1) a))
<diverges>
(run 1 (a b c)  (== '(1) a) (minuso c b a) (== '() c))
<diverges>
(run 1 (a b c)  (== '() c) (minuso c b a) (== '(1) a))
'()
(run 1 (a b c)  (== '(1) a) (== '() c) (minuso c b a))
'()
(run 1 (a b c)  (== '() c) (== '(1) a) (minuso c b a))
'()
Same results as with pluso

(run 1 (a b c)  (pluso a b c) (>= a c))

(run 1 (a b c)  (>= a c) (pluso a b c))


(run 1 (a b c)  (minuso a b c) (>= c a))

(run 1 (a b c)  (>= c a) (minuso a b c))

Why aren't the constraints commutative?

Per [0], conjunctive constraints are commutative modulo divergence
versus failure. Divergent unifications _may_ fail finitely, given
appropriate constraint ordering, but only successful resolvents are
confluent.

[0] https://stackoverflow.com/questions/57123536/understanding-termination-in-minikanren-counting-bits-in-a-list/71394432#71394432

Ex. 3

(run 1 (a b c) (pluso a b c) (== '(0 1) c))
'(((0 1) () (0 1)))
racket@nocko> (run 2 (a b c) (pluso a b c) (== '(0 1) c))
'(((0 1) () (0 1)) (() (0 1) (0 1)))
racket@nocko> (run 3 (a b c) (pluso a b c) (== '(0 1) c))
'(((0 1) () (0 1)) (() (0 1) (0 1)) ((1) (1) (0 1)))
racket@nocko> (run 4 (a b c) (pluso a b c) (== '(0 1) c))
<diverges>
racket@nocko> (run* (a b c) (pluso a b c) (== '(0 1) c))
<diverges>
(run* (a b c) (== '(0 1) c) (pluso a b c))
'(((0 1) () (0 1)) (() (0 1) (0 1)) ((1) (1) (0 1)))

Why are the constraints in Ex. 3 not commutative?

Why does introducing numbero cause the failure of valid unifications?

https://stackoverflow.com/questions/73270998/why-does-introducing-numbero-in-minikanren-cause-the-failure-of-valid-unificatio

I am using Racket v8.5, with the packages for minikanren and
minikanren/numbers required. Why does introducing the numbero
constraint cause valid unifications to fail?

Because numbero operates on Racket natural numbers, not Kiselyov.

> (run 1 (q) (<lo '(1) q))
'((_.0 _.1 . _.2))

> (run 1 (q) (<lo '(1) q) (numbero q))
'()

> (run 1 (q) (numbero q) (<lo '(1) q))
'()

> (run 1 (q) (<lo q '(1)))
'(())

> (run 1 (q) (<lo q '(1)) (numbero q))
'()

> (run 1 (q) (numbero q) (<lo q '(1)))
'()

Addtionally:
(run 1 (q) (numbero q) (<lo '() '(1)))
'((_.0 (num _.0)))
racket@nocko> (run 1 (q) (<lo '() '(1)) (numbero q))
'((_.0 (num _.0)))
(run 1 (q) (<lo '(1) '()) (numbero q))
'()
racket@nocko> (run 1 (q) (numbero q) (<lo '(1) '()))
'()

|#
