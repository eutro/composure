#lang gdlisp

;; Represents a polytype in the Hindley-Milner type system,
;; i.e. the types we actually care about and can assign to values

(class-name Type)

(define type-vars : Dictionary #;{int {TypeClass true}})
(define mono : Mono mono)

(define (_init tvs mn)
  (when (is tvs Array)
    (define new-tvs {})
    (for ([tv tvs])
      (set! (ref new-tvs tv) {}))
    (set! tvs new-tvs))
  (set! type-vars tvs)
  (set! mono mn))

(define (with-mono mono)
  (.new (load "res://src/lambda/type/Type.gd")
        type-vars
        mono))

(define static (str-constraint cnstr)
  (+ (.to-string (ref cnstr 1))
     " "
     (MonoVar.name-for (ref cnstr 0))))

;; NB: Ɓ is replaced with ligatured =>
(define (_to-string)
   ;; Haskell-style
  (define has-constraints false)
  (define cnstrs [])
  (define tvs type-vars)
  (for ([tv tvs])
    (for ([cnstr (ref tvs tv)])
      (.append cnstrs [tv cnstr])))
  (define mono-str (.to-string mono))
  (match (len cnstrs)
    [0 mono-str]
    [1 (+ (str-constraint (ref cnstrs 0))
          " Ɓ "
          mono-str)]
    [_
     (define s "(")
     (+set! s (str-constraint (ref cnstrs 0)))
     (for ([i (range 1 (len cnstrs))])
       (+set! s ", ")
       (+set! s (str-constraint (ref cnstrs i))))
     (+set! s ") Ɓ ")
     (+set! s mono-str)
     s]))

(define (to-json)
  (define tvs-json [])
  (for ([tv type-vars])
    (define cnstrs [])
    (for ([cn (ref type-vars tv)])
      (.append cnstrs cn.name))
    (.append tvs-json [tv cnstrs]))
  {
   "tvs" tvs-json
   "mono" (mono-to-json mono)})

(define (mono-to-json mono)
  (cond
    [(.is-ctor mono)
     (define vl [mono.ctor.name])
     (for ([arg mono.args])
       (.append vl (mono-to-json arg)))
     vl]
    [else
     mono.no]))

(define static (mono-from-json x)
  (cond
    [(is x Array)
     (define ctor (Types.lookup-ctor (ref x 0)))
     (define args [])
     (for ([i (range 1 (len x))])
       (.append args (mono-from-json (ref x i))))
     (MonoCtor.new ctor args)]
    [else (MonoVar.new (int x))]))

(define static (from-json x)
  (define Self (load "res://src/lambda/type/Type.gd"))
  (define tvs {})
  (for ([e x.tvs])
    (define cnstrs {})
    (for ([cn (ref e 1)])
      (set! (ref cnstrs (Types.lookup-typeclass cn)) true))
    (set! (ref tvs (int (ref e 0))) cnstrs))
  (define mono (mono-from-json x.mono))
  (Self.new tvs mono))
