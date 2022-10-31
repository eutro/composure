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
          " => "
          mono-str)]
    [_
     (define s "(")
     (+set! s (str-constraint (ref cnstrs 0)))
     (for ([i (range 1 (len cnstrs))])
       (+set! s ", ")
       (+set! s (str-constraint (ref cnstrs i))))
     (+set! s ") => ")
     (+set! s mono-str)
     s]))
