#lang gdlisp

;; Represents a polytype in the Hindley-Milner type system,
;; i.e. the types we actually care about and can assign to values

(class-name LambdaType)

(define type-vars : Array #;[int] [])
(define mono : LambdaMono mono)

(define (_init tvs mn)
  (set! type-vars tvs)
  (set! mono mn))

(define (_to-string)
  (.to-string mono) ;; Haskell-style
  #;
  (cond
    [((len type-vars) . > . 0)
     (for ([tv type-vars])
       (+set! s " ")
       (+set! s (LambdaMonoVar.name-for tv)))
     (+set! s " . ")
     (+set! s (.to-string mono))
     s]
    [else (.to-string mono)]))
