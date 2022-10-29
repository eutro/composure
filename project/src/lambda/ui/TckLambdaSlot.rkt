#lang gdlisp

(extends LambdaSlot)

(define type)

(define (set-type ty)
  (set! type ty))

(define (_check-term term)
  (cond
    [(== null term) true]
    [(== null type) false]
    [else
     (define tcx (.new TypingCtx))
     (.unify tcx
             null
             (.instantiate tcx (.get-type term))
             (.instantiate tcx type))
     (.-is-ok (.compute-substs tcx))]))
