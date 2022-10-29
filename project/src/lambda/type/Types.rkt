#lang gdlisp

(extends Node)

(define CTOR_NUM := (.new TypeCtor "Num" 0 false))
(define CTOR_FUN := (.new TypeCtor "->" 2 true))
(define CTOR_ACTION := (.new TypeCtor "=>" 2 true))

(define MON_NUM := (.new LambdaMonoCtor CTOR_NUM []))

(define TY_NUM := (.new LambdaType [] MON_NUM))

(define (mono-fun arg ret)
  (.new LambdaMonoCtor CTOR_FUN [arg ret]))

(define (mono-bin-fun arg1 arg2 ret)
  (mono-fun arg1 (mono-fun arg2 ret)))

(define (compute-application f-ty x-ty)
  (define tcx (.new TypingCtx))
  (define fun-ty (.instantiate tcx f-ty))
  (define arg-ty (.instantiate tcx x-ty))
  (define ret-ty (.new LambdaMonoVar (.newtype tcx)))
  (.unify tcx null (Types.mono-fun arg-ty ret-ty) fun-ty)
  (define res (.compute-substs tcx))
  (if (.-is-ok res)
      (.new Result true (.generalise tcx ret-ty))
      res))
