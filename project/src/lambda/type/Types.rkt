#lang gdlisp

(extends Node)

;; Wrapper {
;;   value: T
;; }
(define CTOR_NUM := (.new TypeCtor "Num" 0 false))
(define CTOR_VEC := (.new TypeCtor "Vec" 0 false))

;; Unit {}
(define CTOR_UNIT := (.new TypeCtor "Unit" 0 false))

;; Fun<A, B> {
;;   apply(x: A): B
;; }
(define CTOR_FUN := (.new TypeCtor "->" 2 true))

;; Action<A, B> {
;;   type State: !Lambda
;;   start(): State
;;   step(x: A, s: State): B
;;   finish(s: State)
;; }
(define CTOR_ACTION := (.new TypeCtor "~>" 2 true))

(define MON_NUM := (.new LambdaMonoCtor CTOR_NUM []))
(define MON_VEC := (.new LambdaMonoCtor CTOR_VEC []))
(define MON_UNIT := (.new LambdaMonoCtor CTOR_UNIT []))

(define TY_NUM := (.new LambdaType [] MON_NUM))
(define TY_VEC := (.new LambdaType [] MON_VEC))
(define TY_UNIT := (.new LambdaType [] MON_UNIT))

(define (mono-fun arg ret)
  (.new LambdaMonoCtor CTOR_FUN [arg ret]))

(define (mono-action arg ret)
  (.new LambdaMonoCtor CTOR_ACTION [arg ret]))

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
