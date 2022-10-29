#lang gdlisp

(extends Node)

(class Cons
  (define car)
  (define cdr)
  (define (_init ca cd)
    (set! car ca)
    (set! cdr cd)))

(class Partial
  (extends LambdaValue)

  (define type : LambdaType)
  (define name : String)
  (define arity : int)
  (define p-args)
  (define func-ref : FuncRef)

  (define (_init nm ty arty fref p-rgs)
    (set! name nm)
    (set! type ty)
    (set! arity arty)
    (set! func-ref fref)
    (set! p-args p-rgs))

  (define (get-type) type)

  (define (create-preview)
    (.create TextPreview name))

  (define (apply x)
    (cond
      [(arity . <= . 1)
       ;; do the call
       (define args [])
       (let loop ([pa p-args])
         (when (not (== null pa))
           (.append args (.-car pa))
           (recur loop (.-cdr pa))))
       (.append args x)
       (.call-funcv func-ref args)]
      [else
       (define res (Types.compute-application type (.get-type x)))
       (assert (.-is-ok res))
       (.new Partial
             (+ name "'")
             (.-value res)
             (- arity 1)
             func-ref
             (.new Cons x p-args))])))

(define (add a b) (.new LambdaNumber (+ (.-value a) (.-value b))))

(define VAL_ADD
  (Partial.new
   "+"
   (LambdaType.new
    []
    (Types.mono-bin-fun
     Types.MON_NUM
     Types.MON_NUM
     Types.MON_NUM))
   2
   (funcref self "add")
   null))

(class Composed
  (extends LambdaValue)

  ;; λx.g(f x)
  (define f)
  (define g)

  (define (_init fv gv)
    (set! f fv)
    (set! g gv))

  (define ty)
  (define (get-type)
    (when (== null ty)
      (define tcx (.new TypingCtx))
      (define f-ty (.instantiate tcx (.get-type f)))
      (define g-ty (.instantiate tcx (.get-type g)))
      (define a (LambdaMonoVar.new (.newtype tcx)))
      (define b (LambdaMonoVar.new (.newtype tcx)))
      (define c (LambdaMonoVar.new (.newtype tcx)))
      (.unify tcx null (Types.mono-fun a b) f-ty)
      (.unify tcx null (Types.mono-fun b c) g-ty)
      (define res (.compute-substs tcx))
      (assert (.-is-ok res))
      (set! ty (.generalise tcx c)))
    ty)

  (define (create-preview)
    (.create TextPreview ">>''"))

  (define (apply x)
    (.apply g (.apply f x))))

(define (compose f g) (.new Composed f g))

(define TV_A (LambdaMonoVar.new 0))
(define TV_B (LambdaMonoVar.new 1))
(define TV_C (LambdaMonoVar.new 2))

(define VAL_COMPOSE
  (Partial.new
   ">>"
   (LambdaType.new
    [0 1 2]
    (Types.mono-bin-fun
     (Types.mono-fun TV_A TV_B)
     (Types.mono-fun TV_B TV_C)
     (Types.mono-fun TV_A TV_C)))
   2
   (funcref self "compose")
   null))

(class S
  (extends LambdaValue)

  (define f)
  (define g)

  (define (_init fv gv)
    (set! f fv)
    (set! g gv))

  ;; λf g x.(f x)(g x)
  (define (create-preview)
    (.create TextPreview "S''"))

  (define ty)
  (define (get-type)
    (when (== null ty)
      (define tcx (.new TypingCtx))
      (define f-ty (.instantiate tcx (.get-type f)))
      (define g-ty (.instantiate tcx (.get-type g)))
      (define a-ty (LambdaMonoVar.new (.newtype tcx)))
      (define b-ty (LambdaMonoVar.new (.newtype tcx)))
      (define c-ty (LambdaMonoVar.new (.newtype tcx)))
      (.unify tcx null f-ty (Types.mono-bin-fun a-ty b-ty c-ty))
      (.unify tcx null g-ty (Types.mono-fun a-ty b-ty))
      (define res (.compute-substs tcx))
      (assert (.-is-ok res))
      (set! ty (.generalise tcx (Types.mono-fun a-ty c-ty))))
    ty)

  (define (apply x)
    (.apply (.apply f x)
            (.apply g x))))

(define (s f g) (S.new f g))

(define VAL_S
  (Partial.new
   "S"
   (LambdaType.new
    [0 1 2]
    (Types.mono-bin-fun
     (Types.mono-bin-fun TV_A TV_B TV_C)
     (Types.mono-fun TV_A TV_B)
     (Types.mono-fun TV_A TV_C)))
   2
   (funcref self "s")
   null))

(class Id
  (extends LambdaValue)

  (define ty)
  (define (get-type)
    (when (== null ty)
      (set! ty (LambdaType.new [0] (Types.mono-fun Values.TV_A Values.TV_A))))
    ty)

  (define (create-preview)
    (.create TextPreview "I"))

  (define (apply x) x))

(define VAL_I (.new Id))
