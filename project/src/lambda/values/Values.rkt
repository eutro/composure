#lang gdlisp

(extends Node)

(require "../../macros.rkt")

(begin-escape
  (require (only-in racket/base define-syntax ...)
           (for-syntax racket
                       syntax/parse
                       racket/syntax))
  (define-syntax-rule (cached-type body ...)
    (begin
      (define type)
      (define (get-type)
        (when (== null type)
          (set! type (begin body ...)))
        type)))

  (define-syntax-rule (text-preview name)
    (define (create-preview)
      (.create TextPreview name)))

  (define-syntax (define-caps-val stx)
    (syntax-parse stx
      [(_ name:id value:expr)
       (define name-str (~a (syntax-e #'name)))
       (define caps-name (string->symbol (format "VAL_~a" (string-upcase name-str))))
       (quasisyntax/loc stx
         (define #,caps-name value))]))

  (define-syntax (define-action stx)
    (syntax-parse stx
      [(_ (name:id arg:id)
          #:class-name cn:id
          #:type {~seq type:expr ...+}
          #:preview preview:expr
          #:start () start_:expr ...+
          #:step (x:id s0:id) step_:expr ...+
          #:finish (s1:id) finish_:expr ...+)
       (syntax/loc stx
         (begin
           (class cn
             (extends LambdaValue)
             (cached-type type ...)
             (define (create-preview) preview)
             (define (start) start_ ...)
             (define (step x s0) step_ ...)
             (define (finish s1) finish_ ...))
           (define-caps-val name (.new cn))))]))

  (define-syntax (define-construct stx)
    (syntax-parse stx
      [(_ (name:id args:id ...)
          #:class-name class-name:id
          #:short-name short-name:str
          #:type type:expr
          #:body body ...)
       (define arity (length (syntax->list #'(args ...))))
       (define ctor-name (format-id #'name "cons_~a" #'name))
       (define ctor-name-str (symbol->string (syntax-e ctor-name)))
       (quasisyntax/loc stx
         (begin
           (defrecord class-name (args ...)
             (extends LambdaValue)
             body ...)
           (define (#,ctor-name args ...)
             (.new class-name args ...))
           (define-caps-val name
             (Partial.new
              short-name
              type
              #,arity
              (funcref self #,ctor-name-str)
              null))))]))

  (define-syntax (define-pure stx)
    (syntax-parse stx
      [(_ (name:id args:id ...)
          #:short-name short-name:str
          #:type type:expr ...+
          #:body
          body ...+)
       (quasisyntax/loc stx
         (begin
           (define (name args ...)
             body ...)
           (define-caps-val
             name
             (Partial.new
              short-name
              (begin type ...)
              #,(length (syntax->list #'(args ...)))
              (funcref self #,(symbol->string (syntax-e #'name)))
              null))))])))

(defrecord Cons (car cdr))

(defrecord Partial
  ([name : String]
   [type : LambdaType]
   [arity : int]
   [func-ref : FuncRef]
   p-args)
  (extends LambdaValue)
  (define (get-type) type)
  (text-preview name)
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

(define TV_A (LambdaMonoVar.new 0))
(define TV_B (LambdaMonoVar.new 1))
(define TV_C (LambdaMonoVar.new 2))

(define (wrap-num x) (LambdaWrapper.new x Types.TY_NUM))
(define (wrap-vec3 x) (LambdaWrapper.new x Types.TY_VEC3))
(define (wrap-vec2 x) (LambdaWrapper.new x Types.TY_VEC2))

(define-pure (vec3 x y z)
  #:short-name "v3"
  #:type (LambdaType.new [] (Types.mono-fun Types.MON_NUM (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_VEC3)))
  #:body (wrap-vec3 (Vector3 (.-value x) (.-value y) (.-value z))))
(define-pure (vec2 x y)
  #:short-name "v2"
  #:type (LambdaType.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_VEC2))
  #:body (wrap-vec2 (Vector2 (.-value x) (.-value y))))

;; sorry no type classes
(define-pure (add3 a b)
  #:short-name "+3"
  #:type (LambdaType.new [] (Types.mono-bin-fun Types.MON_VEC3 Types.MON_VEC3 Types.MON_VEC3))
  #:body (wrap-vec3 (+ (.-value a) (.-value b))))

(define-pure (add2 a b)
  #:short-name "+2"
  #:type (LambdaType.new [] (Types.mono-bin-fun Types.MON_VEC2 Types.MON_VEC2 Types.MON_VEC2))
  #:body (wrap-vec2 (+ (.-value a) (.-value b))))

(define-pure (add a b)
  #:short-name "+"
  #:type (LambdaType.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:body (wrap-num (+ (.-value a) (.-value b))))

(define-pure (cons a b)
  #:short-name ":"
  #:type (LambdaType.new [0 1] (Types.mono-bin-fun TV_A TV_B (Types.mono-pair TV_A TV_B)))
  #:body
  (LambdaWrapper.new
   (Cons.new a b)
   (let ([a-ty (.get-type a)]
         [b-ty (.get-type b)]
         [tcx (.new TypingCtx)])
     (tcx.generalise
      (Types.mono-pair
       (.instantiate tcx a-ty)
       (.instantiate tcx b-ty))))))

(begin-escape
  (define-syntax-rule (define-compose name
                        #:class-name class-name
                        #:short-name short-name
                        #:finished-name finished-name
                        #:mono-ctor mono-ctor
                        #:body body ...)
    (define-construct (name f g) ;; λx.g(f x)
      #:class-name class-name
      #:short-name short-name
      #:type
      (LambdaType.new
       [0 1 2]
       (Types.mono-bin-fun
        (mono-ctor TV_A TV_B)
        (mono-ctor TV_B TV_C)
        (mono-ctor TV_A TV_C)))
      #:body
      (cached-type
       (define tcx (.new TypingCtx))
       (define f-ty (.instantiate tcx (.get-type f)))
       (define g-ty (.instantiate tcx (.get-type g)))
       (define a (LambdaMonoVar.new (.newtype tcx)))
       (define b (LambdaMonoVar.new (.newtype tcx)))
       (define c (LambdaMonoVar.new (.newtype tcx)))
       (.unify tcx null (mono-ctor a b) f-ty)
       (.unify tcx null (mono-ctor b c) g-ty)
       (define res (.compute-substs tcx))
       (assert (.-is-ok res))
       (.generalise tcx (mono-ctor a c)))
      (text-preview finished-name)
      body ...)))

(define-compose compose
  #:class-name Compose
  #:short-name ">>"
  #:finished-name ">>''"
  #:mono-ctor Types.mono-fun
  #:body
  (define (apply x) (.apply g (.apply f x))))

(define-construct (s f g) ;; λf g x.(f x)(g x)
  #:class-name S
  #:short-name "S"
  #:type
  (LambdaType.new
    [0 1 2]
    (Types.mono-bin-fun
     (Types.mono-bin-fun TV_A TV_B TV_C)
     (Types.mono-fun TV_A TV_B)
     (Types.mono-fun TV_A TV_C)))
  #:body
  (text-preview "S''")
  (cached-type
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
   (.generalise tcx (Types.mono-fun a-ty c-ty)))
  (define (apply x)
    (.apply (.apply f x)
            (.apply g x))))

(define-construct (k value)
  #:class-name K
  #:short-name "K"
  #:type (LambdaType.new [0 1] (Types.mono-bin-fun TV_A TV_B TV_A))
  #:body
  (cached-type
   (define tcx (.new TypingCtx))
   (.generalise tcx (Types.mono-fun Values.TV_A (.instantiate tcx (.get-type value)))))
  (text-preview "K'")
  (define (apply _x) value))

(class Id
  (extends LambdaValue)
  (cached-type (LambdaType.new [0] (Types.mono-fun Values.TV_A Values.TV_A)))
  (text-preview "I")
  (define (apply x) x))
(define VAL_I (.new Id))

(class Unit
  (extends LambdaValue)
  (define (get-type) Types.TY_UNIT)
  (text-preview "()"))
(define VAL_UNIT (.new Unit))


;; ACTIONS

(define-compose composea
  #:class-name ComposeA
  #:short-name ">>>"
  #:finished-name ">>>''"
  #:mono-ctor Types.mono-action
  #:body
  (define (start) [(.start f) (.start g)])
  (define (step x s)
    (.step g (.step f x (ref s 0)) (ref s 1)))
  (define (finish s)
    (.finish f (ref s 0))
    (.finish g (ref s 1))))

(define-construct (corrupt f) ;; I think this is catchier than `arr`
  #:class-name Corrupt
  #:short-name "♭"
  #:type
  (LambdaType.new
   [0 1]
   (Types.mono-fun
    (Types.mono-fun TV_A TV_B)
    (Types.mono-action TV_A TV_B)))
  #:body
  (cached-type
   (define pure-ty (.get-type f))
   (define mono (.new LambdaMonoCtor Types.CTOR_ACTION pure-ty.mono.args))
   (.new LambdaType pure-ty.type-vars mono))
  (text-preview "f♭")
  (define (start) null)
  (define (step x _s) (.apply f x))
  (define (finish _s) null))

(define-construct (passthrough f) ;; `first` is a really really bad name
  #:class-name Passthrough
  #:short-name "P"
  #:type
  (let ([tv-s (LambdaMonoVar.new (- (ord "s") (ord "a")))])
    (LambdaType.new
     [0 1 (.-no tv-s)]
     (Types.mono-fun
      (Types.mono-action TV_A TV_B)
      (Types.mono-action (Types.mono-pair TV_A tv-s)
                         (Types.mono-pair TV_B tv-s)))))
  #:body
  (cached-type
   (define tcx (.new TypingCtx))
   (define dir-ty (.get-type f))
   (define tv-a (.newtype tcx))
   (define tv-b (.newtype tcx))
   (define tv-s (.newtype tcx))
   (.unify tcx null
           (Types.mono-action tv-a tv-b)
           (.instantiate dir-ty))
   (define s (.compute-substs tcx))
   (assert (.-is-ok s))
   (tcx.generalise
    (Types.mono-action
     (Types.mono-pair tv-a tv-s)
     (Types.mono-pair tv-b tv-s))))
  (define (start) (.start f))
  (define (step x s) [(.step f (ref x 0) s) (ref x 1)])
  (define (finish s) (.finish f s)))

(define-action (move vel)
  #:class-name Move
  #:type (LambdaType.new [] (Types.mono-action Types.MON_VEC2 Types.MON_UNIT))
  #:preview (.create TextPreview "mv")
  #:start () null
  #:step (x s)
  (Game.world.player.user-move (.-value x))
  Values.VAL_UNIT
  #:finish (s)
  (Game.world.player.user-move (Vector2 0 0))
  null)

(define-action (prn x)
  #:class-name Print
  #:type (LambdaType.new [0] (Types.mono-action Values.TV_A Types.MON_UNIT))
  #:preview (.create TextPreview "prn")
  #:start () null
  #:step (x _s)
  (print (if ("value" . in . x) (.-value x) "λ?"))
  Values.VAL_UNIT
  #:finish (_s) null)

(define-action (get-mouse-posn _u)
  #:class-name GetMousePosn
  #:type (LambdaType.new [] (Types.mono-action Types.TY_UNIT Types.TY_VEC2))
  #:preview (.create TextPreview "⨀")
  #:start () null
  #:step (x _s) (.get-mouse-position (Game.get-viewport))
  #:finish (_s) null)

(define-action (camera-project pos)
  #:class-name CameraProject
  #:type (LambdaType.new [] (Types.mono-action Types.TY_VEC2 Types.TY_HALFLINE))
  #:preview (.create TextPreview "⨀")
  #:start () null
  #:step (pos _s)
  (LambdaWrapper.new
   {
    "origin" (.project-ray-origin Game.world.camera pos.value)
    "direction" (.project-ray-normal Game.world.camera pos.value)
    }
   Types.TY_HALFLINE)
  #:finish (_s) null)
