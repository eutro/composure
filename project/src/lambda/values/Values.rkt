#lang gdlisp

(extends Node)

(require "../../macros.rkt")

(begin-escape
  (require (only-in racket/base
                    define-syntax ...
                    begin-for-syntax)
           (for-syntax racket
                       syntax/parse
                       syntax/datum
                       racket/syntax
                       gdlisp/utils))

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

  (begin-for-syntax
    (define-splicing-syntax-class category-clause
      #:attributes (category user-name)
      (pattern {~seq #:category category:expr
                     #:name user-name:str}))
    (define collected-categories (box null)))

  (define-syntax (reset-categories! _stx)
    (set-box! collected-categories null)
    #'(begin))
  (define-syntax (emit-categories! stx)
    (syntax-parse stx
      [(_ category:str ...)
       (syntax-parse (unbox collected-categories)
         [((cat nm val) ...)
          (define category-values
            (for/fold ([vals (hash)])
                      ([entry (in-syntax #'((cat nm val) ...))])
              (define c (syntax->list entry))
              (define cat-name (syntax-e (car c)))
              (hash-set vals cat-name
                        (cons (cdr c)
                              (hash-ref vals cat-name null)))))
          (unless (= (length (syntax->list #'(category ...)))
                     (hash-count category-values))
            (raise-syntax-error
             'emit-categories!
             (format
              "category and order mismatch;\n  order: ~s\n  defined: ~s"
              (syntax->datum #'(category ...))
              (hash-keys category-values))
             stx))
          (syntax-parse (for/list ([cat (in-syntax #'(category ...))])
                          (cons cat (hash-ref category-values (syntax-e cat))))
            [((cat {nm val} ...) ...)
             (syntax/loc stx
               (define CATEGORIES
                 {{~@ cat [[nm val] ...]} ...}))])])]))

  (define-syntax (define-global-val stx)
    (syntax-parse stx
      [(_ name:id value:expr
          {~optional c:category-clause})
       (define name-str (~a (syntax-e #'name)))
       (define caps-name (string->symbol (format "VAL_~a" (string-upcase name-str))))
       (when (datum {~? c #f})
         (set-box! collected-categories
                   (cons #`(c.category c.user-name #,caps-name) (unbox collected-categories))))
       (quasisyntax/loc stx
         (define #,caps-name value))]))

  (define-syntax (define-action stx)
    (syntax-parse stx
      [(_ (name:id arg:id)
          {~optional category:category-clause}
          {~optional {~seq #:description [desc:str ...]}}
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
             {~? (define (_add-tooltip tt) (.append-array tt [desc ...]))}
             (define (start) start_ ...)
             (define (step x s0) step_ ...)
             (define (finish s1) finish_ ...))
           (define-global-val name (.new cn) {~? {~@ . category}})))]))

  (define-syntax (define-construct stx)
    (syntax-parse stx
      [(_ (name:id args:id ...)
          {~optional category:category-clause}
          {~optional {~seq #:description [desc:str ...]}}
          #:class-name class-name:id
          #:short-name short-name:str
          #:type type:expr
          #:body body ...)
       (define arity (length (syntax->list #'(args ...))))
       (define ctor-name (format-id #'name "cons_~a" #'name))
       (define ctor-name-str (mangle (symbol->string (syntax-e ctor-name))))
       (quasisyntax/loc stx
         (begin
           (defrecord class-name (args ...)
             (extends LambdaValue)
             body ...)
           (define (#,ctor-name args ...)
             (.new class-name args ...))
           (define-global-val name
             (Partial.new
              short-name
              {~? [desc ...] []}
              type
              #,arity
              (funcref self #,ctor-name-str)
              null
              0)
             {~? {~@ . category}})))]))

  (define-syntax (define-pure stx)
    (syntax-parse stx
      [(_ (name:id args:id ...)
          {~optional category:category-clause}
          {~optional {~seq #:description [desc:str ...]}}
          #:short-name short-name:str
          #:type type:expr ...+
          #:body
          body ...+)
       (quasisyntax/loc stx
         (begin
           (define (name args ...)
             body ...)
           (define-global-val
             name
             (Partial.new
              short-name
              {~? [desc ...] []}
              (begin type ...)
              #,(length (syntax->list #'(args ...)))
              (funcref self #,(mangle (symbol->string (syntax-e #'name))))
              null
              0)
             {~? {~@ . category}})))])))

(define (auto-type value args)
  (define arg-tys [])
  (for ([arg args])
    (.append arg-tys (.get-type arg)))
  (define res (Types.compute-applications (.get-type value) arg-tys))
  (assert (.-is-ok res))
  (.-value res))

(defrecord Cons (car cdr))

(defrecord Partial
  ([name : String]
   [tooltip : Array #;[String]]
   [type : Type]
   [arity : int]
   [func-ref : FuncRef]
   p-args
   [p-args-len : int])
  (extends LambdaValue)
  (define (get-type) type)
  (text-preview name)
  (define (_add-tooltip tt)
    (.append-array tt tooltip)
    (match p-args-len
      [0 null]
      [1 (.append tt "Applied to 1 argument")]
      [_ (.append tt (+ "Applied to " (str p-args-len) " arguments"))])
    null)
  (define (apply x)
    (cond
      [(arity . <= . 1)
       ;; do the call
       (define args [])
       (let loop ([pa p-args])
         (when (not (== null pa))
           (.append args (.-car pa))
           (recur loop (.-cdr pa))))
       (.invert args)
       (.append args x)
       (.call-funcv func-ref args)]
      [else
       (define res (Types.compute-application type (.get-type x)))
       (assert (.-is-ok res))
       (Partial.new
        (+ name "'")
        tooltip
        (.-value res)
        (- arity 1)
        func-ref
        (.new Cons x p-args)
        (+ 1 p-args-len))])))

(define TV_A (MonoVar.new 0))
(define TV_B (MonoVar.new 1))
(define TV_C (MonoVar.new 2))
(define TV_D (MonoVar.new 3))

(define (wrap-num x) (LambdaWrapper.new x Types.TY_NUM))
(define (wrap-vec3 x) (LambdaWrapper.new x Types.TY_VEC3))
(define (wrap-vec2 x) (LambdaWrapper.new x Types.TY_VEC2))

(reset-categories!)

(define-pure (vec2 x y)
  #:category "Maths" #:name "Vector2"
  #:description ["Vector2 x y : Constructs a 2D vector from components"]
  #:short-name "v2"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_VEC2))
  #:body (wrap-vec2 (Vector2 x.value y.value)))
(define-pure (vec3 x y z)
  #:category "Maths" #:name "Vector3"
  #:description ["Vector3 x y z : Constructs a 3D vector from components"
                 "Note: Y is considered \"up\""]
  #:short-name "v3"
  #:type (Type.new [] (Types.mono-fun Types.MON_NUM (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_VEC3)))
  #:body (wrap-vec3 (Vector3 x.value y.value z.value)))

(define-pure (add a b)
  #:category "Maths" #:name "Add"
  #:description ["Adds two numbers or vectors"]
  #:short-name "+"
  #:type (Type.new {0 {Types.TC_ADD true}} (Types.mono-bin-fun TV_A TV_A TV_A))
  #:body (LambdaWrapper.new (+ a.value b.value) (.get-type a)))

(define-pure (sub a b)
  #:category "Maths" #:name "Sub"
  #:description ["Subtracts two numbers or vectors"]
  #:short-name "-"
  #:type (Type.new {0 {Types.TC_SUB true}} (Types.mono-bin-fun TV_A TV_A TV_A))
  #:body (LambdaWrapper.new (- a.value b.value) (.get-type a)))

(define-pure (mul a b)
  #:category "Maths" #:name "Mul"
  #:description ["Multiplies two numbers or vectors (elementwise)"]
  #:short-name "*"
  #:type (Type.new {0 {Types.TC_MUL true}} (Types.mono-bin-fun TV_A TV_A TV_A))
  #:body (LambdaWrapper.new (* a.value b.value) (.get-type a)))

(define-pure (div a b)
  #:category "Maths" #:name "Div"
  #:description ["Divide two numbers or vectors (elementwise)"
                 "Dividing by zero"]
  #:short-name "/"
  #:type (Type.new {0 {Types.TC_DIV true}} (Types.mono-bin-fun TV_A TV_A TV_A))
  #:body (LambdaWrapper.new (* a.value b.value) (.get-type a)))

(define-pure (scale a b)
  #:category "Maths" #:name "Scale Vector"
  #:description ["Scales a vector by a number"]
  #:short-name "*."
  #:type (Type.new {0 {Types.TC_VEC true}} (Types.mono-bin-fun Types.MON_NUM TV_A TV_A))
  #:body (LambdaWrapper.new (* a.value b.value) (.get-type b)))

(define-pure (magnitude v)
  #:category "Maths" #:name "Vector Magnitude"
  #:description ["Gets the magnitude of a vector"]
  #:short-name "||"
  #:type (Type.new {0 {Types.TC_VEC true}} (Types.mono-fun TV_A Types.MON_NUM))
  #:body (wrap-num (.length v.value)))

(define-pure (normalise v)
  #:category "Maths" #:name "Normalise"
  #:description ["Normalise a vector"]
  #:short-name "||||"
  #:type (Type.new {0 {Types.TC_VEC true}} (Types.mono-fun TV_A TV_A))
  #:body (wrap-num (.normalized v.value)))

(define-pure (dot a b)
  #:category "Maths" #:name "Dot Product"
  #:description ["Computes the dot product of two vectors"]
  #:short-name "⋅"
  #:type (Type.new {0 {Types.TC_VEC true}} (Types.mono-bin-fun TV_A TV_A Types.MON_NUM))
  #:body (wrap-num (.dot a.value b.value)))

(define-pure (cross a b)
  #:category "Maths" #:name "Cross Product"
  #:description ["Computes the cross product of two 3D vectors"
                 "Note: Space is right-handed"]
  #:short-name "×"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_VEC3 Types.MON_VEC3 Types.MON_VEC3))
  #:body (wrap-num (.cross a.value b.value)))

(define-pure (project-xz vec)
  #:category "Maths" #:name "Project XZ"
  #:description ["Projects a 3D vector into the XZ plane, discarding the Y coordinate"]
  #:short-name "XZ"
  #:type (Type.new [] (Types.mono-fun Types.MON_VEC3 Types.MON_VEC2))
  #:body (wrap-vec2 (Vector2 vec.value.x vec.value.z)))
(define-pure (project-xy vec)
  #:category "Maths" #:name "Project XY"
  #:description ["Projects a 3D vector into the XY plane, discarding the Z coordinate"]
  #:short-name "XY"
  #:type (Type.new [] (Types.mono-fun Types.MON_VEC3 Types.MON_VEC2))
  #:body (wrap-vec2 (Vector2 vec.value.x vec.value.y)))
(define-pure (project-yz vec)
  #:category "Maths" #:name "Project YZ"
  #:description ["Projects a 3D vector into the YZ plane, discarding the X coordinate"]
  #:short-name "YZ"
  #:type (Type.new [] (Types.mono-fun Types.MON_VEC3 Types.MON_VEC2))
  #:body (wrap-vec2 (Vector2 vec.value.y vec.value.z)))

(define-pure (plane normal distance)
  #:category "Maths" #:name "Plane"
  #:description ["Plane n d : Constructs a plane with normal n, d away from the origin"]
  #:short-name "Π"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_VEC3 Types.MON_NUM Types.MON_PLANE))
  #:body (LambdaWrapper.new (Plane (.normalized (.-value normal)) (.-value distance)) Types.TY_PLANE))

(define-pure (intersect-plane plane ray)
  #:category "Maths" #:name "Intersect Plane"
  #:description ["Finds the intersection of a plane and a ray"
                 "Returns (0, 0, 0) if they do not intersect"]
  #:short-name "i"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_PLANE Types.MON_RAY Types.MON_VEC3))
  #:body
  (let ([value (.intersects-ray plane.value ray.value.origin ray.value.direction)])
    (wrap-vec3
     (if (== null value)
         (Vector3 0 0 0)
         value))))

(begin-escape
  (define-syntax (define-compose stx)
    (syntax-parse stx
      [(_ name
          category:category-clause
          #:description [desc:str ...]
          #:class-name class-name:id
          #:short-name short-name:str
          #:finished-name finished-name:str
          #:mono-ctor mono-ctor:id
          #:body body:expr ...)
       (syntax/loc stx
         (define-construct (name f g) ;; λx.g(f x)
           {~@ . category}
           #:description [desc ...]
           #:class-name class-name
           #:short-name short-name
           #:type
           (Type.new
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
            (define a (MonoVar.new (.newtype tcx)))
            (define b (MonoVar.new (.newtype tcx)))
            (define c (MonoVar.new (.newtype tcx)))
            (.unify tcx null (mono-ctor a b) f-ty)
            (.unify tcx null (mono-ctor b c) g-ty)
            (define res (.compute-substs tcx))
            (assert (.-is-ok res))
            (.generalise tcx (mono-ctor a c)))
           (text-preview finished-name)
           body ...))])))

(define-compose compose
  #:category "Combinators" #:name "Compose"
  #:description ["Composes two functions, from left to right"]
  #:class-name Compose
  #:short-name ">>"
  #:finished-name ">>''"
  #:mono-ctor Types.mono-fun
  #:body
  (define (apply x) (.apply g (.apply f x))))

(define-compose composea
  #:category "Combinators" #:name "Compose Actions"
  #:description ["Composes two actions, from left to right"]
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
  #:category "Combinators" #:name "Corrupt"
  #:description ["Creates an action from a pure function"]
  #:class-name Corrupt
  #:short-name "♭"
  #:type
  (Type.new
   [0 1]
   (Types.mono-fun
    (Types.mono-fun TV_A TV_B)
    (Types.mono-action TV_A TV_B)))
  #:body
  (cached-type
   (define pure-ty (.get-type f))
   (define mono (.new MonoCtor Types.CTOR_ACTION pure-ty.mono.args))
   (.with-mono pure-ty mono))
  (text-preview "f♭")
  (define (start) null)
  (define (step x _s) (.apply f x))
  (define (finish _s) null))

(define-construct (liftA f a)
  #:category "Combinators" #:name "Lift Action"
  #:description ["Creates a new action that applies a function to its result"
                 "In other words, lift a function to actions"]
  #:class-name LiftA
  #:short-name "LA"
  #:type
  (Type.new
   [0 1 2]
   (Types.mono-fun
    (Types.mono-fun TV_B TV_C)
    (Types.mono-fun
     (Types.mono-action TV_A TV_B)
     (Types.mono-action TV_A TV_C))))
  #:body
  (cached-type (Values.auto-type Values.VAL_LIFTA [f a]))
  (define (start) (.start a))
  (define (step x s) (.apply f (.step a x s)))
  (define (finish s) (.finish a s)))

(define-construct (liftA2 f a1 a2)
  #:category "Combinators" #:name "Lift Action 2"
  #:description ["Combines two actions by applying their results to a function"
                 "In other words, lift a binary function to actions"]
  #:class-name LiftA2
  #:short-name "LA2"
  #:type
  (Type.new
   [0 1 2 3]
   (Types.mono-fun
    (Types.mono-bin-fun TV_B TV_C TV_D)
    (Types.mono-bin-fun
     (Types.mono-action TV_A TV_B)
     (Types.mono-action TV_A TV_C)
     (Types.mono-action TV_A TV_D))))
  #:body
  (cached-type (Values.auto-type Values.VAL_LIFTA2 [f a1 a2]))
  (define (start) [(.start a1) (.start a2)])
  (define (step x s)
    (define b (.step a1 x (ref s 0)))
    (define c (.step a2 x (ref s 1)))
    (.apply (.apply f b) c))
  (define (finish s)
    (.finish a1 (ref s 0))
    (.finish a1 (ref s 1))))

(define-construct (flip f b)
  #:category "Combinators" #:name "Flip"
  #:description ["Flips the arguments of a binary function"]
  #:class-name Flip
  #:short-name "F"
  #:type
  (Type.new
   [0 1 2]
   (Types.mono-fun
    (Types.mono-bin-fun TV_A TV_B TV_C)
    (Types.mono-bin-fun TV_B TV_A TV_C)))
  #:body
  (text-preview "F''")
  (cached-type (Values.auto-type Values.VAL_FLIP [f b]))
  (define (apply a)
    (.apply (.apply f a) b)))

(define-construct (uncurry f)
  #:category "Combinators" #:name "Uncurry"
  #:description ["Converts a binary function into a unary function"
                 "accepting a pair as its argument"]
  #:class-name Uncurry
  #:short-name "UCr"
  #:type
  (Type.new
   [0 1 2]
   (Types.mono-fun
    (Types.mono-bin-fun TV_A TV_B TV_C)
    (Types.mono-fun (Types.mono-pair TV_A TV_B) TV_C)))
  #:body
  (text-preview "UCr'")
  (cached-type (Values.auto-type Values.VAL_UNCURRY [f]))
  (define (apply pair)
    (.apply (.apply f pair.car) pair.cdr)))

(define-construct (curry f a)
  #:category "Combinators" #:name "Curry"
  #:description ["Converts a unary function of a pair to a binary function"]
  #:class-name Curry
  #:short-name "Cr"
  #:type
  (Type.new
   [0 1 2]
   (Types.mono-fun
    (Types.mono-fun (Types.mono-pair TV_A TV_B) TV_C)
    (Types.mono-bin-fun TV_A TV_B TV_C)))
  #:body
  (text-preview "Cr''")
  (cached-type (Values.auto-type Values.VAL_CURRY [f a]))
  (define (apply b)
    (.apply f (Cons.new a b))))

(define-construct (s f g) ;; λf g x.(f x)(g x)
  #:category "Combinators" #:name "S Combinator"
  #:class-name S
  #:short-name "S"
  #:type
  (Type.new
    [0 1 2]
    (Types.mono-bin-fun
     (Types.mono-bin-fun TV_A TV_B TV_C)
     (Types.mono-fun TV_A TV_B)
     (Types.mono-fun TV_A TV_C)))
  #:body
  (text-preview "S''")
  (cached-type (Values.auto-type Values.VAL_S [f g]))
  (define (apply x)
    (.apply (.apply f x)
            (.apply g x))))

(define-construct (k value)
  #:category "Combinators" #:name "Constant"
  #:description ["Creates a function that always returns the given value"]
  #:class-name K
  #:short-name "K"
  #:type (Type.new [0 1] (Types.mono-bin-fun TV_A TV_B TV_A))
  #:body
  (cached-type (Values.auto-type Values.VAL_K [value]))
  (text-preview "K'")
  (define (apply _x) value))

(class Id
  (extends LambdaValue)
  (cached-type (Type.new [0] (Types.mono-fun Values.TV_A Values.TV_A)))
  (text-preview "I")
  (define (_add-tooltip tt) (.append tt "Returns its argument"))
  (define (apply x) x))
(define-global-val i (.new Id)
  #:category "Combinators" #:name "Identity")

(define-construct (passthrough f) ;; `first` is a really really bad name
  #:category "Combinators" #:name "Passthrough"
  #:description ["Creates an action with an extra untouched argument"]
  #:class-name Passthrough
  #:short-name "P"
  #:type
  (let ([tv-s (MonoVar.new (- (ord "s") (ord "a")))])
    (Type.new
     [0 1 (.-no tv-s)]
     (Types.mono-fun
      (Types.mono-action TV_A TV_B)
      (Types.mono-action (Types.mono-pair TV_A tv-s)
                         (Types.mono-pair TV_B tv-s)))))
  #:body
  (cached-type (Values.auto-type Values.VAL_PASSTHROUGH [f]))
  (define (start) (.start f))
  (define (step x s) [(.step f (ref x 0) s) (ref x 1)])
  (define (finish s) (.finish f s)))

(define-action (move vel)
  #:category "Actions" #:name "Move"
  #:description ["Move the player in the specified direction"]
  #:class-name Move
  #:type (Type.new [] (Types.mono-action Types.MON_VEC2 Types.MON_UNIT))
  #:preview (.create TextPreview "mv")
  #:start () [(Vector2 0 0)]
  #:step (x s)
  (define last-move (ref s 0))
  (define cur-move x.value)
  (set! (ref s 0) cur-move)
  (Game.world.player.user-move cur-move last-move)
  Values.VAL_UNIT
  #:finish (s)
  (Game.world.player.user-move (Vector2 0 0) (ref s 0))
  null)

(define-action (prn x)
  #:category "Actions" #:name "Print"
  #:description ["Show the value on screen, and return it"]
  #:class-name Print
  #:type (Type.new [0] (Types.mono-action Values.TV_A Types.MON_UNIT))
  #:preview (.create TextPreview "prn")
  #:start () null
  #:step (x _s)
  (print (if (is x LambdaWrapper) x.value "<λ>"))
  x
  #:finish (_s) null)

(define-action (get-player-posn _u)
  #:category "Actions" #:name "Player Position"
  #:description ["Get the position of the player in the world"]
  #:class-name GetPlayerPosn
  #:type (Type.new [0] (Types.mono-action Values.TV_A Types.MON_VEC3))
  #:preview (.create TextPreview "@p")
  #:start () null
  #:step (_x _s) (Values.wrap-vec3 (.get-position Game.world.player))
  #:finish (_s) null)

(define-action (get-mouse-posn _u)
  #:category "Actions" #:name "Mouse Position"
  #:description ["Get the position of the mouse on the screen"
                 "Note: the center of the screen is (0, 0),"
                 "top left is (-1, -1) and bottom right is (1, 1)"
                 "It is possible to get values greater than 1"
                 "by moving the mouse out of the window"]
  #:class-name GetMousePosn
  #:type (Type.new [0] (Types.mono-action Values.TV_A Types.MON_VEC2))
  #:preview (.create TextPreview "@m")
  #:start () null
  #:step (_x _s) (Values.wrap-vec2 (InputKeyMouse.normalise-pos (.get-mouse-position (Game.get-viewport))))
  #:finish (_s) null)

(define-action (camera-project pos)
  #:category "Actions" #:name "Camera Project"
  #:description ["Get the projection from the camera of a position on the screen"
                 "Note: the center of the screen is (0, 0),"
                 "top left is (-1, -1) and bottom right is (1, 1)"]
  #:class-name CameraProject
  #:type (Type.new [] (Types.mono-action Types.MON_VEC2 Types.MON_RAY))
  #:preview (.create TextPreview "⨀")
  #:start () null
  #:step (pos _s)
  (define denorm-pos (InputKeyMouse.denormalise-pos pos.value))
  (LambdaWrapper.new
   {
    "origin" (Game.world.camera.project-ray-origin denorm-pos)
    "direction" (Game.world.camera.project-ray-normal denorm-pos)
    }
   Types.TY_RAY)
  #:finish (_s) null)

(define-action (psctxzprtp vec) ;; Project Screen Coordinate to XZ Plane Relative to Player
  #:category "Actions" #:name "PSCtXZPRtP"
  #:description ["Project a screen coordinate to the XZ plane (floor), relative to the player"]
  #:class-name PSCtXZPRtP
  #:type (Type.new [] (Types.mono-action Types.MON_VEC2 Types.MON_VEC2))
  #:preview (.create TextPreview ":)")
  #:start () null
  #:step (vec _s)
  (define denorm (InputKeyMouse.denormalise-pos vec.value))
  (define plane (Plane Vector3.UP 0))
  (define intersection
    (.intersects-ray
     plane
     (Game.world.camera.project-ray-origin denorm)
     (Game.world.camera.project-ray-normal denorm)))
  (when (== null intersection)
    (set! intersection (Vector3 0 0 0)))
  (-set! intersection Game.world.player.transform.origin)
  (Values.wrap-vec2 (Vector2 intersection.x intersection.z))
  #:finish (_s) null)

(define-pure (cons a b)
  #:category "Misc" #:name "Cons"
  #:description ["Creates a pair"]
  #:short-name ":"
  #:type (Type.new [0 1] (Types.mono-bin-fun TV_A TV_B (Types.mono-pair TV_A TV_B)))
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

(class Unit
  (extends LambdaValue)
  (define (get-type) Types.TY_UNIT)
  (text-preview "()"))
(define-global-val unit (.new Unit)
  #:category "Misc" #:name "Unit")

(emit-categories! "Maths" "Combinators" "Actions" "Misc")
