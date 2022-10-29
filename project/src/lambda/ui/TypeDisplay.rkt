#lang gdlisp

(extends Control)

(define type : LambdaType
  (.new
   LambdaType [0]
   (.new
    LambdaMonoCtor
    Types.CTOR_FUN
    [(.new LambdaMonoVar 0)
     (.new LambdaMonoVar 0)])))

(define computed-children false)

(define (init [ty : LambdaType])
  (set! type ty)
  (compute-children))

(define (_ready)
  (compute-children))

(define (compute-children)
  (when (not computed-children)
    (cond
      [((len (.-type-vars type)) . > . 0)
       (define tv-cont (get-node "%TVs"))
       (for ([tv (.-type-vars type)])
         (.add-child tv-cont (var-display tv)))]
      [else
       (.hide $TC/PolyPart)])

    (add-mono (.-mono type) false)
    (set! computed-children true)))

(define (var-display vr)
  (str-display (.name-for LambdaMonoVar vr)))

(define (ctor-display ctor)
  (str-display (.-name ctor)))

(define (space-node)
  (str-display " "))

(define (str-display strng)
  (define node (.new Label))
  (set! (.-text node) strng)
  node)

(define (add-mono mn brackets)
  (cond
    [(is mn LambdaMonoVar)
     (.add-child $TC (var-display (.-no mn)))]
    [else
     (assert (is mn LambdaMonoCtor))

     (define ctor (.-ctor mn))
     (define args (.-args mn))

     (when (<= 0 (len args))
       (set! brackets false))

     (when brackets
       (.add-child $TC (str-display "(")))

     (define ctor-node (ctor-display ctor))
     (cond
       [(.-infix ctor)
        (assert (== 2 (len args)))
        (add-mono (ref args 0) true)
        (.add-child $TC (space-node))
        (.add-child $TC ctor-node)
        (.add-child $TC (space-node))
        (add-mono (ref args 1) true)]
       [else
        (.add-child $TC ctor-node)
        (for ([arg args])
          (.add-child $TC (space-node))
          (add-mono arg true))])

     (when brackets
       (.add-child $TC (str-display ")")))])
  null)
