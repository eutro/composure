#lang gdlisp

(extends AspectRatioContainer)

(define (export Array NodePath) args)

(define onready output (get-node "%Output"))
(define onready error (get-node "%Error"))

(define (_ready)
  (when (!= null args)
    (for ([arg args])
      (.connect (get-node arg) "term_changed" self "on_term_changed"))))

(define (on-term-changed _term)
  (inputs-changed)
  null)

(define (inputs-changed)
  (.set-term output null)

  ;; we _can_ fold if we have at least two terms to apply
  (define can-fold
    (and ((len args) . >= . 2)
         (!= null (.-term (get-node (ref args 0))))
         (!= null (.-term (get-node (ref args 1))))))

  ;; check that there's no nulls before another term...
  (define seen-null false)
  (for ([i (range 2 (len args))])
    (define is-null (== null (.-term (get-node (ref args i)))))
    (cond
      [(and is-null seen-null)
       (set! can-fold false)
       break]
      [is-null (set! seen-null true)]))

  (when can-fold (compute-output))

  null)

(define (compute-output)
  (define acc-val (.-term (get-node (ref args 0))))
  (define acc-ty (.get-type acc-val))
  (for ([i (range 1 (len args))])
    (define x (.-term (get-node (ref args i))))
    (when (== null x)
      (#%gdscript "break"))
    (define res
      (Types.compute-application
       acc-ty
       (.get-type x)))
    (cond
      [(.-is-ok res)
       (.set-text error "")
       (set! acc-ty (.-value res))
       (fset! acc-val .apply x)]
      [else
       (.set-text error (ref res.value 0))
       (#%gdscript "return")]))
  (.set-term output acc-val))
