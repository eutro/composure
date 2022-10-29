#lang gdlisp

(extends AspectRatioContainer)

(define onready fun (get-node "%Fun"))
(define onready arg (get-node "%Arg"))
(define onready output (get-node "%Output"))
(define onready error (get-node "%Error"))

(define (_on_Fun_term_changed _term)
  (inputs-changed))

(define (_on_Arg_term_changed _term)
  (inputs-changed))

(define (inputs-changed)
  (cond
    [(and (!= null fun.term)
          (!= null arg.term))
     (compute-output fun.term arg.term)]
    [else (.set-term output null)]))

(define (compute-output f x)
  (define res
    (Types.compute-application
     (.get-type f)
     (.get-type x)))
  (cond
    [(.-is-ok res)
     (.set-text error "")
     (.set-term output (.apply f x))]
    [else
     (.set-text error (ref res.value 0))
     (.set-term output null)])
  null)
