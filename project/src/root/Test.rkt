#lang gdlisp

(extends Node)

(define (_ready)
  (for ([x [Values.VAL_UNIT Values.VAL_UNCURRY Values.VAL_CONS]])
    (define js (.to-json x))
    (define vl (Values.from-json js))
    (print (JSON.print js))
    (print vl))
  null)
