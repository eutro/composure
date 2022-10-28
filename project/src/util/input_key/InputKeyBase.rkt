#lang gdlisp

(class-name InputKeyBase)

(define (_map-key)
  null)

(define (_scene)
  null)

(define (instance-display)
  (define scn (.instance (_scene)))
  (.init scn self)
  scn)
