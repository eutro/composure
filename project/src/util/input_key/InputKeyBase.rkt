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

(define (get-lock-type)
  false
  ;; true -> pressed
  ;; false -> released
  )

(define (_lambda-type)
  ;; type this input can provide the user
  null)

(define (resolve-value)
  ;; get the value to provide the user, must be a (_lambda-type)
  null)
