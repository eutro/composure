#lang gdlisp

(extends Node)

(define active true)

(define active-set {})

(define (_unhandled-input evt)
  (when active
    (define key (InputKeys.evt->input-key evt))
    (when key
      (define lock-type (.get-lock-type key))
      (cond
        [(not lock-type)
         (.erase active-set (._map-key key))]
        [else
         (define binding (.lookup-key Game.keys evt))
         (when (!= null binding)
           (set! (ref active-set (._map-key key)) [binding key]))])))
  null)

(define (_process _delta)
  (when active
    (for ([k active-set])
      (match (ref active-set k)
        [[(var binding) (var key)]
         (.apply binding (.resolve-value key))]))))
