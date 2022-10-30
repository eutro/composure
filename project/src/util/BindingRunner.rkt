#lang gdlisp

(require "../macros.rkt")

(extends Node)

(define active true)

;; _ -> [binding key state-var]
(define active-set {})

(define (_unhandled-input evt)
  (when active
    (define key (InputKeys.evt->input-key evt))
    (when key
      (define lock-type (.get-lock-type key))
      (cond
        [(not lock-type) (dismount-binding key)]
        [else
         (define binding (.lookup-key Game.keys evt))
         (when (!= null binding)
           (mount-binding key binding))])))
  null)

(define (_process _delta)
  (when active
    (for ([k active-set])
      (update-binding (ref active-set k)))))

(define (mount-binding key binding)
  (set! (ref active-set (._map-key key))
        [binding key (.start binding)]))

(define (dismount-binding key)
  (define mk (._map-key key))
  (match (.get active-set mk null)
    [[(var binding) _ (var state)]
     (.finish binding state)])
  (.erase active-set mk))

(define (update-binding entry)
  (match entry
    [[(var binding) (var key) (var state)]
     (.step binding (.resolve-value key) state)]))
