#lang gdlisp

(require "../macros.rkt")

(extends Node)

(define active true)

;; _ -> [binding key state-var]
(define active-set {})

(define (_unhandled-input evt)
  (when active (process-key (InputKeys.evt->input-key evt)))
  null)

(define (process-key key)
  (when key
    (define lock-type (.get-lock-type key))
    (cond
      [(not lock-type) (dismount-binding key)]
      [else
       (define binding (.lookup-key Game.keys key))
       (when (!= null binding)
         (mount-binding key binding))])))

(define (_process _delta)
  (when active
    (for ([key (InputKeys.collect-grouped)]) (process-key key))
    (for ([k active-set]) (update-binding (ref active-set k)))))

(define (mount-binding key binding)
  (define mk (._map-key key))
  (when (not (.has active-set mk))
    (set! (ref active-set mk) [binding key (.start binding)])))

(define (dismount-binding key)
  (define mk (._map-key key))
  (match (.get active-set mk null)
    [[(var binding) _ (var state)] (.finish binding state)])
  (.erase active-set mk))

(define (update-binding entry)
  (match entry
    [[(var binding) (var key) (var state)]
     (cond
       [(.get-lock-type key) (.step binding (.resolve-value key) state)]
       [else (dismount-binding key)])])
  null)
