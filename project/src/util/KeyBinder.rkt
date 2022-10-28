#lang gdlisp

(class-name KeyBinder)

(define key-map {})

(define (->input-key evt-or-key)
  (if (is evt-or-key InputKeyBase)
      evt-or-key
      (InputKeys.evt->input-key evt-or-key)))

(define (bind-key evt-or-key binding)
  (define key (->input-key evt-or-key))
  (cond
    [(== key null)
     null]
    [(== binding null)
     (.erase (._map-key key-map) key)]
    [else
     (set! (ref key-map (._map-key key)) binding)])
  null)

(define (lookup-key evt-or-key)
  (define key (->input-key evt-or-key))
  (if (== key null)
      null
      (.get key-map (._map-key key) null)))
