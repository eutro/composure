#lang gdlisp

(class-name KeyBinder)

(define key-map {})

(define (load-json x)
  (.clear key-map)
  (for ([e x])
    (set! (ref key-map (ref e 0))
          (Values.from-json (ref e 1)))))

(define (save-json)
  (define js [])
  (for ([k key-map])
    (.append js [k (.to-json (ref key-map k))]))
  js)

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
     (key-map.erase (._map-key key))]
    [else
     (set! (ref key-map (._map-key key)) binding)])
  null)

(define (lookup-key evt-or-key)
  (define key (->input-key evt-or-key))
  (if (== key null)
      null
      (.get key-map (._map-key key) null)))
