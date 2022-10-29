#lang gdlisp

(extends Control)

(define (export NodePath) button-path)
(define (export NodePath) display-path)
(define (export NodePath) slot-path)
(define (export NodePath) hint-path)

(define awaiting-input false)
(define setting-up false)

(define current-key null)
(define expected-type null)

(define (_on-Button-pressed)
  (.hide (get-node button-path))
  (set! awaiting-input true)
  (set-current-key null)
  (grab-focus)
  null)

(define (_gui-input evt)
  (when awaiting-input
    (define key (InputKeys.evt->input-key evt))
    (when key
      (set! awaiting-input false)
      (.show (get-node button-path))
      (set-current-key key)
      (accept-event))))

(define (set-current-key key)
  (set! current-key key)
  (define display-node (get-node display-path))
  (for ([child (.get-children display-node)])
    (.queue-free child))

  (cond
    [(!= null key)
     (set! setting-up true)
     (.add-child display-node (.instance-display key))
     (.show (get-node slot-path))
     (setup-type key)
     (.set-term (get-node slot-path) (Game.keys.lookup-key key))
     (set! setting-up false)]

    [else
     (.hide (get-node slot-path))
     (.set-term (get-node slot-path) null)
     (set! expected-type null)
     (.set-type (get-node hint-path) null)])
  null)

(define (setup-type key)
  (define input-type (._lambda-type key))
  (set! expected-type
        (.new
         LambdaType [0]
         (Types.mono-action
          input-type.mono
          Values.TV_A)))
  (.set-type (get-node hint-path) expected-type)
  (.set-type (get-node slot-path) expected-type)
  null)

(define (_on_BoundLambda_term_changed term)
  (when (and (!= null current-key)
             (not setting-up))
    (Game.keys.bind-key current-key term)))
