#lang gdlisp

(extends Control)

(define onready cur-cont (get-node "%CurrentContainer"))

(define (_input evt)
  (define key (InputKeys.evt->input-key evt))
  (when (!= null key)
    (.set-input-as-handled (get-tree))

    (define new-input (.instance-display key))
    (define cur-input (.get-child cur-cont 0))
    (.remove-child cur-cont cur-input)
    (.add-child cur-cont new-input)
    (.queue-free cur-input)
    (.queue-sort cur-cont))
  null)
