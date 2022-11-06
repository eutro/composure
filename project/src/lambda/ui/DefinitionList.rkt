#lang gdlisp

(class-name DefinitionList)
(extends Control)

(define (export NodePath) list-path)
(define onready list-node (get-node list-path))
(define (export NodePath) scroll-path)
(define onready scroll-node (get-node scroll-path))

(define static (is-entry-unlocked entry)
  (or ((len entry) . < . 3)
      (.call-func (ref entry 2))))

(define (add-entry-list name entries editable)
  (define button (.new Button))
  (set! button.text name)
  (.add-child list-node button)

  (define e-insts [])
  (for ([entry entries])
    (when (is-entry-unlocked entry)
      (define e-inst (.instance (preload "DefinitionEntry.tscn")))
      (set! e-inst.entry entry)
      (set! e-inst.editable editable)
      (.hide e-inst)
      (.add-child list-node e-inst)
      (.append e-insts e-inst)))

  (.connect button "pressed" self "_on_Button_pressed" [e-insts] CONNECT_REFERENCE_COUNTED)
  e-insts)

(define (recompute-children)
  (for ([child (.get-children list-node)])
    (.queue-free child))
  (define custom-entries (add-entry-list "Custom" Game.user-defs true))
  (define i 0)
  (for ([entry custom-entries])
    (.connect entry "entry_changed" self "_on_Entry_entry_changed" [i] CONNECT_REFERENCE_COUNTED)
    (+set! i 1))

  (for ([category Values.CATEGORIES])
    (add-entry-list category (ref Values.CATEGORIES category) false)))

(define (_on_Button_pressed e-insts)
  (for ([e-inst e-insts])
    (fset! e-inst.visible not)))

(define (_ready)
  (recompute-children))

(define (_on_Entry_entry_changed name value custom-idx)
  (define user-entry (ref Game.user-defs custom-idx))
  (set! (ref user-entry 0) name)
  (set! (ref user-entry 1) value)
  (Game.dirty))
