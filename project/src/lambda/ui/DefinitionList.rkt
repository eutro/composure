#lang gdlisp

(extends Control)

(define (export NodePath) list-path)
(define onready list-node (get-node list-path))
(define (export NodePath) scroll-path)
(define onready scroll-node (get-node scroll-path))

(define (add-entry-list name entries editable)
  (define button (.new Button))
  (set! button.text name)
  (.add-child list-node button)

  (define e-insts [])
  (for ([entry entries])
    (define e-inst (.instance (preload "DefinitionEntry.tscn")))
    (set! e-inst.entry entry)
    (set! e-inst.editable editable)
    (.add-child list-node e-inst)
    (.append e-insts e-inst))

  (.connect button "pressed" self "_on_Button_pressed" [e-insts] CONNECT_REFERENCE_COUNTED)
  e-insts)

(define (_on_Button_pressed e-insts)
  (for ([e-inst e-insts])
    (fset! e-inst.visible not)))

(define (_ready)
  (define custom-entries (add-entry-list "Custom" Game.user-defs true))
  (define i 0)
  (for ([entry custom-entries])
    (.connect entry "entry_changed" self "_on_Entry_entry_changed" [i] CONNECT_REFERENCE_COUNTED)
    (+set! i 1))

  (for ([category Values.CATEGORIES])
    (add-entry-list category (ref Values.CATEGORIES category) false))

  (for ([child (.get-children list-node)])
    (connect-for-focus child child)))

(define (connect-for-focus node imm-child)
  (when (!= (.get node "focus_mode") FOCUS_NONE)
    (.connect node "focus_entered" self "_on_Node_focused" [imm-child] CONNECT_REFERENCE_COUNTED))
  (for ([child (.get-children node)])
    (when (is child Control)
      (connect-for-focus child imm-child))))

(define (_on_Entry_entry_changed name value custom-idx)
  (define user-entry (ref Game.user-defs custom-idx))
  (set! (ref user-entry 0) name)
  (set! (ref user-entry 1) value))

(define (_on_Node_focused node)
  (define scroll-pos scroll-node.scroll-vertical)
  (define rect-pos node.rect-position.y)
  (define min-visible scroll-pos)
  (define max-visible (- (+ scroll-pos scroll-node.rect-size.y)
                         node.rect-size.y))

  (define is-in-bounds
    (and (min-visible . <= . rect-pos)
         (rect-pos . <= . max-visible)))

  (when (not is-in-bounds)
    (define scroll-delta
      (if (rect-pos . < . min-visible)
          (- rect-pos min-visible)
          (- rect-pos max-visible)))
    (+set! scroll-node.scroll-vertical scroll-delta)))
