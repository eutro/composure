#lang gdlisp

(extends Spatial)
(require threading)

(define (export PackedScene) PuzzleMenu)

(define (export NodePath) shape)
(define (export String) obj-name)
(define (export String) category "Impossible")
(define (export NodePath) fact-path)

(define menu)
(define term)
(define onready has-passed
  (>= (ref Game.puzzle-progress category)
      (len (ref Puzzles.PUZZLES category))))

(define (_on-Interactible-interacted)
  (mount-menu))

(define (_ready)
  (setup-panel)
  (when (not (.is-empty shape))
    (.set-shape $Interactible (get-node shape)))
  null)

(define (setup-panel)
  (define panel $Interactible.panel)
  (.set-passed panel has-passed)
  (.set-text (.get-title panel) obj-name)
  (.set-text
   (.get-text panel)
   (+ (.get-string (get-node fact-path)) "\n"
      "Puzzle: " category "\n"
      (if has-passed "Solved" ""))))

(define (update-term)
  (set! term (.get-term (.get-slot menu))))

(define (set-passed)
  (set! has-passed true)
  (setup-panel))

(define (_on-Menu-puzzle-passed)
  (define just-passed (not has-passed))
  (set-passed)
  (update-term)
  (when just-passed
    (define unlocked (Puzzles.inc-progress category))
    (yield-for 1)
    (when (is-instance-valid Game.ui)
      (.init (.mount (.get-node Game.ui "Unlocked")) unlocked)))
  null)

(define (_on-Menu-puzzle-failed)
  (update-term))

(define (mount-menu)
  (set! menu (.instance PuzzleMenu))
  (.init menu (create-puzzle))
  (.ignore-next-sound (menu.get-slot))
  (.set-term (menu.get-slot) term)
  (.connect menu "puzzle_passed" self "_on_Menu_puzzle_passed")
  (.connect menu "puzzle_failed" self "_on_Menu_puzzle_failed")
  (define tabs (~> Game.ui (.mount-gui) (.get-tabs)))
  (define tab-index (.get-child-count tabs))
  (.add-child tabs menu)
  (.set-tab-title tabs tab-index "Puzzle")
  (set! tabs.current-tab tab-index))

(define loaded-puzzle null)
(define (create-puzzle)
  (when (== loaded-puzzle null)
    (set! loaded-puzzle
          (Puzzles.next-puzzle-for-category category)))
  loaded-puzzle)
