#lang gdlisp

(extends Spatial)
(require threading)

(define (export PackedScene) PuzzleMenu)

(define menu)

(define (_on_Interactible_interacted)
  (mount-menu))

(define (mount-menu)
  (set! menu (.instance PuzzleMenu))
  (.init menu (create-puzzle))
  (define tabs (~> Game.ui (.mount-gui) (.get-tabs)))
  (define tab-index (.get-child-count tabs))
  (.add-child tabs menu)
  (.set-tab-title tabs tab-index "Puzzle")
  (set! tabs.current-tab tab-index))

(define (create-puzzle)
  Puzzles.tutorial-puzzle)
