#lang gdlisp

(extends Spatial)

(define (_ready)
  (when (Game.puzzle-completed? "Arithmetic" 0)
    (.set-passed $Puzzle)
    (set! $Puzzle.loaded-puzzle Puzzles.tutorial-puzzle)
    (set! $Puzzle.term (Values.wrap-num 4))))
