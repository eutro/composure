#lang gdlisp

(extends Control)

(define puzzle)

(signal puzzle-passed)
(signal puzzle-failed)

(define (init puzzle-in)
  (set! puzzle puzzle-in)
  (setup))

(define (setup)
  (.set-type $TckLambdaSlot puzzle.expected-type)
  (.set-text $TypeHint (str puzzle.expected-type))
  (.set-bbcode (get-node "%PuzzleText") puzzle.description)
  (.set-stream $Music puzzle.track)
  (when (is-inside-tree)
    (.play $Music)))

(define (_on-TckLambdaSlot-term-changed term)
  (.set-text $Error "")
  (when (!= null term)
    (define res (.call-func puzzle.check term))
    (cond
      [res.is-ok
       (.set-emitting $TckLambdaSlot/Particles true)
       (.play $Success)
       (emit-signal "puzzle_passed")]
      [else
       (.set-text $Error res.value)
       (.play $Failure)
       (emit-signal "puzzle_failed")]))
  null)
