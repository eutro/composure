#lang gdlisp

(extends Control)
(require "../macros.rkt" threading)

(define (export Font) label-font)

(define (_on_ReplayTutorial_pressed)
  (Game.ui.start-tutorial false))

(define (_on_QTTS_pressed)
  (Game.quit))

(define (_on_Reset_pressed)
  (define creset (get-node "%ConfirmReset"))
  (.popup creset)
  (yield creset "confirmed")
  (Game.reset))

(define (_ready)
  (define progress $MC/VB/Progress)
  (.set-columns progress (len Puzzles.PUZZLE_TYPES))
  (for ([cat Puzzles.PUZZLE_TYPES])
    (progress.add-child
     (doto
      (.new Label)
      (.set "custom_fonts/font" label-font)
      (~> .-size_flags_horizontal (bit-or-set! (<< 1 (- SIZE_EXPAND 1))))
      (.set-text cat))))
  (for ([cat Puzzles.PUZZLE_TYPES])
    (progress.add-child
     (doto
      (.new Label)
      (.set-text
       (+ (str (.get Game.puzzle-progress cat 0))
          "/"
          (str (len (ref Puzzles.PUZZLES cat)))))))))
