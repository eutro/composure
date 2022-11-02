#lang gdlisp

(extends Control)

(define (_ready)
  (.init (get-node "%Puzzle") Puzzles.tutorial-puzzle))
