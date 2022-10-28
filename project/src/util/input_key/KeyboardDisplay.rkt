#lang gdlisp

(extends Control)

(define (init key)
  (set! $Label.text (OS.get_scancode_string key.event.scancode))
  null)
