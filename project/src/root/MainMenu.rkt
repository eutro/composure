#lang gdlisp

(extends Control)

(signal play-pressed)

(define (export AudioStream) track)

(define (_ready)
  (Game.queue-track track)
  (.call-deferred $VB/Play "grab_focus"))

(define (_on_Play_pressed)
  (Game.fade-out-music)
  (emit-signal "play_pressed"))

(define (_on_Settings_pressed)
  (define settings (.mount $Settings))
  (yield settings "close_pressed")
  (.unmount $Settings))

(define (_on_Source_pressed)
  (OS.shell-open "https://github.com/eutro/composure"))
