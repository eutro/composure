#lang gdlisp

(extends Node)

(define (start-game)
  (add-child (.instance (preload "InGame.tscn"))))

(define (start-main-menu)
  (define mm (.instance (preload "MainMenu.tscn")))
  (add-child mm)
  (.connect mm "play_pressed" self "_on_MainMenu_play_pressed"))

(define (_ready)
  (start-main-menu)
  (.connect Game "quit" self "_on_Game_quit"))

(define (_on_Game_quit)
  (.queue-free $InGame)
  (start-main-menu))

(define (_on_MainMenu_play_pressed)
  (.queue-free $MainMenu)
  (start-game))
