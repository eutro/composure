#lang gdlisp

(extends Spatial)

(define (set-passed passed)
  (cond
    [passed
     (.hide $Unpassed)
     (.show $Passed)]
    [else
     (.show $Unpassed)
     (.hide $Passed)]))

(define (get-title)
  $Title)

(define (get-text)
  $Text)
