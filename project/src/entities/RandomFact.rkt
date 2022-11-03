#lang gdlisp

(extends Node)

(define (export float) min-value)
(define (export float) max-value)
(define (export float) step)

(define (export String) value-name)
(define (export String) units)
(define value)

(define (_ready)
  (generate))

(define (generate)
  (define step-count (+ (int (/ (- max-value min-value) step)) 1))
  (define rand-step (floor (* step-count (randf))))
  (set! value (+ min-value (* step rand-step))))

(define (get-string)
  (+ value-name ": " (str value) units))
