#lang gdlisp

(extends Area)

(define (interact)
  (.emit-signal (get-parent) "interacted"))
