#lang gdlisp

(extends Control)

(define (set-text text)
  (.set-text $Panel/Label text))
