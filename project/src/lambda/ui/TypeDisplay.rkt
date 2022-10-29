#lang gdlisp

(extends Control)

(define type)

(define (set-type [ty : LambdaType])
  (set! type ty)
  (compute-children))

(define (compute-children)
  (.set-text $Label (if (== type null) "" (.to-string type))))
