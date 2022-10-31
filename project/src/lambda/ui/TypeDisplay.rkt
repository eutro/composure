#lang gdlisp

(extends Control)

(define type)

(define (set-type [ty : Type])
  (set! type ty)
  (compute-children))

(define (compute-children)
  (.set-text $Label (if (== type null) "" (.to-string type))))
