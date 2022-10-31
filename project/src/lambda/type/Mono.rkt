#lang gdlisp

;; Represents a monotype in the Hindley-Milner type system

(class-name Mono)

(define (_collect-free-vars _vars)
  null)

(define (is-ctor)
  false)
