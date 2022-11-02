#lang gdlisp

(extends Spatial)

(define (export NodePath) root-path)

(define (get-root)
  (get-node root-path))
