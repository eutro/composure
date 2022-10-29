#lang gdlisp

(extends Control)

(define term : LambdaValue)

(define (_ready)
  (.set-term (get-node "%Add") Values.VAL_ADD)
  (.set-term (get-node "%S") Values.VAL_S)
  (.set-term (get-node "%I") Values.VAL_I)
  (.set-term (get-node "%Compose") Values.VAL_COMPOSE))
