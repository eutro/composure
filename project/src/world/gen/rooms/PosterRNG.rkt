#lang gdlisp

(extends Node)

(define list (preload "PosterList.tres"))

(define used-0-pool)
(define used-1-pool)
(define unused-pool)

(define (_ready)
  (randomize)
  (define sprites (.duplicate list.sprites))
  (.shuffle sprites)
  (define cut-0 3)
  (define cut-1 7)
  (set! used-0-pool (.slice sprites 0 (- cut-0 1)))
  (set! used-1-pool (.slice sprites cut-0 (- cut-1 1)))
  (set! unused-pool (.slice sprites cut-1 (len sprites))))

(define (next)
  (randomize)
  (define i (% (randi) (len used-0-pool)))
  (define j (% (randi) (len used-1-pool)))
  (define k (% (randi) (len unused-pool)))
  (define ret (ref unused-pool k))
  (set! (ref unused-pool k) (ref used-1-pool j))
  (set! (ref used-1-pool j) (ref used-0-pool i))
  (set! (ref used-0-pool i) ret)
  ret)
