#lang gdlisp

(extends Node)

(define keys : KeyBinder (.new KeyBinder))
(define world null)
(define ui null)

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket syntax/parse))
  (define-syntax (repeat stx)
    (syntax-parse stx
      [(_ n:integer expr)
       (with-syntax ([(repetitions ...)
                      (for/list ([i (in-range (syntax-e #'n))])
                        #'expr)])
         (syntax/loc stx [repetitions ...]))])))

(define user-defs (repeat 100 ["" null]))

(define puzzle-progress (Puzzles.init-progress))

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)

(define music (.new AudioStreamPlayer))
(define queued-track null)

(define (_on_Music_finished)
  (when (and (is-instance-valid ui) (!= null queued-track))
    (.set-stream music queued-track)
    (.play music)))

(define (fade-out-music)
  (set! queued-track null)
  (define tween (.create-tween (get-tree)))
  (define amplify-effect (AudioServer.get-bus-effect 0 0))
  (.tween-property tween amplify-effect "volume_db" -80.0 5)
  (yield tween "finished")
  (.stop music)
  (.set amplify-effect "volume_db" 0))

(define (queue-track stream)
  (when (not (.is-connected music "finished" self "_on_Music_finished"))
    (.connect music "finished" self "_on_Music_finished"))
  (set! queued-track stream)
  (when (not (.is-playing music))
    (.set-stream music queued-track)
    (.play music))
  null)

(define (_ready)
  (add-child music)
  (set! music.bus "Music")
  (randomize))
