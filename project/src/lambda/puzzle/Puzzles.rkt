#lang gdlisp

(extends Node)

(require "../../macros.rkt"
         threading)

(define tracks (.-tracks (preload "AudioTracks.tres")))

(defrecord Puzzle
  ([description : String] ;; bbcode supported!
   [expected-type : Type]
   [check : FuncRef #;(-> LambdaValue Result)]
   [track : AudioStream])

  (define (instance-menu)
    (define menu (.instance (preload "PuzzleMenu.tscn")))
    (.load menu self)
    menu))

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket
                       racket/syntax
                       syntax/parse
                       gdlisp/utils))
  (define-syntax (define-puzzle stx)
    (syntax-parse stx
      [(_ name
          #:description desc:expr
          #:type type:expr
          #:track track:expr
          #:check (val:id) body:expr ...)
       (with-syntax* ([check-fn (format-id #'name "~a-check" #'name)]
                      [check-name (mangle (symbol->string (syntax-e #'check-fn)))])
         (syntax/loc stx
           (begin
             (define (check-fn val) body ...)
             (define name
               (Puzzle.new
                desc
                type
                (funcref self check-name)
                track)))))])))

(define-puzzle tutorial-puzzle
  #:description "What is the value of 2 + 2?"
  #:type Types.TY_NUM
  #:track tracks.persistence
  #:check (v) (Result.new (== v.value 4) "Incorrect answer"))

(define-puzzle sort-0
  #:description "What is the minimum of 2 and 3?"
  #:type Types.TY_NUM
  #:track tracks.persistence
  #:check (v) (Result.new (== v.value 2) "Incorrect answer"))

(define-puzzle sort-1
  #:description "Give me a function that returns whether its first argument is smaller than its second."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_BOOL))
  #:track tracks.persistence
  #:check (v)
  (Result.new
   (and (.-value (v.apply 0 10))
        (.-value (v.apply 0 1))
        (.-value (v.apply (randf) (+ (randf) 2)))
        (not (.-value (v.apply (randf) (+ (randf) -2))))
        (not (.-value (v.apply 0 0)))
        (not (.-value (v.apply 0.5 0.5)))
        (not (.-value (v.apply 0 -1)))
        (not (.-value (v.apply -1 -10))))
   "Incorrect answer"))

(define-puzzle sort-2
  #:description "Give me a function that returns the smaller of its two arguments."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:track tracks.persistence
  #:check (v)
  (Result.new
   (and (== 0 (.-value (v.apply 0 10)))
        (== 0 (.-value (v.apply 0 1)))
        (== 0 (.-value (v.apply 0 0)))
        (== 0.5 (.-value (v.apply 0.5 0.5)))
        (== -1 (.-value (v.apply 0 -1)))
        (== -10 (.-value (v.apply -1 -10))))
   "Incorrect answer"))

(define (eq-pair lhs rhs b)
  (and (== lhs b.car)
       (== rhs b.cdr)))

(define-puzzle sort-3
  #:description "Give me a function that returns its two arguments as a pair in ascending order"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM (Types.mono-pair Types.MON_NUM Types.MON_NUM)))
  #:track tracks.persistence
  #:check (v)
  (Result.new
   (and (eq-pair 0 10 (.-value (v.apply 0 10)))
        (eq-pair 0 1 (.-value (v.apply 0 1)))
        (eq-pair 0 0 (.-value (v.apply 0 0)))
        (eq-pair 0.5 0.5 (.-value (v.apply 0.5 0.5)))
        (eq-pair -1 0 (.-value (v.apply 0 -1)))
        (eq-pair -10 -1 (.-value (v.apply -1 -10))))
   "Incorrect answer"))
