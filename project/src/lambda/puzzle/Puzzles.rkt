#lang gdlisp

(extends Node)

(require "../../macros.rkt"
         threading)

(define tracks (.-tracks (preload "AudioTracks.tres")))

(defrecord Puzzle
  ([category : String]
   [description : String] ;; bbcode supported!
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
          #:category cat:expr #;nya
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
                cat
                desc
                type
                (funcref self check-name)
                track)))))])))

(define-puzzle impossible-puzzle
  #:category "Impossible"
  #:description "What is the last digit of π?"
  #:type Types.TY_NUM
  #:track tracks.red_tears
  #:check (_v) (Result.new false "Incorrect answer"))

(define-puzzle tutorial-puzzle
  #:category "Simple Arithmetic"
  #:description "What is the value of [code]2 + 2[/code]?"
  #:type Types.TY_NUM
  #:track tracks.first ;; so true!
  #:check (v)
  (Result.new
   (== v.value 4)
   (if (== v.value 5)
       "One think only please"
       "Incorrect answer")))

(define-puzzle sort-0
  #:category "Sorting"
  #:description "What is the minimum of 2 and 3?"
  #:type Types.TY_NUM
  #:track tracks.persistence
  #:check (v) (Result.new (== v.value 2) "Incorrect answer"))

(define (appv-int f xs)
  (for ([i (range (len xs))])
    (fset! (ref xs i) Values.wrap-num))
  (.apply-a f xs))

(define-puzzle sort-1
  #:category "Sorting"
  #:description "Give me a function that returns whether its first argument is [b]less[/b] than its second."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_BOOL))
  #:track tracks.persistence
  #:check (v)
  (define is-ok
    (and (.-value (appv-int v [0 10]))
         (.-value (appv-int v [0 1]))
         (.-value (appv-int v [(randf) (+ (randf) 2)]))
         (not (.-value (appv-int v [(randf) (+ (randf) -2)])))
         (not (.-value (appv-int v [0 0])))
         (not (.-value (appv-int v [0.5 0.5])))
         (not (.-value (appv-int v [0 -1])))
         (not (.-value (appv-int v [-1 -10])))))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [(and (.-value (appv-int v [2 0]))
           (.-value (appv-int v [3 0]))
           (not (.-value (appv-int v [-3 0]))))
      "[b]Less[/b] than, not greater"]
     [(and (.-value (appv-int v [0 1]))
           (.-value (appv-int v [0 0])))
      "[b]Strictly[/b] less than, if they are equal it must be false!"]
     [else "Incorrect answer"])))

(define-puzzle sort-2
  #:category "Sorting"
  #:description "Give me a function that returns the [b]lower[/b] of its two arguments."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:track tracks.persistence
  #:check (v)
  (define is-ok
    (and (== 0 (.-value (appv-int v [0 10])))
         (== 0 (.-value (appv-int v [0 1])))
         (== 0 (.-value (appv-int v [0 0])))
         (== 0.5 (.-value (appv-int v [0.5 0.5])))
         (== -1 (.-value (appv-int v [0 -1])))
         (== -10 (.-value (appv-int v [-1 -10])))))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [(and (== 10 (.-value (appv-int v [0 10])))
           (== 10 (.-value (appv-int v [10 0]))))
      "[b]Lower[/b] of its two arguments, not greater"]
     [else "Incorrect answer"])))

(define (eq-pair lhs rhs b)
  (and (== lhs b.car)
       (== rhs b.cdr)))

(define-puzzle sort-3
  #:category "Sorting"
  #:description "Give me a function that returns its two arguments as a pair in ascending order"
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM (Types.mono-pair Types.MON_NUM Types.MON_NUM)))
  #:track tracks.persistence
  #:check (v)
  (Result.new
   (and (eq-pair 0 10 (.-value (appv-int v [0 10])))
        (eq-pair 0 1 (.-value (appv-int v [0 1])))
        (eq-pair 0 0 (.-value (appv-int v [0 0])))
        (eq-pair 0.5 0.5 (.-value (appv-int v [0.5 0.5])))
        (eq-pair -1 0 (.-value (appv-int v [0 -1])))
        (eq-pair -10 -1 (.-value (appv-int v [-1 -10]))))
   "Incorrect answer"))

(define (init-progress)
  {
   "Arithmetic" 0
   "Sorting" 0
   "Impossible" 0
   })

(define (inc-progress cat)
  (+set! (ref Game.puzzle-progress cat) 1))

(define (next-puzzle-for-category cat)
  (define progress (ref Game.puzzle-progress cat))
  (match cat
    ["Arithmetic" tutorial-puzzle]
    ["Sorting" (match progress [0 sort-0] [1 sort-1] [2 sort-2] [_ sort-3])]
    [_ impossible-puzzle]))
