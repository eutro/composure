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
  (require (only-in racket/base begin-for-syntax define-syntax)
           (for-syntax racket
                       racket/syntax
                       syntax/parse
                       gdlisp/utils))
  (begin-for-syntax
    (define puzzles (make-hash)))
  (define-syntax (reset-puzzles! stx)
    (hash-clear! puzzles)
    #'(begin))

  (define-syntax (emit-puzzles! stx)
    (syntax-parse stx
      [(_ names:str ...)
       (when (not (= (hash-count puzzles)
                     (add1 (length (syntax->list #'(names ...))))))
         (raise-syntax-error
          'emit-puzzles!
          (format "mismatched keys;\n  expected: ~s \\ (\"Impossible\")\n  got: ~s"
                  (hash-keys puzzles)
                  (syntax->datum #'(names ...)))
          stx))
       (syntax-parse
           (for/list ([(cat c-pzzls) (in-hash puzzles)])
             #`(#,cat #,(reverse c-pzzls)))
         [((cat:str [pzl ...]) ...)
          #'(begin
              (define PUZZLE_TYPES [names ...])
              (define PUZZLES
                {{~@ cat [pzl ...]} ...}))])]))

  (define-syntax (define-puzzle stx)
    (syntax-parse stx
      [(_ name:id
          #:category cat:str #;nya
          #:description desc:expr
          #:type type:expr
          #:track track:expr
          #:check (val:id) body:expr ...)
       (define cat-str (syntax-e #'cat))
       (hash-set! puzzles cat-str
                  (cons #'name (hash-ref puzzles cat-str null)))
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

(define (appv-int f xs)
  (for ([i (range (len xs))])
    (fset! (ref xs i) Values.wrap-num))
  (.apply-a f xs))

(define TV_A (MonoVar.new 0))
(define TV_B (MonoVar.new 1))
(define TV_C (MonoVar.new 2))
(define TV_D (MonoVar.new 3))

(reset-puzzles!)

(define-puzzle impossible-puzzle
  #:category "Impossible"
  #:description "What is the last digit of π?"
  #:type Types.TY_NUM
  #:track tracks.red_tears
  #:check (_v) (Result.new false "Incorrect answer"))

(define-puzzle tutorial-puzzle
  #:category "Arithmetic"
  #:description "What is the value of [code]2 + 2[/code]?"
  #:type Types.TY_NUM
  #:track tracks.first ;; so true!
  #:check (v)
  (Result.new
   (== v.value 4)
   (if (== v.value 5)
       "One think only please"
       "Incorrect answer")))

(define-puzzle arith-1
  #:category "Arithmetic"
  #:description "What is the value of [code]3 * 4[/code]?"
  #:type Types.TY_NUM
  #:track tracks.first
  #:check (v)
  (Result.new
   (== v.value 12)
   "Incorrect answer"))

(define-puzzle arith-2
  #:category "Arithmetic"
  #:description "Give me a function that adds two numbers together."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:track tracks.first
  #:check (v)
  (Result.new
   (and (== 7 (.-value (appv-int v [2 5])))
        (== 8 (.-value (appv-int v [3 5])))
        (== 15 (.-value (appv-int v [5 10])))
        (== 61 (.-value (appv-int v [60 1])))
        (== 10 (.-value (appv-int v [-20 30]))))
   "Incorrect answer"))

(define-puzzle arith-3
  #:category "Arithmetic"
  #:description "Give me a function that adds 10 to a number."
  #:type (Type.new [] (Types.mono-fun Types.MON_NUM Types.MON_NUM))
  #:track tracks.transience
  #:check (v)
  (Result.new
   (and (== 17 (.-value (appv-int v [7])))
        (== 18 (.-value (appv-int v [8])))
        (== 30 (.-value (appv-int v [20])))
        (== 0 (.-value (appv-int v [-10])))
        (== 5 (.-value (appv-int v [-5]))))
   "Incorrect answer"))

(define-puzzle arith-4
  #:category "Arithmetic"
  #:description "Give me a function that doubles a number."
  #:type (Type.new [] (Types.mono-fun Types.MON_NUM Types.MON_NUM))
  #:track tracks.transience
  #:check (v)
  (Result.new
   (and (== 14 (.-value (appv-int v [7])))
        (== 16 (.-value (appv-int v [8])))
        (== 40 (.-value (appv-int v [20])))
        (== -20 (.-value (appv-int v [-10])))
        (== -10 (.-value (appv-int v [-5]))))
   "Incorrect answer"))

(define-puzzle arith-5
  #:category "Arithmetic"
  #:description "Give me a function that squares a number."
  #:type (Type.new [] (Types.mono-fun Types.MON_NUM Types.MON_NUM))
  #:track tracks.transience
  #:check (v)
  (Result.new
   (and (== 49 (.-value (appv-int v [7])))
        (== 64 (.-value (appv-int v [8])))
        (== 400 (.-value (appv-int v [20])))
        (== 100 (.-value (appv-int v [-10])))
        (== 25 (.-value (appv-int v [-5]))))
   "Incorrect answer"))

(define-puzzle arith-6
  #:category "Arithmetic"
  #:description "Give me a function that returns both square roots of a positive number, in any order."
  #:type (Type.new [] (Types.mono-fun Types.MON_NUM (Types.mono-pair Types.MON_NUM Types.MON_NUM)))
  #:track tracks.transience
  #:check (v)
  (Result.new
   (and (eq-pair-either 7 -7 (.-value (appv-int v [49])))
        (eq-pair-either 8 -8 (.-value (appv-int v [64])))
        (eq-pair-either 20 -20 (.-value (appv-int v [400])))
        (eq-pair-either 10 -10 (.-value (appv-int v [100])))
        (eq-pair-either 5 -5 (.-value (appv-int v [25]))))
   "Incorrect answer"))

(define-puzzle arith-7
  #:category "Arithmetic"
  #:description "Give me a function that, given arguments [code]a[/code], [code]b[/code] and [code]c[/code], \
returns any solution to [code]ax² + bx + c = 0[/code]."
  #:type
  (Type.new
   []
   (Types.mono-fun
    Types.MON_NUM
    (Types.mono-bin-fun
     Types.MON_NUM Types.MON_NUM
     Types.MON_NUM)))
  #:track tracks.transience
  #:check (v)
  (Result.new
   (and (eq-either 7 -7 (.-value (appv-int v [1 0 -49])))
        (eq-either 8 -8 (.-value (appv-int v [1 0 -64])))
        (eq-either 20 -20 (.-value (appv-int v [1 0 -400])))
        (eq-either 10 -10 (.-value (appv-int v [1 0 -100])))
        (eq-either 5 -5 (.-value (appv-int v [1 0 -25])))

        (eq-either 3 7 (.-value (appv-int v [1 -10 21])))
        (eq-either 3 7 (.-value (appv-int v [2 -20 42]))))
   "Incorrect answer"))

(define-puzzle comb-0
  #:category "Combinator"
  #:description "Give me a function that composes any two functions, from [b]left[/b] to [b]right[/b].

i.e. X f g -> λx.g(f x)"
  #:type (Type.new
          [0 1 2]
          (Types.mono-bin-fun
           (Types.mono-fun TV_A TV_B)
           (Types.mono-fun TV_B TV_C)
           (Types.mono-fun TV_A TV_C)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 3)
   "Incorrect answer"))

(define-puzzle comb-1
  #:category "Combinator"
  #:description "Give me a function that composes any two functions, from [b]right[/b] to [b]left[/b].

i.e. [code]X f g[/code] becomes [code]λx.f(g x)[/code]"
  #:type (Type.new
          [0 1 2]
          (Types.mono-bin-fun
           (Types.mono-fun TV_B TV_C)
           (Types.mono-fun TV_A TV_B)
           (Types.mono-fun TV_A TV_C)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 3)
   "Incorrect answer"))

(define-puzzle comb-2
  #:category "Combinator"
  #:description "Give me a function that returns [code]1[/code] if its argument is true, or [code]0[/code] if it is false."
  #:type (Type.new [] (Types.mono-fun Types.MON_BOOL Types.MON_NUM))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (and (== 1 (.-value (.apply v (LambdaWrapper.new true Types.TY_BOOL))))
        (== 0 (.-value (.apply v (LambdaWrapper.new false Types.TY_BOOL)))))
   "Incorrect answer"))

(define-puzzle comb-3
  #:category "Combinator"
  #:description "Give me a function that, given a value [code]x[/code], returns a pair of [code]x[/code] and [code]x[/code]."
  #:type (Type.new
          [0]
          (Types.mono-fun
           TV_A
           (Types.mono-pair TV_A TV_A)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 1)
   "Incorrect answer"))

(define-puzzle comb-4
  #:category "Combinator"
  #:description "Give me a function that, given a binary function [code]f[/code], returns a function that \
applies its argument to [code]f[/code] twice.

i.e. [code]X f[/code] becomes [code]λx.f x x[/code]"
  #:type (Type.new
          [0 1]
          (Types.mono-fun
           (Types.mono-bin-fun TV_A TV_A TV_B)
           (Types.mono-fun TV_A TV_B)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 2)
   "Incorrect answer"))

(define-puzzle comb-5
  #:category "Combinator"
  #:description "Give me a function that, given a pair, returns its left value."
  #:type (Type.new
          [0 1]
          (Types.mono-fun
           (Types.mono-pair TV_A TV_B)
           TV_A))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 2)
   "Incorrect answer"))

(define-puzzle comb-6
  #:category "Combinator"
  #:description "Give me a function that swaps the values in a pair."
  #:type (Type.new
          [0 1]
          (Types.mono-fun
           (Types.mono-pair TV_A TV_B)
           (Types.mono-pair TV_B TV_A)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 2)
   "Incorrect answer"))

(define-puzzle comb-7
  #:category "Combinator"
  #:description "Give me a function that, given a binary function and a unary function, \
returns a function that applies the unary function to the result of the binary function."
  #:type (Type.new
          [0 1 2 3]
          (Types.mono-bin-fun
           (Types.mono-bin-fun TV_A TV_B TV_C)
           (Types.mono-fun TV_C TV_D)
           (Types.mono-bin-fun TV_A TV_B TV_D)))
  #:track tracks.red-tears
  #:check (v)
  (Result.new
   (== (~> v .get-type .-type-vars len) 4)
   "Incorrect answer"))

(define-puzzle sort-0
  #:category "Sorting"
  #:description "What is the minimum of 2 and 3?"
  #:type Types.TY_NUM
  #:track tracks.persistence
  #:check (v) (Result.new (== v.value 2) "Incorrect answer"))

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
  #:description "Give me a function that returns -1 if its first argument is [b]less[/b] than its second, and 1 otherwise.

You may wish to solve the [b]Combinator[/b] puzzles first."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:track tracks.persistence
  #:check (v)
  (define is-ok
    (and (== -1 (.-value (appv-int v [0 10])))
         (== -1 (.-value (appv-int v [0 1])))
         (== -1 (.-value (appv-int v [(randf) (+ (randf) 2)])))
         (== 1 (.-value (appv-int v [(randf) (+ (randf) -2)])))
         (== 1 (.-value (appv-int v [0 0])))
         (== 1 (.-value (appv-int v [0.5 0.5])))
         (== 1 (.-value (appv-int v [0 -1])))
         (== 1 (.-value (appv-int v [-1 -10])))))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [(and (== -1 (.-value (appv-int v [2 0])))
           (== -1 (.-value (appv-int v [3 0])))
           (== 1 (.-value (appv-int v [-3 0]))))
      "[b]Less[/b] than, not greater"]
     [else "Incorrect answer"])))

(define-puzzle sort-3
  #:category "Sorting"
  #:description "Give me a function that returns the [b]lower[/b] of its two arguments.

Once again, you may wish to solve the [b]Combinator[/b] puzzles first."
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
  (and (== lhs b.car.value)
       (== rhs b.cdr.value)))

(define (eq-val-pair lhs r-lhs r-rhs b)
  (and (== lhs b.car.value)
       (eq-pair r-lhs r-rhs b.cdr.value)))

(define (eq-pair-pair l-lhs l-rhs r-lhs r-rhs b)
  (and (eq-pair l-lhs l-rhs b.car.value)
       (eq-pair r-lhs r-rhs b.cdr.value)))

(define (eq-pair-either lhs rhs b)
  (or (eq-pair lhs rhs b)
      (eq-pair rhs lhs b)))

(define (eq-either x y b)
  (or (== x b)
      (== y b)))

(define-puzzle sort-4
  #:category "Sorting"
  #:description "Give me a function that returns its two arguments as a pair in ascending order."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM (Types.mono-pair Types.MON_NUM Types.MON_NUM)))
  #:track tracks.persistence
  #:check (v)
  (Result.new
   (and (eq-pair 0 10 (.-value (appv-int v [0 10])))
        (eq-pair 0 1 (.-value (appv-int v [1 0])))
        (eq-pair 0 0 (.-value (appv-int v [0 0])))
        (eq-pair 0.5 0.5 (.-value (appv-int v [0.5 0.5])))
        (eq-pair -1 0 (.-value (appv-int v [0 -1])))
        (eq-pair -10 -1 (.-value (appv-int v [-1 -10]))))
   "Incorrect answer"))

(define-puzzle sort-5
  #:category "Sorting"
  #:description "Give me a function that compares its first argument to its second, returns -1 if it is less, \
1 if it is more, and 0 if they are equal."
  #:type (Type.new [] (Types.mono-bin-fun Types.MON_NUM Types.MON_NUM Types.MON_NUM))
  #:track tracks.persistence
  #:check (v)
  (define is-ok
    (and (== -1 (.-value (appv-int v [0 10])))
         (== -1 (.-value (appv-int v [0 1])))
         (== -1 (.-value (appv-int v [(randf) (+ (randf) 2)])))
         (== 1 (.-value (appv-int v [(randf) (+ (randf) -2)])))
         (== 0 (.-value (appv-int v [0 0])))
         (== 0 (.-value (appv-int v [0.5 0.5])))
         (== 1 (.-value (appv-int v [0 -1])))
         (== 1 (.-value (appv-int v [-1 -10])))))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [(and (== 1 (.-value (appv-int v [0 10])))
           (== 1 (.-value (appv-int v [0 1])))
           (== 1 (.-value (appv-int v [(randf) (+ (randf) 2)])))
           (== -1 (.-value (appv-int v [(randf) (+ (randf) -2)])))
           (== 0 (.-value (appv-int v [0 0])))
           (== 0 (.-value (appv-int v [0.5 0.5])))
           (== -1 (.-value (appv-int v [0 -1])))
           (== -1 (.-value (appv-int v [-1 -10]))))
      "Other way around!"]
     [else "Incorrect answer"])))

(define-puzzle sort-6
  #:category "Sorting"
  #:description "Give me a function that returns its three arguments as a pair of a number and a pair, with values in ascending order."
  #:type
  (Type.new
   []
   (Types.mono-bin-fun
    Types.MON_NUM Types.MON_NUM
    (Types.mono-fun
     Types.MON_NUM
     (Types.mono-pair Types.MON_NUM (Types.mono-pair Types.MON_NUM Types.MON_NUM)))))
  #:track tracks.persistence
  #:check (v)
  (define is-ok true)
  (define failed-on null)
  (for ([i (range 100)])
    (define xs [(% (randi) 5) (+ (% (randi) 5) 10) (+ (% (randi) 5) 20)])
    (define sh-xs (.duplicate xs))
    (.shuffle sh-xs)
    (cond
      [(not (eq-val-pair (ref xs 0) (ref xs 1) (ref xs 2)
                         (.-value (appv-int v sh-xs))))
       (set! is-ok false)
       (set! failed-on sh-xs)
       (#%gdscript "break")]))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [else (+ "Incorrect answer, failed on: " (str failed-on))])))

#;
(define-puzzle sort-7
  #:category "Sorting"
  #:description "Give me a function that returns its four arguments as a pair of pairs with values in ascending order."
  #:type
  (Type.new
   []
   (Types.mono-bin-fun
    Types.MON_NUM Types.MON_NUM
    (Types.mono-bin-fun
     Types.MON_NUM Types.MON_NUM
     (Types.mono-pair
      (Types.mono-pair Types.MON_NUM Types.MON_NUM)
      (Types.mono-pair Types.MON_NUM Types.MON_NUM)))))
  #:track tracks.persistence
  #:check (v)
  (define is-ok true)
  (define failed-on null)
  (for ([i (range 100)])
    (define xs [(% (randi) 5) (+ (% (randi) 5) 10) (+ (% (randi) 5) 20) (+ (% (randi) 5) 30)])
    (define sh-xs (.duplicate xs))
    (.shuffle sh-xs)
    (cond
      [(not (eq-pair-pair (ref xs 0) (ref xs 1) (ref xs 2) (ref xs 3)
                          (.-value (appv-int v sh-xs))))
       (set! is-ok false)
       (set! failed-on sh-xs)
       (#%gdscript "break")]))
  (Result.new
   is-ok
   (cond
     [is-ok ""]
     [else (+ "Incorrect answer, failed on: " (str failed-on))])))

(define-puzzle vec-0
  #:category "Vector"
  #:description "What is the length of the vector [code](0, 1, 0)[/code]?"
  #:type Types.TY_NUM
  #:track tracks.rest
  #:check (v) (Result.new (== 1 v.value) "Incorrect answer"))

(define-puzzle vec-1
  #:category "Vector"
  #:description "Give me a 3D vector that points [b]up[/b]."
  #:type Types.TY_VEC3
  #:track tracks.rest
  #:check (v)
  (define is-ok (.is-equal-approx Vector3.UP (.normalized v.value)))
  (Result.new is-ok "Incorrect answer"))

(define-puzzle vec-2
  #:category "Vector"
  #:description "Give me a function that returns the [code]y[/code] component of a 3D vector."
  #:type (Type.new [] (Types.mono-fun Types.MON_VEC3 Types.MON_NUM))
  #:track tracks.rest
  #:check (v)
  (Result.new
   (and (is-equal-approx 30 (.-value (v.apply (Values.wrap-vec3 (Vector3 40 30 20)))))
        (is-equal-approx 20 (.-value (v.apply (Values.wrap-vec3 (Vector3 40 20 30)))))
        (is-equal-approx 40 (.-value (v.apply (Values.wrap-vec3 (Vector3 20 40 -10))))))
   "Incorrect answer"))

(define (is-perp? u v)
  (and (not (.is-equal-approx v (Vector2 0 0)))
       (is-zero-approx (.dot u v))))

(define-puzzle vec-3
  #:category "Vector"
  #:description "Give me a function that returns a (nontrivial) perpendicular to a 2D vector."
  #:type (Type.new [] (Types.mono-fun Types.MON_VEC2 Types.MON_VEC2))
  #:track tracks.rest
  #:check (v)
  (Result.new
   (and (is-perp? (Vector2 30 20) (.-value (v.apply (Values.wrap-vec2 (Vector2 30 20)))))
        (is-perp? (Vector2 20 30) (.-value (v.apply (Values.wrap-vec2 (Vector2 20 30)))))
        (is-perp? (Vector2 40 -10) (.-value (v.apply (Values.wrap-vec2 (Vector2 40 -10))))))
   "Incorrect answer"))

(define (init-progress)
  (define m {})
  (for ([k PUZZLES])
    (set! (ref m k) 0))
  m)

(define (compute-unlocked ucat)
  (define l [])
  (for ([cat Values.CATEGORIES])
    (for ([entry (ref Values.CATEGORIES cat)])
      (define unlocked-before (DefinitionList.is-entry-unlocked entry))
      (when (not unlocked-before)
        (+set! (ref Game.puzzle-progress ucat) 1)
        (define unlocked-after (DefinitionList.is-entry-unlocked entry))
        (when unlocked-after
          (.append l [cat entry]))
        (-set! (ref Game.puzzle-progress ucat) 1))))
  l)

(define (inc-progress cat)
  (define unlocked (compute-unlocked cat))
  (fset! (ref Game.puzzle-progress cat) ~> (+ 1) (min (len (ref PUZZLES cat))))
  (~> Game.ui .mount-gui .get-definitions .recompute-children)
  (Game.dirty)
  unlocked)

(define (ref-wrap v i)
  (ref v (min i (- (len v) 1))))

(define (next-puzzle-for-category cat)
  (define progress (ref Game.puzzle-progress cat))
  (ref-wrap (.get PUZZLES cat "Impossible") progress))

(emit-puzzles! "Arithmetic" "Vector" "Sorting" "Combinator") ;; hide "Impossible"
