#lang gdlisp

(class-name LambdaMonoCtor)
(extends LambdaMono)

(define ctor : TypeCtor)
(define args : Array #;[LambdaMono] [])

(define (_init ct ag)
  (set! ctor ct)
  (set! args ag))

(define (is-ctor)
  true)

(define (_collect-free-vars vars)
  ;; we could cache these but we can also not
  (for ([arg args])
    (._collect-free-vars arg vars)))

(define static (to-str-with-brackets t)
  (define p
    (and (.is-ctor t)
         ((len t.args) . > . 0)))
  (define s "")
  (when p (+set! s "("))
  (+set! s (.to-string t))
  (when p (+set! s ")"))
  s)

(define (_to-string)
  (define s "")
  (cond
    [(.-infix ctor)
     (let loop ([t self])
       (cond
         [(and (.is-ctor t) (.-infix t.ctor))
          (+set! s (to-str-with-brackets (ref t.args 0)))
          (+set! s " ")
          (+set! s (.-name t.ctor))
          (+set! s " ")
          (recur loop (ref t.args 1))]
         [else (+ s (to-str-with-brackets t))]))]
    [else
     (+set! s (.-name ctor))
     (for ([arg args])
       (+set! s " ")
       (+set! s (.to-string arg)))
     s]))
