#lang gdlisp

(extends Node)

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket racket/syntax syntax/parse))
  (define-syntax (define-type stx)
    (syntax-parse stx
      [(_ (code-name:id arg-name:id ...)
          #:name name:str
          {~optional {~and #:infix {~bind [infix #'true]}}})
       (define arity (length (syntax->list #'(arg-name ...))))
       (define upcase-name (string-upcase (symbol->string (syntax-e #'code-name))))
       (define ctor-id (format-id stx "CTOR_~a" upcase-name))
       (quasisyntax/loc stx
         (begin
           (define #,ctor-id := (TypeCtor.new name #,arity {~? infix false}))
           #,(cond
               [(= 0 arity)
                (define mon-id (format-id stx "MON_~a" upcase-name))
                (define ty-id (format-id stx "TY_~a" upcase-name))
                (quasisyntax/loc stx
                  (begin
                    (define #,mon-id := (LambdaMonoCtor.new #,ctor-id []))
                    (define #,ty-id := (LambdaType.new [] #,mon-id))))]
               [else
                (define mon-id (format-id stx "mono-~a" #'code-name))
                (quasisyntax/loc stx
                  (define (#,mon-id arg-name ...)
                    (LambdaMonoCtor.new #,ctor-id [arg-name ...])))])))])))

;; Wrapper {
;;   value: T
;; }
(define-type (num) #:name "Num")
(define-type (vec3) #:name "Vec3")
(define-type (vec2) #:name "Vec2")
(define-type (halfline) #;"T = {origin: Vec3; direction: Vec3}" #:name "Halfline"
  )

;; Unit {}
(define-type (unit) #:name "Unit")

;; Fun<A, B> {
;;   apply(x: A): B
;; }
(define-type (fun arg ret) #:name "->" #:infix)

;; Action<A, B> {
;;   type State: !Lambda
;;   start(): State
;;   step(x: A, s: State): B
;;   finish(s: State)
;; }
(define-type (action arg ret) #:name "~>" #:infix)

;; Pair<A, B> {
;;   car: A
;;   cdr: B
;; }
(define-type (pair lhs rhs) #:name ":" #:infix)

(define (mono-bin-fun arg1 arg2 ret)
  (mono-fun arg1 (mono-fun arg2 ret)))

(define (compute-application f-ty x-ty)
  (define tcx (.new TypingCtx))
  (define fun-ty (.instantiate tcx f-ty))
  (define arg-ty (.instantiate tcx x-ty))
  (define ret-ty (.new LambdaMonoVar (.newtype tcx)))
  (.unify tcx null (Types.mono-fun arg-ty ret-ty) fun-ty)
  (define res (.compute-substs tcx))
  (if (.-is-ok res)
      (.new Result true (.generalise tcx ret-ty))
      res))
