#lang gdlisp

(extends Node)

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket racket/syntax syntax/parse))
  (define-syntax (define-type stx)
    (syntax-parse stx
      [(_ (code-name:id arg-name:id ...)
          #:name name:str
          {~optional {~and #:infix {~bind [infix #'true]}}}
          {~optional {~seq #:classes tc:expr ...}})
       (define arity (length (syntax->list #'(arg-name ...))))
       (define upcase-name (string-upcase (symbol->string (syntax-e #'code-name))))
       (define ctor-id (format-id stx "CTOR_~a" upcase-name))
       (quasisyntax/loc stx
         (begin
           (define #,ctor-id :=
             (TypeCtor.new name #,arity {~? infix false} {{~? {~@ {~@ tc true} ...}}}))
           #,(cond
               [(= 0 arity)
                (define mon-id (format-id stx "MON_~a" upcase-name))
                (define ty-id (format-id stx "TY_~a" upcase-name))
                (quasisyntax/loc stx
                  (begin
                    (define #,mon-id := (MonoCtor.new #,ctor-id []))
                    (define #,ty-id := (Type.new [] #,mon-id))))]
               [else
                (define mon-id (format-id stx "mono-~a" #'code-name))
                (quasisyntax/loc stx
                  (define (#,mon-id arg-name ...)
                    (MonoCtor.new #,ctor-id [arg-name ...])))])))])))

(define TC_ADD (TypeClass.new "Add"))
(define TC_SUB (TypeClass.new "Sub"))
(define TC_MUL (TypeClass.new "Mul"))
(define TC_VEC (TypeClass.new "Vec"))

;; Wrapper {
;;   value: T
;; }
(define-type (num) #:name "Num" #:classes TC_ADD TC_SUB TC_MUL)
(define-type (vec3) #:name "Vec3" #:classes TC_ADD TC_SUB TC_MUL TC_VEC)
(define-type (vec2) #:name "Vec2" #:classes TC_ADD TC_SUB TC_MUL TC_VEC)
(define-type (halfline) #;"T = {origin: Vec3; direction: Vec3}" #:name "Halfline")
(define-type (plane) #;"T = {normal: Vec3; distance: Num}" #:name "Plane")

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
  (compute-applications f-ty [x-ty]))

(define (compute-applications f-ty x-tys)
  (define tcx (.new TypingCtx))
  (define res-ty (.instantiate tcx f-ty))
  (for ([x-ty x-tys])
    (define next-res (MonoVar.new (.newtype tcx)))
    (.unify tcx null (Types.mono-fun (.instantiate tcx x-ty) next-res) res-ty)
    (set! res-ty next-res))
  (define res (.compute-substs tcx))
  (if (.-is-ok res)
      (.new Result true (.generalise tcx res-ty))
      res))
