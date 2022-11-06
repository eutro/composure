#lang gdlisp

(extends Node)
(require "../../macros.rkt")

(begin-escape
  (require (only-in racket/base begin-for-syntax define-syntax)
           (for-syntax racket racket/syntax syntax/parse gdlisp/utils))
  (define-syntax (define-type stx)
    (syntax-parse stx
      [(_ (code-name:id arg-name:id ...)
          #:name name:str
          {~optional {~and #:infix {~bind [infix #'true]}}}
          {~optional {~seq #:classes tc:expr ...}}
          #:fmt (v:id) fmt-body:expr ...)
       (define arity (length (syntax->list #'(arg-name ...))))
       (define code-name-str (mangle (symbol->string (syntax-e #'code-name))))
       (define upcase-name (string-upcase code-name-str))
       (define ctor-id (format-id stx "CTOR_~a" upcase-name))
       (define fmt-id (format-id stx "fmt_~a" code-name-str))
       (define fmt-str (symbol->string (syntax-e fmt-id)))
       (quasisyntax/loc stx
         (begin
           (define (#,fmt-id v) fmt-body ...)
           (define #,ctor-id :=
             (TypeCtor.new name
                           #,arity
                           {~? infix false}
                           {{~? {~@ {~@ tc true} ...}}}
                           (funcref self #,fmt-str)))
           (do-onready (set! (ref CTORS name) #,ctor-id))
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

(begin-ready!)

(define TC_ADD (TypeClass.new "Add"))
(define TC_SUB (TypeClass.new "Sub"))
(define TC_MUL (TypeClass.new "Mul"))
(define TC_DIV (TypeClass.new "Div"))
(define TC_VEC (TypeClass.new "Vec"))
(define (lookup-typeclass x)
  (match x
    ["Add" TC_ADD]
    ["Sub" TC_SUB]
    ["Mul" TC_MUL]
    ["Div" TC_DIV]
    ["Vec" TC_VEC]
    [_ (assert false "Failed Lookup") null]))

(define CTORS {})
(define (lookup-ctor x)
  (ref CTORS x))

;; Wrapper {
;;   value: T
;; }
(define-type (num) #:name "Num" #:classes TC_ADD TC_SUB TC_MUL TC_DIV #:fmt (v) (TextPreview.create (str v)))
(define-type (bool) #:name "Bool" #:classes #:fmt (v) (TextPreview.create (if v "⊤" "⊥")))
(define-type (vec3) #:name "Vec3" #:classes TC_ADD TC_SUB TC_MUL TC_DIV TC_VEC #:fmt (v) (VectorPreview.create v))
(define-type (vec2) #:name "Vec2" #:classes TC_ADD TC_SUB TC_MUL TC_DIV TC_VEC #:fmt (v) (VectorPreview.create v))
(define-type (ray) #;"T = {origin: Vec3; direction: Vec3}" #:name "Ray" #:fmt (v) (TextPreview.create "Ray"))
(define-type (plane) #:name "Plane" #:fmt (v) (TextPreview.create "Pln"))

;; Option { value: X (nullable) }
(define-type (maybe x) #:name "Maybe" #:fmt (v) (if (== null v) (TextPreview.create "Nil") (.create-preview v)))

;; Unit {}
(define-type (unit) #:name "Unit" #:fmt (_v) null)

;; NB: I've edited the game's version of JBMono to map these characters to their ligatured characters...

;; Fun<A, B> {
;;   apply(x: A): B
;; }
(define-type (fun arg ret) #:name "ƀ" #;-> #:infix #:fmt (_v) null)

;; Action<A, B> {
;;   type State: !Lambda
;;   start(): State
;;   step(x: A, s: State): B
;;   finish(s: State)
;; }
(define-type (action arg ret) #:name "Ƃ" #;~> #:infix #:fmt (_v) null)

;; Pair<A, B> {
;;   car: A
;;   cdr: B
;; }
(define-type (pair lhs rhs) #:name ":" #:infix #:fmt (v) (TextPreview.create "Pair"))

(define (mono-bin-fun arg1 arg2 ret)
  (mono-fun arg1 (mono-fun arg2 ret)))

(define (compute-application f-ty x-ty)
  (compute-applications f-ty [x-ty]))

(define (compute-applications f-ty x-tys)
  (define tcx (.new TypingCtx))
  (define res-ty (.instantiate tcx f-ty))
  (for ([x-ty x-tys])
    (define next-res (MonoVar.new (.newtype tcx)))
    (.unify tcx null (mono-fun (.instantiate tcx x-ty) next-res) res-ty)
    (set! res-ty next-res))
  (define res (.compute-substs tcx))
  (if (.-is-ok res)
      (.new Result true (.generalise tcx res-ty))
      res))

(finish-ready!)
