#lang gdlisp

(class-name TypingCtx)

(define type-counter 0)
(define unifications [])
(define substitutions {})
(define constraints : Dictionary #;{int {TypeClass true}} {})

(define (newtype)
  (define t type-counter)
  (+set! type-counter 1)
  t)

(define (unify term [expected : Mono] [actual : Mono])
  (.append unifications [term expected actual]))

(define (apply-substs-0 term)
  (let loop ([t term])
    (cond
      [(and (is t MonoVar)
            (.has substitutions (.-no t)))
       (recur loop (.get substitutions (.-no t)))]
      [else t])))

(define (apply-substs term)
  (fset! term apply-substs-0)
  (cond
    [(is term MonoVar) term]
    [else
     (define new-tys [])
     (for ([ty (.-args term)])
       (.append new-tys (apply-substs ty)))
     (if (== new-tys (.-args term))
         term
         (.new MonoCtor (.-ctor term) new-tys))]))

(define (define-subst vr type)
  (fset! type apply-substs)
  (define free-vars {})
  (._collect-free-vars type free-vars)
  (cond
    [(.has free-vars vr) "cannot create infinite type"]
    [else
     (set! (ref substitutions vr) type)
     (when (.has constraints vr)
       (cond
         [(is type MonoVar)
          (define cnstrs (.get constraints type.no {}))
          (set! (ref constraints type.no) cnstrs)
          (.merge cnstrs (ref constraints vr))
          null]
         [else
          (define ctor type.ctor)
          (for ([tc (ref constraints vr)])
            (when (not (.has ctor.type-classes tc))
              (#%gdscript "return " (!expr (+ "type " (str type) " does not implement " (str tc))))))
          null]))]))

(define (run-unification expected actual)
  ;; returns null if ok, a string error message otherwise
  (define s0 (apply-substs-0 expected))
  (define s1 (apply-substs-0 actual))
  #;(print "unifying " s0 " and " s1)
  (cond
    [(== s0 s1) null]
    [(or (is s0 MonoVar)
         (is s1 MonoVar))
     (match (if (is s0 MonoVar)
                [s0 s1]
                [s1 s0])
       [[(var vr) (var ty)] (define-subst (.-no vr) ty)])]
    [else
     (assert (and (is s0 MonoCtor)
                  (is s1 MonoCtor)))
     (define c0 (.-ctor s0))
     (define a0 (.-args s0))
     (define c1 (.-ctor s1))
     (define a1 (.-args s1))
     (cond
       [(!= c0 c1)
        (+ "mismatched types: " (.to-string s0) " and " (.to-string s1))]
       [(!= (len a0) (len a1))
        "mismatched type constructor arities (if you are seeing this it's a bug!)"]
       [else
        (let loop ([i 0])
          (cond
            [(< i (len a0))
             (define res (run-unification (ref a0 i) (ref a1 i)))
             (if (== null res)
                 (recur loop (+ i 1))
                 res)]
            [else null]))])]))

(define (compute-substs) ;; Result<(), (term, msg)>
  (let loop ([i 0])
    (cond
      [(< i (len unifications))
       (define unif (ref unifications i))
       (define res (run-unification (ref unif 1) (ref unif 2)))
       (cond
         [(== null res)
          (recur loop (+ i 1))]
         [else
          (set! unifications (.slice unifications i (- (len unifications) 1)))
          (Result.new
           false
           [res (ref unif 0)])])]
      [else
       (set! unifications [])
       (Result.new true null)])))

(define (generalise [ty : Mono]) : Type
  (fset! ty apply-substs)
  (define fvs {})
  (._collect-free-vars ty fvs)
  (for ([fv fvs])
    (.merge (ref fvs fv) (.get constraints fv {})))
  (.new Type fvs ty))

(define (instantiate [ty : Type]) : Mono
  (for ([tv (.-type-vars ty)])
    (fset! type-counter max (+ tv 1)))

  (for ([tv (.-type-vars ty)])
    (define nt (newtype))
    (define cnstrs {})
    (set! (ref substitutions tv) (.new MonoVar nt))
    (set! (ref constraints nt) cnstrs)
    (.merge cnstrs (ref ty.type-vars tv)))

  (define substituted (apply-substs (.-mono ty)))
  (for ([tv (.-type-vars ty)])
    (.erase substitutions tv))
  substituted)
