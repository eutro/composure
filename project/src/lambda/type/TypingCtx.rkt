#lang gdlisp

(class-name TypingCtx)

(define var-types {})
(define type-counter 0)
(define unifications [])
(define substitutions {})

(define (newtype)
  (define t type-counter)
  (+set! type-counter 1)
  t)

(define (lookup-var [vr : String])
  (cond
    [(in vr var-types)
     (ref var-types vr)]
    [else
     (define t (newtype))
     (set! (ref var-types vr) t)
     t]))

(define (unify term [expected : LambdaMono] [actual : LambdaMono])
  (.append unifications [term expected actual]))

(define (apply-substs-0 term)
  (let loop ([t term])
    (cond
      [(and (is t LambdaMonoVar)
            (.has substitutions (.-no t)))
       (recur loop (.get substitutions (.-no t)))]
      [else t])))

(define (apply-substs term)
  (fset! term apply-substs-0)
  (cond
    [(is term LambdaMonoVar) term]
    [else
     (define new-tys [])
     (for ([ty (.-args term)])
       (.append new-tys (apply-substs ty)))
     (if (== new-tys (.-args term))
         term
         (.new LambdaMonoCtor (.-ctor term) new-tys))]))

(define (define-subst vr type)
  (define free-vars {})
  (._collect-free-vars type free-vars)
  (cond
    [(.has free-vars vr) "cannot create infinite type"]
    [else (set! (ref substitutions vr) type)]))

(define (run-unification expected actual)
  ;; returns null if ok, a string error message otherwise
  (define s0 (apply-substs-0 expected))
  (define s1 (apply-substs-0 actual))
  (print "unifying " s0 " and " s1)
  (cond
    [(== s0 s1) null]
    [(or (is s0 LambdaMonoVar)
         (is s1 LambdaMonoVar))
     (match (if (is s0 LambdaMonoVar)
                [s0 s1]
                [s1 s0])
       [[(var vr) (var ty)] (define-subst (.-no vr) ty)])]
    [else
     (assert (and (is s0 LambdaMonoCtor)
                  (is s1 LambdaMonoCtor)))
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

(define (generalise [ty : LambdaMono]) : LambdaType
  (fset! ty apply-substs)
  (define fvs {})
  (._collect-free-vars ty fvs)
  (define params (.keys fvs))
  (.sort params)
  (.new LambdaType params ty))

(define (instantiate [ty : LambdaType]) : LambdaMono
  (for ([tv (.-type-vars ty)])
    (fset! type-counter max (+ tv 1)))
  (for ([tv (.-type-vars ty)])
    (set! (ref substitutions tv) (.new LambdaMonoVar (newtype))))
  (apply-substs (.-mono ty)))
