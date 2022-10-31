#lang gdlisp

(class-name MonoVar)
(extends Mono)

(define no : int)

(define (_init i)
  (set! no i))

(define (_collect-free-vars vars)
  (set! (ref vars no) true))

(define const ABC_COUNT 26)

(define static (char-for n)
  (char (+ (% n ABC_COUNT) (ord "a"))))

(define static (name-for no)
  (cond
    [(no . < . ABC_COUNT) (char-for no)]
    [else
     (define strr "")
     (let loop ([n no])
       (+set! strr (char-for n))
       (when (>= n ABC_COUNT)
         (recur loop (- (/ n ABC_COUNT) 1))))
     ;; no string reverse, godot?
     (define strrev "")
     (for ([c (range (len strr))])
       (+set! strrev (ref strr (- 0 c 1))))
     strrev]))

(define (_to-string) (name-for no))
