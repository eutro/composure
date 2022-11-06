#lang gdlisp

(extends Node)
(require "../macros.rkt"
         threading)

(define keys : KeyBinder (.new KeyBinder))
(define world null)
(define ui null)

(begin-escape
  (require (only-in racket/base define-syntax)
           (for-syntax racket syntax/parse))
  (define-syntax (repeat stx)
    (syntax-parse stx
      [(_ n:integer expr)
       (with-syntax ([(repetitions ...)
                      (for/list ([i (in-range (syntax-e #'n))])
                        #'expr)])
         (syntax/loc stx [repetitions ...]))])))

(define user-defs (repeat 100 ["" null]))

(define puzzle-progress (Puzzles.init-progress))

(define const FILE_PATH "user://game.json")

(define (dirty)
  (define js (save-json))
  (when js
    (doto (File.new)
     (.open FILE_PATH File.WRITE)
     (.store-string (JSON.print js))
     (.close))))

(define (load-data)
  (define file (File.new))
  (when (file.file-exists FILE_PATH)
    (file.open FILE_PATH File.READ)
    (define parse-res (~> file .get-as-text JSON.parse))
    (when (== OK parse-res.error)
      (load-json parse-res.result))
    (file.close)))

(define (puzzle-completed? cat i)
  ((ref puzzle-progress cat) . > . i))

(define tutorial-done false)
(define (maybe-start-tutorial)
  (cond
    [(not tutorial-done) (ui.start-tutorial true)]
    [else (ui.start-reloaded)]))

(define (save-defs)
  (define ls [])
  (for ([def user-defs])
    (when (!= null (ref def 1))
      (.append ls [(ref def 0) (.to-json (ref def 1))])))
  ls)

(define (load-defs defs)
  (for ([i (range (len user-defs))])
    (cond
      [(< i (len defs))
       (define e (ref defs i))
       (set! (ref user-defs i)
             [(str (ref e 0))
              (Values.from-json (ref e 1))])]
      [else
       (set! (ref user-defs i)
             ["" null])])))

(define (save-json)
  {
   "tutorial_done" tutorial-done
   "progress" puzzle-progress
   "defs" (save-defs)
   "keys" (keys.save-json)
   })

(define (load-json json)
  (when (is json Dictionary)
    (set! tutorial-done (.get json "tutorial_done" false))
    (.merge puzzle-progress (.get json "progress" {}) true)
    (when ("keys" . in . json)
      (.load-json keys json.keys))
    (load-defs (.get json "defs" [])))
  null)

(define (reset)
  (set! tutorial-done false)
  (set! puzzle-progress (Puzzles.init-progress))
  (set! keys (.new KeyBinder))
  (for ([i (range (len user-defs))])
    (set! (ref user-defs i) ["" null]))
  (dirty))

(signal copy-source-changed)
(define copy-src)
(define (set-copy-source src)
  (set! copy-src src)
  (emit-signal "copy_source_changed")
  null)

(define music (.new AudioStreamPlayer))
(define queued-track null)

(define (_on_Music_finished)
  (when (and (is-instance-valid ui) (!= null queued-track))
    (.set-stream music queued-track)
    (.play music)))

(define (fade-out-music)
  (when (.is-playing music)
    (set! queued-track null)
    (define tween (.create-tween (get-tree)))
    (define amplify-effect (AudioServer.get-bus-effect 0 0))
    (.tween-property tween amplify-effect "volume_db" -80.0 5)
    (yield tween "finished")
    (.stop music)
    (.set amplify-effect "volume_db" 0)
    (when (!= null queued-track)
      (.set-stream music queued-track)
      (.play music)))
  null)

(define (queue-track stream)
  (when (not (.is-connected music "finished" self "_on_Music_finished"))
    (.connect music "finished" self "_on_Music_finished"))
  (set! queued-track stream)
  (when (not (.is-playing music))
    (.set-stream music queued-track)
    (.play music))
  null)

(signal quit)
(define (quit)
  (fade-out-music)
  (emit-signal "quit"))

(define (play-open)
  (when (!= null ui) (.play (.get-node ui "Open"))))

(define (play-close)
  (when (!= null ui) (.play (.get-node ui "Close"))))

(define const BUS_COUNT 3)
(define const buses-path "user://buses")

(define (load-volume)
  (define file (.new File))
  (when (file.file-exists buses-path)
    (when (== OK (.open file buses-path File.READ))
      (for ([bus (range BUS_COUNT)])
        (define dbl (clamp (.get-double file) 0 1))
        (when (and (== OK (.get-error file))
                   (0 . <= . dbl)
                   (dbl . <= . 1))
          (AudioServer.set-bus-volume-db bus (linear2db dbl))))
      (.close file))))

(define (save-volume)
  (define file (.new File))
  (.open file buses-path File.WRITE)
  (for ([bus (range BUS_COUNT)])
    (.store-double file (db2linear (AudioServer.get-bus-volume-db bus))))
  (.close file))

(define (_ready)
  (add-child music)
  (load-volume)
  (set! music.bus "Music")
  (randomize))
