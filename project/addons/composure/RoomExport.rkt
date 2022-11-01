#lang gdlisp

tool
(extends Control)

(define plugin)

(define (_on-Button-pressed)
  (define ei (.get-editor-interface plugin))
  (define root (ei.get-edited-scene-root))
  (cond
    [(.is-valid (funcref root "to_room")) (save-room root)]
    [else (.popup $NotGridmap)])
  null)

(define (save-room node)
  (define room (.to-room node))
  (when (== null room)
    (push_error "Failed to convert to room")
    (#%gdscript "return"))

  (define dialog (EditorFileDialog.new))

  (.set-margins-preset dialog PRESET_WIDE)
  (.set-anchor dialog MARGIN_LEFT 0.2)
  (.set-anchor dialog MARGIN_TOP 0.2)
  (.set-anchor dialog MARGIN_RIGHT 0.8)
  (.set-anchor dialog MARGIN_BOTTOM 0.8)

  (.add-filter dialog "*.tres; Rooms")

  (.set-title dialog "Export Room")

  (.set-current-path dialog (.get-basename node.filename))

  (add-child dialog)
  (.popup dialog)
  (define file (yield dialog "file_selected"))
  (define err (ResourceSaver.save file room))
  (cond
    [(!= OK err)
     (push_error (+ "Failed to save room: " (str err)))]
    [else
     (print "Saved room to " file)]))
