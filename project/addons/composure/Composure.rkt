#lang gdlisp

tool
(extends EditorPlugin)

(define dock)

(define (_enter-tree)
  (set! dock (.instance (load "res://addons/composure/RoomExport.tscn")))
  (set! dock.plugin self)
  (add-control-to-dock DOCK_SLOT_LEFT_UR dock)
  null)

(define (_exit-tree)
  (remove-control-from-docks dock)
  (.queue-free dock)
  null)
