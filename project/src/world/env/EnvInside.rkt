#lang gdlisp

(extends Spatial)

(define onready gridmap $GridMap)
(define onready placer (RoomPlacer.new gridmap))
(require threading)

(define (export Dictionary) rooms)

(define onready anchor-types : Array #;[[RoomSet]]
  [;; 0
   [(RoomPlacer.RoomSet.new
     [rooms.first-corridor]
     [1.0])]
   ;; 1
   [(RoomPlacer.RoomSet.new
     [rooms.corridor]
     [1.0])]
   ;; 2
   [(RoomPlacer.RoomSet.new
     [rooms.corridor]
     [1.0])]])

(define (ensure-neighbours)
  (define node
    (~> Game.world.player
        .get-position
        gridmap.world-to-map
        placer.project-xz
        placer.get-node-at))
  (when (!= node null) ;; nothing we can do otherwise
    (for ([anchor node.anchors])
      (for ([set (ref anchor-types anchor.type)])
        (when (placer.try-place-any anchor set)
          (#%gdscript "break"))))
    (.clear node.anchors)))

(define (init)
  (placer.force-place-room
   Transform.IDENTITY
   rooms.origin
   null))

(define (export String) save-to-file)

(define (_physics-process _delta)
  (ensure-neighbours)
  (when (!= null save-to-file)
    (define pck (.new PackedScene))
    (.pack pck self)
    (walk-children self)
    (ResourceSaver.save save-to-file pck)))

(define (walk-children node)
  (for ([child (.get-children node)])
    (set! child.owner self)
    (walk-children child)))
