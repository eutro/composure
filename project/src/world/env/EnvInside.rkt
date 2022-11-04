#lang gdlisp

(extends Spatial)

(define onready gen-root $GenRoot)

(define const CELL_SIZE 2)

(define onready placer (RoomPlacer.new gen-root CELL_SIZE))
(require threading)

(define (export NodePath) player)

(define onready rooms
  {
   "origin" (preload "../gen/rooms/inside/Origin.tres")

   "first_corridor" (preload "../gen/rooms/inside/FirstCorridor.tres")

   "corridor" (preload "../gen/rooms/inside/Corridor.tres")
   "corridor2" (preload "../gen/rooms/inside/Corridor2.tres")
   "corridor3" (preload "../gen/rooms/inside/Corridor3.tres")
   "corridor4" (preload "../gen/rooms/inside/Corridor4.tres")

   "room1" (preload "../gen/rooms/inside/Room1.tres")
   "room2" (preload "../gen/rooms/inside/Room2.tres")
   "roomtiny" (preload "../gen/rooms/inside/RoomTiny.tres")})

(define onready anchor-types : Array #;[[RoomSet]]
  [;; 0
   [(RoomPlacer.RoomSet.new
     [rooms.first-corridor]
     [1.0])]
   ;; 1
   [(RoomPlacer.RoomSet.new
     [rooms.corridor rooms.corridor4 rooms.corridor2]
     [0.1 0.05 0.85])
    (RoomPlacer.RoomSet.new
     [rooms.corridor3]
     [1.0])]
   ;; 2
   [(RoomPlacer.RoomSet.new
     [rooms.room1 rooms.room2]
     [0.5 0.5])
    (RoomPlacer.RoomSet.new
     [rooms.roomtiny]
     [1.0])]])

(define (ensure-neighbours)
  (define node
    (~> (get-node player)
        .-global-translation
        (/ CELL_SIZE)
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

(define (_physics-process _delta)
  (maybe-dbg-save)
  (ensure-neighbours)
  null)

(define (export String) save-to-file)

(define (maybe-dbg-save)
  (when (and (!= null save-to-file)
             (not (.empty save-to-file)))
    (define pck (.new PackedScene))
    (.pack pck self)
    (walk-children self)
    (ResourceSaver.save save-to-file pck)))

(define (walk-children node)
  (for ([child (.get-children node)])
    (set! child.owner self)
    (walk-children child)))
