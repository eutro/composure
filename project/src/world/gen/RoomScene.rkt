#lang gdlisp

tool
(extends Spatial)
(require threading)

(define (export NodePath) gridmap)
(define (export NodePath) anchors)
(define (export NodePath) entities)
(define (export Array int) edge-items)

(define (compute-edges)
  (define gm (get-node gridmap))
  (when (== null gm)
    (push-error "GridMap not found")
    (#%gdscript "return null"))

  (define ret {})
  (for ([item edge-items])
    (for ([cell (.get-used-cells-by-item gm item)])
      (set! (ref ret (Vector2 (int cell.x) (int cell.z))) true)))
  (.keys ret))

(define (compute-anchors)
  (define ac (get-node anchors))
  (when (== null ac)
    (push-error "Anchors not found")
    (#%gdscript "return null"))
  (define gm (get-node gridmap))
  (define ret [])
  (for ([anc (.get-children ac)])
    (define map-pos (gm.world-to-map anc.translation))
    (.append ret
             [(Vector2 (int map-pos.x) (int map-pos.z))
              anc.type
              (.get-direction anc)]))
  ret)

(define (compute-packed)
  (when (== null (get-node entities))
    (push-error "Entities not found")
    (#%gdscript "return null"))

  ;; this is a weird hack, but here I'm going to pack this,
  ;; instance it, and harvest the nodes from that
  (define self-packed (PackedScene.new))
  (define err (.pack self-packed self))
  (when (!= OK err)
    (push-error (+ "Failed to pack self: " (str err)))
    (#%gdscript "return null"))

  (define self-dup (.instance self-packed))
  (define entities-packed (.pack-entities self-dup anchors))
  (.free self-dup)
  entities-packed)

(define (compute-tiles)
  (define gm (get-node gridmap))
  (define tiles (gm.get-used-cells))
  (define ret {})
  (for ([tile tiles])
    (define x (int tile.x))
    #;(define y (int tile.y))
    (define z (int tile.z))
    (set! (ref ret (Vector2 x z)) true
          #;
          [(gm.get-cell-item x y z)
           (gm.get-cell-item-orientation x y z)]))
  (.keys ret))

(define (pack-entities anchors)
  (define pck (PackedScene.new))
  (remove-child (get-node anchors))
  (walk-children self self)
  (define err (.pack pck self))
  (cond
    [(== OK err)
     (define inst (.instance pck))
     (print "Inst tree:")
     (.print-tree inst)
     (.free inst)
     pck]
    [else
     (push-error (+ "Failed to pack entities: " (str err)))
     null]))

(define (walk-children new-owner node)
  (when (or (== node.owner self)
            (== node.owner null))
    (when (!= node new-owner)
      (set! node.owner new-owner))
    (for ([child (.get-children node)])
      (walk-children new-owner child))))

(define (to-room)
  (define ett (compute-packed))
  (when (== null ett) (#%gdscript "return null"))
  (define edges (compute-edges))
  (when (== null edges) (#%gdscript "return null"))
  (define acr (compute-anchors))
  (when (== null acr) (#%gdscript "return null"))

  (define room (.new (preload "Room.gd")))
  (set! room.edges edges)
  (set! room.anchors acr)
  (set! room.tiles (compute-tiles))
  (set! room.entities ett)
  room)
