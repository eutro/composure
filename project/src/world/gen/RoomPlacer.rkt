#lang gdlisp

(class-name RoomPlacer)
(require "../../macros.rkt"
         threading)

(defrecord Anchor
  ([pos : Vector2]
   [type : int]
   [dir : Vector2])

  (define (get-transform-to o)
    (define rotation-angle
      (- (dir.angle-to (- o.dir))))
    (define target-pos
      (.rotated
       (+ pos (dir.normalized) (Vector2 0.5 0.5))
       (- rotation-angle)))
    (define translation
      (- (+ o.pos (Vector2 0.5 0.5)) target-pos))
    (define xf
      (Transform
       (Basis Vector3.UP rotation-angle)
       (Vector3 (round translation.x)
                0
                (round translation.y))))
    xf))

(define (parse-raw-anchor raw)
  (Anchor.new (ref raw 0) (ref raw 1) (ref raw 2)))

(defrecord RoomNode
  ([anchors : Dictionary #;{Anchor true}]))

(defrecord RoomSet
  ;; (must be equal lengths)
  ([rooms : Array #;[CRoom]]
   ;; NOT going to prefix sum, (re)indexing it is O(n) anyway
   ;; and I CBA to implement a Fenwick tree
   [chances : Array #;[float]]))

(splice-record
 ([common-parent : Node]
  [cell-size : float]))

;; there's um, a lot of `double` equality going on here,
;; in theory they're all just integers so it should be fine(tm)
(define occupied : Dictionary #;{Vector2 RoomNode})

(define (get-node-at [pos : Vector2])
  (.get occupied (Vector2 (floor pos.x) (floor pos.y)) null))

(define (try-place-any
         ;; erasing this anchor is the caller's responsibility
         [anchor : Anchor]
         #;"not m*tated" [set : RoomSet])
  ;; I don't think I can make such maze-style generation deterministic
  ;; and seedable, since the order in which anchors are forced
  ;; (i.e. the path of the player) can change the generation,
  ;; hence no seed or rng passed in here
  (define exclusions {})
  (define chance-sum 1.0)
  (let loop ()
    (define x (randf))
    (define r 0.0)
    (for ([i (range (len set.rooms))])
      (when (not (.has exclusions i))
        (define c (ref set.chances i))
        (+set! r (/ c chance-sum))
        (when (x . <= . r)
          (define success
            (try-place-room anchor (ref set.rooms i)))
          (cond
            [success (#%gdscript "return true")]
            [else
             (set! (ref exclusions i) true)
             (-set! chance-sum c)
             (#%gdscript "break")]))))
    (cond
      [(== (len exclusions) (len set.rooms))
       false ;; every room failed
       ]
      [else (recur loop)])))

(define (try-place-room
         [anchor : Anchor]
         [room : CRoom])
  (define idcs (range (len room.anchors)))
  (.shuffle idcs)
  (for ([i idcs])
    (define room-anchor (parse-raw-anchor (ref room.anchors i)))
    (when (== room-anchor.type anchor.type)
      (when (try-place-room-with-anchor anchor room room-anchor)
        (#%gdscript "return true"))))
  false)

(define (project-xz pos)
  (Vector2 pos.x pos.z))

(define (unproject-xz pos)
  (Vector3 pos.x 0 pos.y))

(define (xf-pos3d xf pos)
  (define xfp (xf.xform (+ pos (Vector3 0.5 0 0.5))))
  (Vector3 (floor xfp.x) pos.y (floor xfp.z)))

(define (xf-pos2d xf pos)
  (~> pos unproject-xz (xf-pos3d xf _) project-xz))

(define (round-vec2 pos)
  (Vector2 (round pos.x) (round pos.y)))

(define (try-place-room-with-anchor
         [global-anchor : Anchor]
         [room : CRoom]
         [room-anchor : Anchor])
  (define xf (room-anchor.get-transform-to global-anchor))
  (for ([edge room.edges])
    (define edge-xf (xf-pos2d xf edge))
    (when (.has occupied edge-xf)
      (#%gdscript "return false")))

  ;; all clear! we can place the room
  (force-place-room xf room room-anchor)
  true)

(define (force-place-room [xf : Transform] [room : CRoom] maybe-anchor)
  (define node (RoomNode.new {}))
  (for ([raw-pos room.tiles])
    (define pos (xf-pos2d xf raw-pos))
    (set! (ref occupied pos) node))
  (for ([raw-anchor room.anchors])
    (define anchor (parse-raw-anchor raw-anchor))
    (when (or (== null maybe-anchor)
              (!= anchor.pos maybe-anchor.pos))
      (define xf-pos (round-vec2 (xf-pos2d xf anchor.pos)))
      (define xf-dir (round-vec2 (project-xz (xf.basis.xform (unproject-xz anchor.dir)))))
      (set! (ref node.anchors (Anchor.new xf-pos anchor.type xf-dir))
            true)))
  (define entities (.instance room.entities))
  (fset! entities.transform.basis ~> (* xf.basis _))
  (fset! entities.transform.origin + (* cell-size xf.origin))
  (.add-child common-parent entities))
