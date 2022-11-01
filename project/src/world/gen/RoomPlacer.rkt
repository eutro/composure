#lang gdlisp

(class-name RoomPlacer)
(require "../../macros.rkt")

(defrecord Anchor
  ([pos : Vector2]
   [type : int]
   [dir : Vector2])

  (define (get-transform-to o)
    (define translation (- o.pos (+ pos (dir.normalized))))
    (define rotation-angle (dir.angle-to (- o.dir)))
    (Transform (Basis Vector3.UP rotation-angle)
               (Vector3 (int translation.x)
                        0
                        (int translation.y)))))

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

(splice-record ([gridmap : GridMap]))

;; there's um, a lot of `double` equality going on here,
;; in theory they're all just integers so it should be fine(tm)
(define occupied : Dictionary #;{Vector2 RoomNode})

(define (get-node-at [pos : Vector2])
  (.get occupied (Vector2 (int pos.x) (int pos.y)) null))

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
        (define c (ref set.chances r))
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
  (for ([raw room.anchors])
    (define room-anchor (parse-raw-anchor raw))
    (when (== room-anchor.type anchor.type)
      (when (try-place-room-with-anchor anchor room room-anchor)
        (#%gdscript "return true"))))
  false)

(define (project-xz pos)
  (Vector2 (int pos.x) (int pos.z)))

(define (unproject-xz pos)
  (Vector3 (int pos.x) 0 (int pos.y)))

(define (try-place-room-with-anchor
         [global-anchor : Anchor]
         [room : CRoom]
         [room-anchor : Anchor])
  (define xf (room-anchor.get-transform-to global-anchor))
  (for ([edge room.edges])
    (define edge-xf (project-xz (xf.xform (unproject-xz edge))))
    (when (.has occupied edge-xf)
      (#%gdscript "return false")))

  ;; all clear! we can place the room
  (force-place-room xf room room-anchor)
  true)

(define (force-place-room [xf : Transform] [room : CRoom] maybe-anchor)
  (define node (RoomNode.new {}))
  (for ([raw-pos room.tiles])
    (define pos (xf.xform raw-pos))
    (match (ref room.tiles raw-pos)
      [[(var item) (var ori)]
       (.set-cell-item
        gridmap
        (int pos.x)
        (int pos.y)
        (int pos.z)
        item
        ori)])
    (set! (ref occupied (project-xz pos)) node))
  (for ([raw-anchor room.anchors])
    (define anchor (parse-raw-anchor raw-anchor))
    (when (or (== null maybe-anchor)
              (!= anchor.pos maybe-anchor.pos))
      (define xf-pos (project-xz (xf.xform (unproject-xz anchor.pos))))
      (define xf-dir (project-xz (xf.basis.xform (unproject-xz anchor.dir))))
      (set! (ref node.anchors (Anchor.new xf-pos anchor.type xf-dir))
            true)))
  (.add-child gridmap (.instance room.entities)))
