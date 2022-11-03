#lang gdlisp

(extends Spatial)
(require threading)

(define (export NodePath) target-path)
(define onready target (get-node target-path))

(define const TARGET_DISTANCE 4)
(define const POS_Y 4)
(define const TRACK_SPEED 6)

(define collision-exceptions [])

(define (_ready)
  (let loop ([node target])
    (when (is node RigidBody)
      (.append collision-exceptions (.get-rid target)))
    (when (!= null node)
      (recur loop (.get-parent node))))

  (set-as-toplevel true))

(define (_physics-process delta)
  (when (!= null target-path)
    (define target-pos
      (+ target.global-translation
         (* (.normalized (* target.global-transform.basis Vector3.BACK))
            TARGET_DISTANCE)))
    (define new-pos (.move-toward translation target-pos (* TRACK_SPEED delta)))
    (set! new-pos.y POS_Y)

    (define ds (~> (get-world) .get-space PhysicsServer.space-get-direct-state))

    (define col (ds.intersect-ray target.global-translation new-pos collision-exceptions))

    (when (not (.empty col))
      (set! new-pos col.position))

    (when (!= new-pos target-pos)
      (look-at-from-position new-pos target.global-translation Vector3.UP)))
  null)
