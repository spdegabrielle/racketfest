#lang racket/base

(provide ^aimer)

(require goblins
         goblins/actor-lib/methods)

;; We multiply the Y space by two and then scale it back down before
;; applying because otherwise the speed is goofy since characters are
;; so much taller than they are wide

(define (^aimer bcom
                start-x start-y
                target-x target-y
                #:speed [speed .3])
  (define dx
    (- target-x start-x))
  (define dy
    (* (- target-y start-y) 2.0))
  (define distance  ; or magnitude
    (sqrt (+ (* dx dx) (* dy dy))))
  (define normalized-dx
    (if (zero? distance)
        0.0
        (/ dx distance)))
  (define normalized-dy
    (if (zero? distance)
        0
        (/ dy distance)))

  (define (next traveled)
    (define (inexact-pos)
      (define cur-x
        (+ start-x (* traveled normalized-dx)))
      (define cur-y
        (+ start-y (/ (* traveled normalized-dy) 2.0)))
      (values cur-x cur-y))
    (methods
     [(move [speed speed])
      (bcom (next (+ traveled speed)))]
     [(pos)
      (define-values (x y)
        (inexact-pos))
      (vector (inexact->exact (floor x))
              (inexact->exact (floor y)))]
     [(precise-pos)
      (define-values (x y)
        (inexact-pos))
      (vector x y)]
     [(traveled)
      traveled]))
  (next 0))

(module+ test
  (require rackunit)
  (define am (make-actormap))

  (define aimer1
    (actormap-spawn! am ^aimer
                     1 2
                     4 8))

  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 2))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 2))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 2))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 2))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 3))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 3))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 3))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(1 3))
  (actormap-poke! am aimer1 'move)
  (check-equal?
   (actormap-peek am aimer1 'pos)
   #(2 4)))
