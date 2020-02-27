#lang racket/base

(provide ^starfield prime-starfield)

(require racket/set
         "braille-rast.rkt"
         goblins
         goblins/actor-lib/methods
         (prefix-in raart: raart))

;; Periods make perfectly fine starfields, it appears...

;;    .        .
;;  .     .    
;;     .     .     .
;;   .    .    .    
;; .    .        .

;; Hm, only for the intro.  It won't work for the game because what
;; will make the stars look nice is that they move at a different
;; speed that doesn't look like the objects I think.

;; What we really ought to use is the unicode braille characters.
;; So that's what we do.

;; Note that this makes the wireworld proposal of using periods messier.
;; This may mean that we should move to something else like single/double
;; quotes, like so:

;; '''   "'
;;   '   '
;;   '@*''

(struct star (x y relative-speed on? blink-time))

(define (^starfield bcom width height
                    #:till-spawn-min [till-spawn-min 0.3]
                    #:till-spawn-max [till-spawn-max 2.4])
  (define-cell stars
    (set))

  (define (oob? x)
    (<= x 0))

  (define (new-blink-on-time)
    (* (random 10 200) .1))
  (define (new-blink-off-time)
    (* (random 300 1000) .1))

  (define (make-new-star initial-distance)
    (define y
      (random 0 height))
    (define relative-speed
      (+ (* (- (random 1 15) 10) .1) 1.0))
    (define on?
      (zero? (random 4)))
    (define blink-time
      (if on?
          (new-blink-off-time)
          (new-blink-on-time)))
    (star (- width initial-distance) y
          relative-speed on? blink-time))

  (define (new-till-spawn)
    (random 5 30))

  (define (next till-spawn stars)
    (methods
     [(advance speed)
      (define base-distance
        (/ 1.0 (+ speed .1)))
      (define new-stars
        (for/fold ([new-stars (set)])
                  ([this-star stars])
          (define star-distance
            (* base-distance (star-relative-speed this-star)))
          (define new-x
            (- (star-x this-star) star-distance))
          (cond
            ;; drop this star
            [(oob? new-x)
             new-stars]
            ;; Otherwise, we'll add it back having updated some
            ;; properties
            [else
             (define blink-time
               (- (star-blink-time this-star) star-distance))
             (define on?
               (star-on? this-star))
             (when (<= blink-time 0)
               (set! on? (not on?))
               (set! blink-time
                     (if on?
                         (new-blink-off-time)
                         (new-blink-on-time))))
             (define new-star
               (struct-copy star this-star
                            [x new-x]
                            [on? on?]
                            [blink-time blink-time]))
             (set-add new-stars new-star)])))

      (define next-till-spawn
        (- till-spawn base-distance))
      (when (<= next-till-spawn 0)
        (set! new-stars (set-add new-stars (make-new-star base-distance)))
        (set! next-till-spawn (new-till-spawn)))

      (bcom (next next-till-spawn new-stars))]
     [(render)
      (define grid
        (make-vector height #f))
      (for ([i height])
        (vector-set! grid i (make-vector width #f)))
      (for ([star stars])
        (when (star-on? star)
          (define y (star-y star))
          (define x
            (inexact->exact (floor (star-x star))))
          (vector-set! (vector-ref grid (star-y star))
                       x 'SS)))
      (define braille-lines
        (brailleify-grid/list grid))

      (raart:fg 'brblack
                (raart:vappend*
                 (map raart:text braille-lines)))]))
  (next (new-till-spawn) (set)))

(define (prime-starfield starfield width)
  (for ([i (* width 5)])
    ($ starfield 'advance 1.0)))

(module+ main
  (define am
    (make-actormap))
  (define starfield
    (actormap-spawn! am ^starfield 100 45))
  (actormap-run!
   am (λ () (prime-starfield starfield 30)))
  (raart:draw-here (actormap-peek am starfield 'render))
  )
