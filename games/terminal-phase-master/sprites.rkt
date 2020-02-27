#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/function
         goblins
         goblins/actor-lib/methods
         goblins/actor-lib/add-sub
         "no-op.rkt"
         (submod "posinfo.rkt" simpler-posinfo)
         "level-const.rkt"
         "loopdown.rkt"
         "aim.rkt")

(require pk)

;;; Sprites and other goblin'y things
;;; =================================

(define base-sprite
  (methods
   [tick no-op]
   [collide no-op]
   [posinfo (const '())]
   [level-advance no-op]))

(define (^player-ship bcom ticky lives x y)
  (define-cell moved-this-tick? #f)
  (define-cell fire-cooldown 0)

  (define base-ship
    (methods
     #:extends base-sprite
     [(dead?)
      ($ ticky 'dead?)]
     ;; Stubs.
     [move-down no-op]
     [move-up no-op]
     [move-left no-op]
     [move-right no-op]
     [fire no-op]))

  (define (move-deathcheck)
    ($ ticky 'last-collide
       (alive-posinfo) 'player-move
       (λ _ ($ ticky 'die)))
    ;; simplifies the "and" which calls this
    #t)

  (define (alive-posinfo)
    (posinfo ($ x) ($ y)
             #\> 'bryellow
             'player))

  ;; The four different player states:
  ;; active -> dying -> pause-before-resurrection -> dying
  (define active
    (methods
     #:extends base-ship
     [(tick)
      ;; Reset whether we moved this tick or not
      ($ moved-this-tick? #f)

      ;; Decrease the fire cooldown, if appropriate
      (unless (= ($ fire-cooldown) 0)
        ($ fire-cooldown (sub1 ($ fire-cooldown))))]
     ;; returns a boolean (whether we moved or not)
     [(move-up)
      (unless (or ($ moved-this-tick?)
                  (<= ($ y) 0))
        ($ moved-this-tick? #t)
        (cell-sub1 y)
        #t)
      #f]
     [(move-down)
      (unless (or ($ moved-this-tick?)
                  (>= ($ y) (sub1 LEVEL-HEIGHT)))
        ($ moved-this-tick? #t)
        (cell-add1 y)
        #t)
      #f]
     [(move-left)
      (unless (or ($ moved-this-tick?)
                  (<= ($ x) 0))
        ($ moved-this-tick? #t)
        (cell-sub1 x)
        #t)
      #f]
     [(move-right)
      (unless (or ($ moved-this-tick?)
                  (>= ($ x) (sub1 LEVEL-WIDTH)))
        ($ moved-this-tick? #t)
        (cell-add1 x)
        #t)
      #f]
     [(fire)
      (when (= ($ fire-cooldown) 0)
        ($ ticky 'to-tick
           (lambda (ticky)
             (spawn ^player-bullet ticky
                    ($ x) ($ y))))
        ($ fire-cooldown 10))]
     [(posinfo)
      (alive-posinfo)]
     [(collide with with-posinfo phase)
      (match (posinfo-layer with-posinfo)
        [(or 'enemy 'enemy-bullet 'terrain)
         ;; time to die
         (bcom (dying))]
        [_ 'no-op])]))

  (define alive
    (methods
     #:extends active
     ;; Extend all movement actions to do a move-deathcheck
     [(move-up)
      (and (active 'move-up)
           (move-deathcheck))]
     [(move-down)
      (and (active 'move-down)
           (move-deathcheck))]))

  (define (dying)
    (define-cell death-countdown
      (* 30 2))
    (methods
     #:extends base-ship
     [(tick)
      ;; one step closer to death...
      (cell-sub1 death-countdown)
      ;; and heck, maybe we're dead
      (when (zero? ($ death-countdown))
        (cell-sub1 lives)
        (if (zero? ($ lives))
            ($ ticky 'die)  ; death by ticky is true death
            (bcom (pause-before-resurrection))))]
     [(posinfo)
      (match (modulo (quotient ($ death-countdown) 6) 6)
        [0 (posinfo ($ x) ($ y)
                    #\% 'brred
                    'player)]
        [1 (posinfo ($ x) ($ y)
                    #\# 'bryellow
                    'player)]
        [2 '()]
        [3 (posinfo ($ x) ($ y)
                    #\+ 'red
                    'player)]
        [4 (posinfo ($ x) ($ y)
                    #\# 'yellow
                    'player)]
        [5 '()])
      ]))

  (define (pause-before-resurrection)
    (define-cell resurrect-countdown
      30)
    (methods
     #:extends base-ship
     [(tick)
      (cell-sub1 resurrect-countdown)
      (when (zero? ($ resurrect-countdown))
        (bcom (resurrecting)))]))

  (define (resurrecting)
    (define-cell resurrection-countdown
      (* 30 4))
    (methods
     #:extends active
     [(tick)
      ;; call the main tick method first
      (active 'tick)
      ;; now reduce time to resurrection
      (cell-sub1 resurrection-countdown)
      (when (zero? ($ resurrection-countdown))
        (bcom alive))]
     ;; disable handling collisions while resurrecting
     [collide no-op]
     ;; TODO: disable firing while resurrecting?
     #;[(fire) no-op]
     ;; flicker in and out of reality
     [(posinfo)
      (if (even? (quotient ($ resurrection-countdown) 6))
          (posinfo ($ x) ($ y)
                   #\> 'bryellow
                   'player)
          '())]))

  alive)

(define BULLET-COUNTDOWN 1)

(define (^player-bullet bcom ticky
                        start-x y)
  (define-cell x start-x)
  (define-cell countdown BULLET-COUNTDOWN)

  (define (get-posinfos)
    (define cur-x ($ x))
    (define head
      (posinfo (add1 ($ x)) y #\=
               'yellow 'player-bullet))
    (define tail
      (posinfo ($ x) y #\-
               'yellow 'player-bullet))
    (if (< (add1 cur-x) LEVEL-WIDTH)
        (list tail head)
        tail))

  (define (collide with with-posinfo phase)
    (match (posinfo-layer with-posinfo)
      [(or 'enemy 'terrain)
       ;; time to die
       ($ ticky 'die)]
      ['enemy-bullet
       (when ($ with 'kills-player-bullet?)
         ($ ticky 'die))]
      [_ 'no-op]))

  (methods
   #:extends base-sprite
   [(tick)
    ;; advance x if appropriate
    (cond
      [(= ($ countdown) 0)
       ($ x (+ ($ x) 2))
       ;; See if we would have died based on our new position
       ;; based on what was previously here; if so, die
       ($ ticky 'last-collide
          (get-posinfos) 'main-tick
          collide)
       ;; Now decrease countdown
       ($ countdown BULLET-COUNTDOWN)]
      [else
       (cell-sub1 countdown)])

    (unless ($ ticky 'dead?)
      (when (>= ($ x) LEVEL-WIDTH)
        ;; we've gone too far
        ($ ticky 'die)))

    (void)]
   [(posinfo)
    ;; Write head only if it hasn't gone oob
    (get-posinfos)]
   [collide collide]))


;;; =======
;;; Enemies
;;; =======

;; alias, for now.  but we may add methods later
(define base-enemy base-sprite)

;;  - tracks health
;;  - handles PLAYER BULLET collisions only.  Other collision handling
;;    is passed off to the player
;;  - handles the display, usually (unless you override it)

(define (make-healthy-mixin ticky initial-health
                            #:hit-color [hit-color 'brwhite])
  ;; TODO: This is a bit wasteful, because there's no need to track
  ;;   health or flashing if the character doesn't have health.
  (define health
    (spawn ^countdown initial-health))

  (define flashing
    (spawn ^countdown 6 0))

  (define (hit-posinfo pinfo)
    (recolor-posinfo pinfo hit-color))

  (define (mixin base)
    (methods
     #:extends base
     ;; remove one from the flashing counter then apply base tick
     ;; method
     [(tick)
      ($ flashing 'sub1)
      (base 'tick)]
     [(collide with with-posinfo phase)
      (cond
        ;; Got hit by a player bullet...
        [(eq? (posinfo-layer with-posinfo) 'player-bullet)
         ($ health 'sub1)
         ($ flashing 'reset)
         (when ($ health 'zero?)
           ($ ticky 'die
              #:points? #t))]
        ;; otherwise, let the base handle it
        [else (base 'collide with with-posinfo phase)])]
     [(posinfo)
      (define base-posinfos
        (base 'posinfo))
      (if ($ flashing 'zero?)
          ;; not flashing, show it as-is
          base-posinfos
          ;; otherwise, we need to invert all of them
          (if (pair? base-posinfos)
              (map hit-posinfo base-posinfos)
              (hit-posinfo base-posinfos)))]))
  mixin)


;;; Ships
;;; -----

(define enemy:<:move-speed
  4)
(define enemy:<:fire-speed
  (* enemy:<:move-speed 20))

(define (move-forward ticky x y)
  (cell-sub1 x))

(define ((make-fire-ray #:collide? [collide? #f])
         ticky bullet-x bullet-y)
  ($ ticky 'to-tick
     (lambda (ticky)
       (spawn ^enemy-bullet:ray ticky bullet-x bullet-y
              #:collide? collide?))))

(define fire-ray (make-fire-ray))
(define fire-collide-ray (make-fire-ray #:collide? #t))

(define (never-fire . args)
  'no-op)

(define (mover-ship-constructor char color
                                #:move-speed [move-speed enemy:<:move-speed]
                                #:fire-speed [fire-speed enemy:<:fire-speed]
                                #:move [move-proc move-forward]
                                #:fire [fire-proc fire-ray])
  (lambda (bcom ticky initial-x initial-y)
    (define-cell x initial-x)
    (define-cell y initial-y)
    (define move-countdown
      (spawn ^loopdown move-speed))
    (define fire-countdown
      (spawn ^loopdown fire-speed (quotient fire-speed 3)))

    (methods
     #:extends base-enemy
     [(tick)
      (when ($ move-countdown 'zero?)
        (move-proc ticky x y))
        
      (cond
        ;; Left the screen?  time to die
        [(oob? ($ x) ($ y))
         ($ ticky 'die)]

        ;; Still on screen?  Guess we're still alive
        [else
         ;; Fire a bullet if it's bullet firing time
         (when ($ fire-countdown 'zero?)
           (let ([bullet-x (sub1 ($ x))]
                 [bullet-y ($ y)])
             (unless (oob? bullet-x bullet-y)
               (fire-proc ticky bullet-x bullet-y))))

         ($ move-countdown 'sub1)
         ($ fire-countdown 'sub1)
         (void)])]
     [(posinfo)
      (posinfo ($ x) ($ y) char color
               'enemy)]
     [(collide with with-posinfo phase)
      (match (posinfo-layer with-posinfo)
        [(or 'player-bullet)
         ;; time to die
         ($ ticky 'die #:points? #t)]
        [_ 'no-op])])))

(define ^enemy:<
  (mover-ship-constructor #\< 'yellow
                          #:fire no-op))

(define ^enemy:<-firing
  (mover-ship-constructor #\< 'cyan))

(define ^enemy:<-firing-collides
  (mover-ship-constructor #\< 'green
                          #:fire fire-collide-ray))

(define squiggler-pattern
  #(-1 -1 0 0 0 +1 +1 +1 0 0 0 -1))

(define (^enemy:squiggler bcom ticky initial-x initial-y
                          #:slightly-slow? [slightly-slow? #f])
  (define squiggle-pos
    (spawn ^loopdown (sub1 (vector-length squiggler-pattern))))
  (define (move-squiggle ticky x y)
    ;; first move to the left
    (cell-sub1 x)
    ;; then squiggle
    (define squiggle-offset
      (vector-ref squiggler-pattern
                  ($ squiggle-pos 'counter)))
    ($ y (+ ($ y) squiggle-offset))  ; set y to y plus squiggle-offset
    ;; push down squiggle counter
    ($ squiggle-pos 'sub1))
  
  (define ship-constructor
    (mover-ship-constructor #\S
                            (if slightly-slow?
                                'blue
                                'brblue)
                            #:move-speed (if slightly-slow?
                                             5 4)
                            #:move move-squiggle
                            #:fire never-fire))
  (ship-constructor bcom ticky initial-x initial-y))

(define (^enemy:slightly-slow-squiggler bcom ticky initial-x initial-y)
  (^enemy:squiggler bcom ticky initial-x initial-y
                    #:slightly-slow? #t))

(define (^enemy:nymph bcom ticky initial-x initial-y
                      #:move-speed [move-speed 3]
                      #:shuf-countdown [shuf-countdown-every 5])
  (define shuf-countdown
    (spawn ^loopdown shuf-countdown-every))

  (define (shuf-closer ticky x y)
    ;; first, move forward
    (move-forward ticky x y)
    (when ($ shuf-countdown 'zero?)
      ;; now determine if we need to "slide" closer to the player
      (match-define (cons _player-x player-y)
        ($ ticky 'player-pos))
      (cond
        [(< ($ y) player-y)
         (cell-add1 y)]
        [(> ($ y) player-y)
         (cell-sub1 y)]))
    ($ shuf-countdown 'sub1))

  (define ship-constructor
    (mover-ship-constructor #\n 'magenta
                            #:move-speed move-speed
                            #:fire never-fire
                            #:move shuf-closer))

  (ship-constructor bcom ticky initial-x initial-y))

;; Kamikaze moves in, tries to align itself with the player,
;; locks on, and then RAPIDLY hurtles towards the player
(define (^enemy:kamikaze bcom ticky initial-x initial-y)
  (define-cell x initial-x)
  (define-cell y initial-y)

  (define healthy-mixin
    (make-healthy-mixin ticky 3))

  (define base-kamikaze
    (methods
     #:extends base-enemy
     [(posinfo)
      (posinfo ($ x) ($ y) #\K 'brmagenta
               'enemy)]
     [(collide with with-posinfo phase)
      (match (posinfo-layer with-posinfo)
        [(or 'player 'terrain)
         ;; time to die
         ($ ticky 'die)]
        [_ 'no-op])]))

  (define (plod-in)
    (define move-countdown
      (spawn ^loopdown 5))
    (define plods-left
      (spawn ^countdown 2))

    (healthy-mixin
     (methods
      #:extends base-kamikaze
      [(tick)
       (when ($ move-countdown 'zero?)
         (move-forward ticky x y)
         ($ plods-left 'sub1))
       ($ move-countdown 'sub1)
       (when ($ plods-left 'zero?)
         (bcom (aim-self)))])))

  (define (aim-self)
    (define shuf-countdown
      (spawn ^loopdown 12))
    (define shufs-left
      (spawn ^countdown 8))

    (healthy-mixin
     (methods
      #:extends base-kamikaze
      [(tick)
       (when ($ shuf-countdown 'zero?)
         (match-define (cons _player-x player-y)
           ($ ticky 'player-pos))
         (cond
           [(< ($ y) player-y)
            (cell-add1 y)]
           [(> ($ y) player-y)
            (cell-sub1 y)])
         ($ shufs-left 'sub1))

       ($ shuf-countdown 'sub1)

       (when ($ shufs-left 'zero?)
         (bcom (pause-and-flash)))])))

  (define (pause-and-flash)
    (define flash-countdown
      (spawn ^countdown 15))

    (healthy-mixin
     (methods
      #:extends base-kamikaze
      [(tick)
       ($ flash-countdown 'sub1)
       (when ($ flash-countdown 'zero?)
         (bcom race-to-death))]
      [(posinfo)
       (posinfo ($ x) ($ y) #\K
                (if (even? (quotient ($ flash-countdown 'counter) 5))
                    'red
                    'brmagenta)
                'enemy)])))

  (define race-to-death
    (healthy-mixin
     (methods
      #:extends base-kamikaze
      [(tick)
       (move-forward ticky x y)
       (when (oob? ($ x) ($ y))
         ($ ticky 'die))])))

  (plod-in))

(define (screenslider-mixin base
                            x y ticky)
  (methods
   #:extends base
   [(level-advance)
    (cell-sub1 x)
    ;; die if we pass off screen
    (when (oob? ($ x) ($ y))
      ($ ticky 'die))]))

(define turret-fire-speed
  80)

(define (^enemy:turret bcom ticky initial-x initial-y
                       ;; #:health [initial-health 0]
                       )
  (define-cell x initial-x)
  (define-cell y initial-y)

  (define healthy-mixin
    (make-healthy-mixin ticky 3
                        #:hit-color 'brred))

  (define fire-cooldown
    (spawn ^loopdown turret-fire-speed 30))

  (healthy-mixin
   (methods
    #:extends (screenslider-mixin base-enemy
                                  x y ticky)
    [(tick)
     (when ($ fire-cooldown 'zero?)
       ($ ticky 'to-tick
          (λ (ticky)
            (match-define (cons player-x player-y)
              ($ ticky 'player-pos))
            (unless (and (eqv? ($ x) player-x)
                         (eqv? ($ y) player-y))
              (spawn ^enemy-bullet:drifter ticky
                     ($ x) ($ y)
                     player-x player-y
                     ;; Penalize bullets that are moving to the right
                     ;; to handle visual weirdness
                     #:speed (if (>= (- player-x ($ x)) 0)
                                 0.115 .2))))))
     ($ fire-cooldown 'sub1)]

    [(posinfo)
     (posinfo ($ x) ($ y)
              #\I 'brwhite
              'enemy)]

    ;; we also die on player impact
    [(collide with with-posinfo phase)
     (match (posinfo-layer with-posinfo)
       ['player
        ($ ticky 'die)]
       [_ 'no-op])])))

;;; Bullets
;;; -------

(define enemy-bullet:move-speed
  2)

(define (^enemy-bullet:ray bcom ticky x y
                           #:collide? [collide? #t])
  (define-spawned move-countdown
    ^loopdown enemy-bullet:move-speed)
  (define (next x)
    (methods
     #:extends base-sprite
     [(tick)
      ($ move-countdown 'sub1)
      (define next-x
        (if ($ move-countdown 'zero?)
            (sub1 x)
            x))
      (cond
        [(oob? next-x y)
         ($ ticky 'die)]
        [else
         (bcom (next next-x))])]
     [(posinfo)
      (posinfo x y #\-
               (if collide?
                   'brcyan
                   'white)
               'enemy-bullet)]
     [(collide with with-posinfo phase)
      (match (posinfo-layer with-posinfo)
        [(or 'player 'terrain)
         ;; time to die
         ($ ticky 'die)]
        ['player-bullet
         (when collide?
           ($ ticky 'die))]
        [_ 'no-op])]
     [(kills-player-bullet?)
      collide?]
     ))
  (next x))

(define (^enemy-bullet:drifter bcom ticky
                               start-x start-y
                               target-x target-y
                               #:speed [speed 0.2]
                               #:lifetime [lifetime #f]
                               #:char [char #\*]
                               #:color [color 'brwhite]
                               #:shootable? [shootable? #f]
                               #:crashable? [crashable? #t])
  (when (and (eqv? start-x target-x)
             (eqv? start-y target-y))
    (error "Can't fire at self"))

  (define aimer
    (spawn ^aimer
           start-x start-y
           target-x target-y
           #:speed speed))
  (define lifetime-tracker
    (and lifetime
         (spawn ^countdown lifetime)))

  (methods
   #:extends base-sprite
   [(tick)
    ($ aimer 'move)

    ;; decrement lifetime if appropriate
    (when lifetime
      (when ($ lifetime-tracker 'zero?)
        ($ ticky 'die))
      ($ lifetime-tracker 'sub1))

    (match-define (vector x y)
      ($ aimer 'pos))
    (when ($ ticky 'oob? x y)
      ($ ticky 'die))]
   [(posinfo)
    (match-define (vector x y)
      ($ aimer 'pos))
    (posinfo x y char color
             'enemy-bullet)]
   [(collide with with-posinfo phase)
    (match (posinfo-layer with-posinfo)
      ['player
       ;; time to die
       ($ ticky 'die)]
      ['player-bullet
       (when shootable?
         ($ ticky 'die))]
      ['terrain
       (when crashable?
         ($ ticky 'die))]
      [_ 'no-op])]
   [(kills-player-bullet?)
    shootable?]))

(define (^enemy-mine bcom ticky initial-x initial-y)
  (define-cell x initial-x)
  (define-cell y initial-y)
  (define healthy-mixin
    (make-healthy-mixin ticky 2))

  (healthy-mixin
   (methods
    #:extends (screenslider-mixin base-enemy
                                  x y ticky)
    [(posinfo)
     (posinfo ($ x) ($ y)
              #\+ 'brgreen
              'enemy)]
    ;; Additionally, we die if a player collides with us
    [(collide with with-posinfo phase)
     (match (posinfo-layer with-posinfo)
       ['player
        ;; time to die
        ($ ticky 'die #:points? #t)]
       [_ 'no-op])])))
