#lang racket/base

(provide ^level ^level-manager)

(require pk
         racket/match
         racket/set
         racket/format
         racket/stream)

(require (prefix-in raart: raart)
         goblins
         goblins/actor-lib/methods
         goblins/actor-lib/ticker2
         "sprites.rkt"
         "no-op.rkt"
         "level-const.rkt"
         "level-tape.rkt"
         "posinfo.rkt"
         "raart-render.rkt"
         "credits.rkt"
         "starfield.rkt")

(define render-layer-order
  '(powerup
    explosion
    terrain
    enemy-bullet
    player-bullet
    enemy
    player))

(define (make-empty-terrain-vector)
  (let ([empty-col
         (vector->immutable-vector
          (make-vector LEVEL-HEIGHT #f))])
    (make-vector LEVEL-WIDTH empty-col)))

(define empty-terrain-vector
  (vector->immutable-vector
   (make-empty-terrain-vector)))

(define (terrain-push-col terrain-vec col-vec)
  (define new-terrain
    (make-empty-terrain-vector))

  ;; copy over all terrain except first element, shifted over one
  (vector-copy! new-terrain 0
                terrain-vec 1)

  ;; blit in the new column
  (vector-set! new-terrain (sub1 LEVEL-WIDTH) col-vec)

  (vector->immutable-vector new-terrain))


(define (^terrain bcom [terrain-vec empty-terrain-vector])
  (define (terrain-ref x y)
    (vector-ref (vector-ref terrain-vec x) y))

  (define posinfos
    (for/fold ([posinfos '()])
              ([col terrain-vec]
               [x (in-naturals)])
      (for/fold ([posinfos posinfos])
                ([cell col]
                 [y (in-naturals)])
        (if (terrain-ref x y)
            (cons (posinfo x y
                           #\# 'red
                           'terrain #f)
                  posinfos)
            posinfos))))

  (methods
   [tick no-op]
   ;; Push a column of terrain (a vector of... booleans?)
   ;; onto the terrain-vec
   [(push-col col-vec)
    (bcom (^terrain bcom (terrain-push-col terrain-vec col-vec)))]
   ;; Write out the full terrain as a list
   [(posinfo)
    posinfos]
   ;; We're not the ones affected by a collision... the object
   ;; colliding is!
   [collide no-op]
   ;; TODO: I guess maybe the level advancement code goes... here?
   ;;   Not sure.
   [level-advance no-op]))

(define terrain-speed-control-chars
  #hasheq((#\0 . 0)
          (#\1 . 1)
          (#\2 . 2)
          (#\3 . 3)
          (#\4 . 4)
          (#\5 . 5)
          (#\6 . 6)
          (#\7 . 7)
          (#\8 . 8)
          (#\9 . 9)
          (#\! . 10)
          (#\@ . 12)
          (#\# . 14)
          (#\$ . 16)
          (#\% . 18)
          (#\^ . 20)
          (#\& . 22)
          (#\& . 24)
          (#\* . 26)
          (#\( . 28)
          (#\) . 30)))

(define (terrain-speed-char? char)
  (hash-has-key? terrain-speed-control-chars char))

(define (^level bcom level-tape score-tracker lives
                #:starfield? [starfield? #t])
  (define tape-eater
    (spawn ^tape-eater level-tape))

  (define-cell ticked-this-turn?
    #f)

  (define-cell terrain-speed
    default-terrain-speed)

  (define level-ticker (spawn-ticker))

  (define-cell exit-reason #f)

  ;; set up starfield, if appropriate
  (define starfield
    (and starfield?
         (spawn ^starfield (* LEVEL-WIDTH 2) (* LEVEL-HEIGHT 4))))
  (when starfield?
    (prime-starfield starfield (* LEVEL-WIDTH 2)))

  ;; We add some extra methods to this ticky that let this object
  ;; do some things:
  ;;  - Add points to the score on death
  (define (^custom-ticky bcom orig-ticky this-obj-box
                         #:points [points #f])
    (define (default-other-on-collide with this-posinfo phase)
      ($ with 'collide (unbox this-obj-box) this-posinfo phase))
    (methods
     #:extends orig-ticky
     ;; Give points for death via the ticky
     [(die #:points? [points? #f])
      (when (and points points?)
        ($ score-tracker 'add points))
      ($ orig-ticky 'die)]
     ;; Check if we're out of bounds
     ;; (This will change with possibly changing level sizes in the future)
     [(oob? x y)
      (oob? x y)]
     ;; Collides with the last layout?
     [(last-collide check-posinfos phase collide-this 
                    #:collide-other
                    [collide-other default-other-on-collide])
      (define this-obj
        (unbox this-obj-box))
      ;; Who else is here?
      (for ([check-posinfo (match check-posinfos
                             [(list items ...)
                              items]
                             [posinfo (list posinfo)])])
        (unless (oob? (posinfo-x check-posinfo)
                      (posinfo-y check-posinfo))
          (define vector-pos
            (posinfo->vector-pos check-posinfo))
          (for ([co-inhabitant (vector-ref ($ last-layout) vector-pos)])
            (match-define (vector other-ticked other-posinfo)
              co-inhabitant)
            (unless (eq? other-ticked this-obj)
              (collide-this other-ticked other-posinfo phase)
              (collide-other other-ticked check-posinfo phase)))))]

     ;; TODO: we're adding keyword arguments... we might want to accept at
     ;;   this point that this should be named something else?  Dunno.
     ;; TODO: I'm lost about what to do about this-obj-box
     [(to-tick give-ticky #:points [points 0])
      ($ orig-ticky 'to-tick
         (λ (ticky)
           (define new-obj-box (box #f))
           (define customized-ticky
             (spawn ^custom-ticky ticky new-obj-box))
           (define new-refr
             (give-ticky customized-ticky))
           (set-box! new-obj-box new-refr)
           new-refr))]

     [(player-pos)
      (cons ($ player-x) ($ player-y))]

     [(ticked-this-turn?)
      ($ ticked-this-turn?)]

     [(terrain-speed)
      ($ terrain-speed)]))

  (define-cell loop?
    #f)

  (define-cell player-x 1)
  (define-cell player-y
    (floor (/ LEVEL-HEIGHT 2)))

  ;; Define the player and spawn a ticked facet of it
  ;; TODO: Yet another reduntant way of setting up the custom-ticky...
  (define player-ship
    ($ level-ticker 'to-tick
       (lambda (ticky)
         (define new-obj-box (box #f))
         (define customized-ticky
           (spawn ^custom-ticky ticky new-obj-box))
         (define player-refr
           (spawn ^player-ship customized-ticky lives player-x player-y))
         (set-box! new-obj-box player-refr)
         player-refr)))

  (define terrain
    ($ level-ticker 'to-tick
       (lambda (ticky)
         (spawn ^terrain))))

  ;; TODO: Store last layout when generating the level
  (define-cell last-layout)

  (define (posinfo->vector-pos this-posinfo)
    (+ (* LEVEL-HEIGHT (posinfo-x this-posinfo))
       (posinfo-y this-posinfo)))

  ;; Calculate collisions and inform participants.
  ;; The big, general collision detection.
  (define (do-collisions)
    (define level-vector
      (make-vector (* LEVEL-WIDTH LEVEL-HEIGHT) '()))
    ;; Fill vector 
    ($ level-ticker 'foldr
       (lambda (ticked _)
         (define posinfos
           (match ($ ticked 'posinfo)
             [(list items ...)
              items]
             [posinfo (list posinfo)]))
         (for [(this-posinfo posinfos)]
           (define vector-pos
             (posinfo->vector-pos this-posinfo))
           (vector-set! level-vector vector-pos
                        (cons (vector ticked this-posinfo)
                              (vector-ref level-vector vector-pos)))))
       #f)
    ($ last-layout level-vector)
    ;; Go through and inform of collisions
    (for ([co-habitants level-vector]
          [vec-i (in-naturals)])
      (match co-habitants
        ;; zero or more co-habitants is no real collision
        ['()
         'no-op]
        [(list one-entity)
         'no-op]
        ;; ok, inform colliders
        [_
         (define informed-collisions
           (make-hasheq))
         (for ([this-inhabitant co-habitants])
           (for ([other-inhabitant co-habitants])
             ;; skip if the same
             (unless (eq? this-inhabitant other-inhabitant)
               ;; otherwise, inform me
               (match-define (vector this-ticked this-posinfo)
                 this-inhabitant)
               (match-define (vector other-ticked other-posinfo)
                 other-inhabitant)
               ;; Set of objects this-ticked has already collided with
               (define already-collided
                 (if (hash-has-key? informed-collisions this-ticked)
                     (hash-ref informed-collisions this-ticked)
                     (let ([new-collided (mutable-seteq)])
                       (hash-set! informed-collisions this-ticked
                                  new-collided)
                       new-collided)))
               (unless (set-member? already-collided other-ticked)
                 ($ this-ticked 'collide
                    other-ticked other-posinfo 'super-collide)
                 (set-add! already-collided other-ticked)))))])))

  (define (advance-level [level-right-x (sub1 LEVEL-WIDTH)])
    (call/ec
     (lambda (return)
       (match-define (vector cells-column level-instructions)
         ($ tape-eater))

       ;; For now we loop forever.  This won't be the case later,
       ;; when levels will end.
       (unless cells-column
         (cond
           ;; We're set to loop infinitely... ok
           [($ loop?)
            (set! tape-eater
                  (spawn ^tape-eater level-tape))
            (match-let ([(vector new-cells-column new-level-instructions)
                         ($ tape-eater)])
              (set! cells-column new-cells-column)
              (set! level-instructions new-level-instructions))]
           ;; otherwise the level completed
           [else
            (return 'level-over)]))

       ;; Process any level instructions.
       (for ([level-instruction level-instructions])
         (match level-instruction
           [(? terrain-speed-char?)
            (define new-speed
              (hash-ref terrain-speed-control-chars
                        level-instruction))
            ($ terrain-speed new-speed)]
           [#\L ($ loop? #t)]
           [_ 'no-op]))

       ;; Now we walk through the column and build up the new
       ;; terrain-col
       (define terrain-col
         (for/fold ([terrain-cells '()]
                    #:result (list->vector
                              (reverse terrain-cells)))
                   ([cell cells-column]
                    [y (in-naturals)])
           (match cell
             [(cons #\# _)
              (cons #t terrain-cells)]
             [(cons tile-char flavor-char)
              (define (spawn-ticked-enemy constructor points)
                ($ level-ticker 'to-tick
                   (λ (ticky)
                     (define obj-box
                       (box #f))
                     (define score-ticky
                       (spawn ^custom-ticky ticky obj-box
                              #:points points))
                     (define enemy
                       (spawn constructor score-ticky level-right-x y))
                     (set-box! obj-box enemy)
                     enemy)))
              (match tile-char
                [#\<
                 (match flavor-char
                   [#\f (spawn-ticked-enemy ^enemy:<-firing 10)]
                   [#\F (spawn-ticked-enemy ^enemy:<-firing-collides 10)]
                   ;; default
                   [_ (spawn-ticked-enemy ^enemy:< 10)])]
                [#\S
                 (match flavor-char
                   [#\s
                    (spawn-ticked-enemy ^enemy:slightly-slow-squiggler 15)]
                   [_ (spawn-ticked-enemy ^enemy:squiggler 15)])]
                [#\n
                 (spawn-ticked-enemy ^enemy:nymph 20)]
                [#\K
                 (spawn-ticked-enemy ^enemy:kamikaze 20)]
                [#\I
                 (spawn-ticked-enemy ^enemy:turret 40)]
                [#\+
                 (spawn-ticked-enemy ^enemy-mine 10)]
                [_ 'no-op])
              (cons #f terrain-cells)]
             [#f
              (cons #f terrain-cells)])))
       
       ($ terrain 'push-col terrain-col)

       ;; Inform all ticked items that the level has advanced
       ($ level-ticker 'foldr
          (λ (ticked _)
            ($ ticked 'level-advance))
          #f))))

  (define header
    (raart:fg 'brwhite
              (raart:text ".-*[ TERMINAL PHASE ]*-.")))
  (define blank-level-canvas
    (raart:blank LEVEL-WIDTH LEVEL-HEIGHT))

  (define base-methods
    (methods
     [(exit-reason) ($ exit-reason)]
     [(render)
      (define blit-layers
        ($ level-ticker 'foldr
           (lambda (ticked layers)
             (define (add-to-layers posinfo layers)
               (define layer
                 (posinfo-layer posinfo))
               (define current-occupants
                 (hash-ref layers layer '()))
               (hash-set layers layer
                         (cons posinfo current-occupants)))
             (match ($ ticked 'posinfo)
               ;; nothing to do
               [(or '() #f)
                layers]
               [(list posinfos ...)
                (foldl add-to-layers layers posinfos)]
               [posinfo (add-to-layers posinfo layers)]))
           #hasheq()))

      (define backdrop-canvas
        (if starfield?
            ($ starfield 'render)
            blank-level-canvas))

      (define level-canvas
        (for/fold ([canvas backdrop-canvas])
                  ([layer render-layer-order])
          (for/fold ([canvas canvas])
                    ([this-posinfo (hash-ref blit-layers layer '())])
            (match-define (posinfo x y char color layer-type bg-color)
              this-posinfo)
            (define colored-char
              (raart:fg color (raart:char char)))
            (define bg-char
              (if bg-color
                  (raart:bg bg-color colored-char)
                  colored-char))
            (raart:place-at canvas y x bg-char))))

      (define footer
        (raart:place-at
         (raart:place-at
          (raart:blank LEVEL-WIDTH 2)
          0 (- LEVEL-WIDTH 18) ; 18 is width of HI-SCORE + zeroes
          (raart:vappend
           #:halign 'right
           (raart-labeled-val "SCORE"
                              (~r ($ score-tracker 'get-score)
                                  #:min-width 8 #:pad-string "0"))
           (raart-labeled-val "HI-SCORE"
                              (~r ($ score-tracker 'get-high-score)
                                  #:min-width 8 #:pad-string "0"))))
         0 0
         (raart:vappend
          #:halign 'left
          (raart-labeled-val "LIVES" (number->string ($ lives))
                             #:goal-width 10)
          (raart-labeled-val "LEVEL" "1"
                             #:goal-width 10))))

      (raart:vappend
       #:halign 'center
       header
       (raart:frame level-canvas)
       footer)]
     ;; you'll need to implement handle-event though
     [tick no-op]))

  (define (running [tape-countdown ($ terrain-speed)])
    (methods
     #:extends base-methods
     [(tick)
      (when starfield?
        ($ starfield 'advance ($ terrain-speed)))

      (call/ec
       (lambda (return)
         ;; maybe advance tape
         (cond
           [(zero? tape-countdown)
            ($ ticked-this-turn? #t)
            (define advance-result
              (advance-level))
            ;; Level's over?  End it now by congratulating
            ;; the player!
            (when (eq? advance-result 'level-over)
              (return (bcom you-win)))]
           [else
            ($ ticked-this-turn? #f)])

         ;; Tick all the level items
         ($ level-ticker 'tick 'tick)

         ;; collision detection time
         (do-collisions)

         (cond
           [($ player-ship 'dead?)
            (bcom game-over)]
           [else
            ;; decrement/reset tape counter
            (bcom (running
                   (if (zero? tape-countdown)
                       ($ terrain-speed)
                       (sub1 tape-countdown))))])))]

     [(handle-event evt)
      (match evt
        ["<up>"
         ($ player-ship 'move-up)]
        ["<down>"
         ($ player-ship 'move-down)]
        ["<left>"
         ($ player-ship 'move-left)]
        ["<right>"
         ($ player-ship 'move-right)]
        [" "
         ($ player-ship 'fire)]
        [(or "q" "C-[")
         ($ exit-reason 'quit)]
        [_
         'no-op])]))

  (define (say-it-state say-this give-exit-reason)
    (methods
     #:extends base-methods
     [(render)
      (define game-render
        (base-methods 'render))
      (define say-over-box
        (raart:frame say-this))
      ;; place the say-over-box centered
      (raart:place-at
       game-render
       (- (quotient (raart:raart-h game-render) 2)
          (quotient (raart:raart-h say-over-box) 2))
       (- (quotient (raart:raart-w game-render) 2)
          (quotient (raart:raart-w say-over-box) 2))
       say-over-box)]
     [(handle-event evt)
      (match evt
        [(or "q" " " "C-M" "C-[")
         ($ exit-reason give-exit-reason)]
        [_ 'no-op])]))

  (define game-over
    (say-it-state (raart:text " Game Over! ") 'game-over))

  (define you-win
    (say-it-state (raart:text " Success! ") 'beat-level))

  ;; Fast forward the level to its starting position
  (for ([i LEVEL-WIDTH])
    (advance-level i))

  ;; Do a first path of running collisions
  ;; (hopefully there aren't any immediately!)
  (do-collisions)

  (running))

(define (^level-manager bcom levels dpr-pushdown score-tracker)
  (define-cell lives
    3)

  (define (next-level-state levels)
    (if (stream-empty? levels)
        (victory)
        (playing levels)))
  (define (playing levels)
    (define level-tape
      (stream-first levels))
    (define level
      (spawn ^level level-tape score-tracker lives))
    
    (define (maybe-handle-exit-method)
      (define exit-reason
        ($ level 'exit-reason))
      (match exit-reason
        [#f 'no-op]
        [(or 'quit 'game-over)
         ($ dpr-pushdown 'pop)]
        ['beat-level
         (bcom (next-level-state (stream-rest levels)))]))

    (methods
     #:extends level
     [(tick)
      ;; tick the level first
      ($ level 'tick)
      (maybe-handle-exit-method)]
     [(handle-event evt)
      ($ level 'handle-event evt)
      (maybe-handle-exit-method)]))
  (define (victory)
    (methods
     [(render)
      (raart:frame
       (raart:vappend
        #:halign 'center
        (raart:text " YOU WIN THE GAME ")
        (raart:text "")
        (raart:text " Having fought mighty foes, ")
        (raart:text " you held on strong and have emerged ")
        (raart:text " victorious!  Congratulations! ")
        (raart:text "")
        (raart:text "")
        (raart:text " (... upcoming releases will have ")
        (raart:text " more levels!) ")))]
     [tick no-op]
     [(handle-event evt)
      (match evt
        [(or "q" " " "C-M" "C-[")
         ($ dpr-pushdown 'pop)
         ;; and now push on the credits
         ($ dpr-pushdown 'push
            (spawn ^credits dpr-pushdown))]
        [_ 'no-op])]))
  (next-level-state levels))

