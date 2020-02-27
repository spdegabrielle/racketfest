#lang racket/base

(require racket/match
         racket/cmdline

         (prefix-in raart: raart)
         ansi
         lux

         ;; debugging
         pk

         goblins
         goblins/actor-lib/ticker2
         goblins/actor-lib/facet
         goblins/actor-lib/imply-method
         goblins/actor-lib/pushdown

         "sprites.rkt"
         "posinfo.rkt"
         "raart-render.rkt"
         "main-menu.rkt")

(define (^starfield bcom)
  'TODO)

;;; Game infrastructure
;;; ===================

(define (make-raart-render-game)
  (define prev-width
    #f)
  (define prev-height
    #f)
  (define cached-blank
    #f)

  (λ (dispatcher gw)
    ;; Find out what we're going to render and see if a special
    ;; background color is specified (otherwise use black)
    (define-values (to-display bg-color)
      (call-with-values
       (λ ()
         ($ dispatcher 'render))
       (λ (to-display [bg-color 'black])
         (values (or to-display (raart:blank 0 0))
                 bg-color))))
    ;; Get the width and height
    (define width
      (game-display-cols gw))
    (define height
      (game-display-rows gw))

    ;; Regenerate and cache the blank if necessary
    (unless (and cached-blank
                 (= width prev-width)
                 (= height prev-height))
      (set! prev-width width)
      (set! prev-height height)
      (set! cached-blank
            (spaced-blank width height)))

    (raart:fg 'white
              (raart:bg 'black (center-over cached-blank to-display)))))

(define current-renderer
  (make-parameter (make-raart-render-game)))

;;; Each column is a single column of current terrain.
;;; They're happend'ed together.

(struct game
  (;; actormap of participants
   actormap
   ;; dispatcher of ticks, events
   dispatch-stack
   ;; pd automata pushdown (used to check if empty)
   pushdown
   ;; resolution info
   display-rows
   display-cols)
  #:methods gen:word
  [(define (word-tick gw)
     ;; Actors we'll call
     (define dispatch-stack (game-dispatch-stack gw))
     ;; Transactionally update actors for this tick.
     ;; Either all of this succeeds or nothing succeeds.
     (define (update!)
       ;; run all objects
       ($ dispatch-stack 'tick))
     (actormap-run! (game-actormap gw)
                    update!)  ; test optimizing with: #:reckless? #t
     gw)
   (define (word-event gw e)
     (match e
       ;; update screen resolution
       [(screen-size-report rows cols)
        (struct-copy game gw
                     [display-rows rows]
                     [display-cols cols])]
       ;; Give it to the current dispatcher
       [_
        (define (dispatcher-handle-event!)
          ($ (game-dispatch-stack gw) 'handle-event e))
        (actormap-run! (game-actormap gw)
                       dispatcher-handle-event!)  ; test optimizing with: #:reckless? #t
        ;; quit if there's no dispatcher on the stack
        (if (actormap-peek (game-actormap gw)
                           (game-pushdown gw)
                           'empty?)
            #f gw)]))
   (define (word-output gw)
     (define (compose-display)
       ((current-renderer) (game-dispatch-stack gw) gw))
     (define cols (game-display-cols gw))
     (define rows (game-display-rows gw))
     (define too-small?
       (or (< rows 22)
           (< cols 60)))
     (cond
       [too-small?
        (raart:vappend
         #:halign 'left
         (raart:text "Terminal too small!")
         (raart:text "60x22 minimum!"))]
       [else
        ;; Return the raart to be displayed.
        ;; Note that we use actormap-run rather than -run! because
        ;; there shouldn't be any applicable side effects.
        (actormap-run (game-actormap gw)
                      compose-display)]))
   (define (word-label gw frame-time)
     (define game-name "> Terminal Phase <")
     (if (show-fps?)
         (lux-standard-label game-name frame-time)
         game-name))
   (define (word-fps gw)
     ;; 30?  60???
     ;; probably 30 is something terminals can reliably
     ;; keep up with...
     30.0)
   (define (word-return gw)
     (void))])

(define (make-spawn-ticked ticker-register)
  (make-keyword-procedure
   (lambda (kws kw-args constructor . args)
     (define ref
       (keyword-apply spawn kws kw-args constructor args))
     ($ ticker-register ref)
     ref)))

(define (new-game rows cols level-files)
  (define actormap (make-actormap))
  (define (make-new-game)
    (match-define (list dispatcher-pushdown dispatcher-forwarder)
      (spawn-pushdown-pair))
    (define main-menu
      (spawn ^main-menu dispatcher-pushdown level-files))
    ($ dispatcher-pushdown 'push main-menu)

    (game actormap
          dispatcher-forwarder dispatcher-pushdown
          rows cols))
  (actormap-run! actormap make-new-game
                 #:reckless? #t))


(define (display* . things)
  (for-each display things)
  (flush-output))

(define (with-clean-tty proc)
  (tty-raw!)
  (display* (dec-soft-terminal-reset)
            (device-request-screen-size))
  #;(match-define (screen-size-report rows columns)
    (lex-lcd-input (current-input-port)))
  (define proc-result
    #;(proc 24 80)
    (proc 22 72))
  #;(display* (dec-soft-terminal-reset)
            (clear-screen/home))
  proc-result)

(define (start-game level-files)
  #;(with-handlers ([exn:fail?
                   (lambda (err)
                     #;(display* (dec-soft-terminal-reset)
                                 (clear-screen/home))
                     #;(display* (clear-screen/home)
                                 (dec-reset-margins)
                                 
                                 (dec-soft-terminal-reset)
                                 (xterm-full-reset)
                                 )
                     #;(displayln "hi")
                     #;((error-display-handler) (exn-message err) err)
                     #;(displayln "there"))]))
  (with-clean-tty
    (lambda (rows cols)
      (call-with-chaos
       (raart:make-raart)
       (lambda ()
         (fiat-lux (new-game rows cols level-files))))))
  (void))

(define show-fps?
  (make-parameter #f))

(module+ main
  (define level-files
    (command-line
     #:once-each
     [("-d" "--debug-file")
      debug-file-arg
      "Print current-error-port to this debug file"
      (let ([debug-file (open-output-file debug-file-arg
                                          #:exists 'append)])
        (current-error-port debug-file))]
     [("-f" "--show-fps")
      "Show current FPS"
      (show-fps? #t)]
     #:args level-files
     level-files))

  (start-game (match level-files
                ['() #f]
                [_ level-files])))
