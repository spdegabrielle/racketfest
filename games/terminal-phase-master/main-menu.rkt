#lang racket/base

(provide ^main-menu)

(require racket/match
         (prefix-in raart: raart)
         goblins
         goblins/actor-lib/methods
         "credits.rkt"
         "no-op.rkt"
         "level-tape.rkt"
         "level.rkt"
         "logo.rkt"
         "score.rkt"
         "pwd.rkt")

(define (^main-menu bcom dpr-pushdown [level-files #f])
  (define total-score
    (spawn ^total-score-tracker))

  (define (start-game)
    (define levels-stream
      (match level-files
        [(? pair?)
         (list->levels-stream level-files)]
        [_ (list->levels-stream
            (map (λ (fname)
                   (build-path pwd (build-path "levels" fname)))
                 '("level1.txt" "level2.txt")))]))
    (define level-manager
      (spawn ^level-manager levels-stream dpr-pushdown
             ($ total-score 'sprout)))
    ($ dpr-pushdown 'push level-manager))

  (define (show-help)
    ($ dpr-pushdown 'push (spawn ^help-menu dpr-pushdown)))

  (define (show-credits)
    ($ dpr-pushdown 'push (spawn ^credits dpr-pushdown)))

  (define menu-options
    `#(("Play game!" ,start-game)
       ("Help" ,show-help)
       ("Show Credits" ,show-credits)))

  (define (next [selection 0])
    (methods
     [tick no-op]
     [(handle-event evt)
      (match evt
        ["<up>"
         (bcom (next (modulo (sub1 selection)
                             (vector-length menu-options))))]
        ["<down>"
         (bcom (next (modulo (add1 selection)
                             (vector-length menu-options))))]
        [(or " " "C-M")
         (match (vector-ref menu-options selection)
           [(list _option-text option-func)
            (option-func)])]
        [(or "q" "C-[")
         ($ dpr-pushdown 'pop)]
        [_
         'no-op])]
     [(render)
      (define menu-items
        (for/list ([option menu-options]
                   [i (in-naturals)])
          (match option
            [(list option-text _option-func)
             (if (= i selection)
                 (raart:fg 'bryellow
                           (raart:text
                            (string-append ">> "
                                           option-text
                                           " <<")))
                 (raart:fg 'white
                           (raart:text option-text)))])))
      (raart:vappend
       #:halign 'center
       (raart:frame (raart:inset 1 0 (raart:fg 'brwhite logo-raart)))
       (raart:blank 0 3)
       (apply
        raart:vappend
        #:halign 'center
        menu-items))]))
  (next))

(define (^help-menu bcom dpr-pushdown)
  (methods
   [tick no-op]
   [(handle-event evt)
    ($ dpr-pushdown 'pop)]
   [(render)
    (raart:frame
     (raart:vappend
      #:halign 'center
      (raart:text " Arrow keys: move ")
      (raart:text " Space: fire")
      (raart:blank 0 1)
      (raart:text " ... that's it!  Good luck!")))]))
