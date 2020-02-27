#lang info
(define collection "terminal-phase")
(define deps '("lux"
               "goblins"
               "pk"
               "raart"
               "ansi"))
(define pkg-desc
  "A space shooter game that runs in your terminal!")
(define version "1.0")
(define pkg-authors '(cwebber))
(define racket-launcher-names '("terminal-phase"))
(define racket-launcher-libraries '("terminal-phase.rkt"))
(define raco-commands
  '(("terminal-phase" (submod terminal-phase/terminal-phase main)
                      "Play terminal-phase game"
                      #f)))
