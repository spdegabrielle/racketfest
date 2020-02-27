#lang racket/base

(provide ^total-score-tracker)

(require goblins
         goblins/actor-lib/methods
         (prefix-in raart: raart))

(define (^total-score-tracker bcom #:high-score [initial-high-score 0])
  (define-cell high-score
    initial-high-score)
  ;; for a single game
  (define (^single-score-tracker bcom [score 0])
    (methods
     [(add n)
      (let ([new-score
             (+ score n)])
        (when (> new-score ($ high-score))
          ($ high-score new-score))
        (bcom (^single-score-tracker bcom new-score)))]
     [(get-score)
      score]
     [(get-high-score)
      ($ high-score)]))

  (methods
   [(get-high-score)
    ($ high-score)]
   [(sprout)
    (spawn ^single-score-tracker)]))

(module+ test
  (require rackunit)
  (define am (make-actormap))
  (define total-score
    (actormap-spawn! am ^total-score-tracker))
  (check-equal?
   (actormap-peek am total-score 'get-high-score)
   0)
  (define single-score
    (actormap-poke! am total-score 'sprout))
  (check-equal?
   (actormap-poke! am single-score 'get-score)
   0)
  (actormap-poke! am single-score 'add 30)
  (check-equal?
   (actormap-poke! am single-score 'get-score)
   30)
  (check-equal?
   (actormap-poke! am single-score 'get-high-score)
   30)
  (check-equal?
   (actormap-poke! am total-score 'get-high-score)
   30)
  (actormap-poke! am single-score 'add 50)
  (check-equal?
   (actormap-poke! am single-score 'get-score)
   80)
  (check-equal?
   (actormap-poke! am single-score 'get-high-score)
   80)
  (check-equal?
   (actormap-poke! am total-score 'get-high-score)
   80)
  (define single-score2
    (actormap-poke! am total-score 'sprout))
  (check-equal?
   (actormap-poke! am total-score 'get-high-score)
   80)
  (actormap-poke! am single-score2 'add 300)
  (check-equal?
   (actormap-poke! am total-score 'get-high-score)
   300)
  (check-equal?
   (actormap-poke! am single-score2 'get-score)
   300)
  (check-equal?
   (actormap-poke! am single-score 'get-score)
   80)
  (check-equal?
   (actormap-poke! am single-score 'get-high-score)
   300))
