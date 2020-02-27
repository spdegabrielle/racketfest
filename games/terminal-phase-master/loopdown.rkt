#lang racket/base

(provide ^loopdown ^countdown)

(require goblins/actor-lib/methods
         racket/match)

(define (^countdown bcom steps [start-at steps])
  (define (next n)
    (methods
     [(counter) n]
     [(zero?)
      (zero? n)]
     [(sub1)
      (unless (zero? n)
        (bcom (next (sub1 n))))]
     [(reset)
      (bcom (next steps))]))
  (next start-at))

(define (next-step steps)
  (match steps
    [(? number?)
     steps]
    [(? procedure?)
     (steps)]))

;; counts down, but loops
(define (^loopdown bcom steps [start-at (next-step steps)])
  (define (next n)
    (methods
     [(counter) n]
     [(zero?)
      (zero? n)]
     [(sub1)
      (if (zero? n)
          (bcom (next (next-step steps)))
          (bcom (next (sub1 n))))]))
  (next start-at))

(module+ test
  (require goblins
           rackunit)
  (define am (make-actormap))
  (define ldown
    (actormap-spawn! am ^loopdown 3))
  (check-equal?
   (actormap-peek am ldown 'counter)
   3)
  (check-equal?
   (actormap-peek am ldown 'zero?)
   #f)
  (actormap-poke! am ldown 'sub1)
  (check-equal?
   (actormap-peek am ldown 'counter)
   2)
  (check-equal?
   (actormap-peek am ldown 'zero?)
   #f)
  (actormap-poke! am ldown 'sub1)
  (check-equal?
   (actormap-peek am ldown 'counter)
   1)
  (check-equal?
   (actormap-peek am ldown 'zero?)
   #f)
  (actormap-poke! am ldown 'sub1)
  (check-equal?
   (actormap-peek am ldown 'counter)
   0)
  (check-equal?
   (actormap-peek am ldown 'zero?)
   #t)
  (actormap-poke! am ldown 'sub1)
  (check-equal?
   (actormap-peek am ldown 'counter)
   3)
  (check-equal?
   (actormap-peek am ldown 'zero?)
   #f)

  (define cdown
    (actormap-spawn! am ^countdown 2))
  (check-equal?
   (actormap-peek am cdown 'counter)
   2)
  (actormap-poke! am cdown 'sub1)
  (check-equal?
   (actormap-peek am cdown 'counter)
   1)
  (actormap-poke! am cdown 'sub1)
  (check-equal?
   (actormap-peek am cdown 'counter)
   0)
  (actormap-poke! am cdown 'sub1)
  (check-equal?
   (actormap-peek am cdown 'counter)
   0)
  (actormap-poke! am cdown 'reset)
  (check-equal?
   (actormap-peek am cdown 'counter)
   2)

  (define ldown-proc
    (actormap-spawn! am ^loopdown
                     (let ([n 1])
                       (lambda ()
                         (let ([cur-n n])
                           (set! n (add1 n))
                           cur-n)))))
  
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   1)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #f)
  (actormap-poke! am ldown-proc 'sub1)
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   0)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #t)
  (actormap-poke! am ldown-proc 'sub1)
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   2)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #f)
  (actormap-poke! am ldown-proc 'sub1)
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   1)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #f)
  (actormap-poke! am ldown-proc 'sub1)
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   0)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #t)
  (actormap-poke! am ldown-proc 'sub1)
  (check-equal?
   (actormap-peek am ldown-proc 'counter)
   3)
  (check-equal?
   (actormap-peek am ldown-proc 'zero?)
   #f))
