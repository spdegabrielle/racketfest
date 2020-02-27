#lang racket/base

(provide (all-defined-out))

(require (prefix-in raart: raart)
         racket/match)

;;; Some raart utilities
;;;

(define (raart-labeled-val label val #:goal-width [goal-width 18])
  (define (horizontal-whitespace len)
    (raart:blank len 1))
  (define blank-space-size
    (max 1 (- goal-width
              (string-length label)
              (string-length val)
              1)))
  (raart:happend
   (raart:fg 'brwhite (raart:text
                       (string-append label
                                      ":")))
   (horizontal-whitespace blank-space-size)
   (raart:fg 'white
             (raart:text val))))

(define (val->raart val)
  (match val
    [(? raart:raart?) val]
    [(? procedure?) (val)]))

(define (list->raart lst)
  (map (lambda (v)
         (val->raart v))
       lst))

(define (spaced-blank w h)
  (define canvas
    (raart:blank w h))
  (for/fold ([canvas canvas])
            ([h-i h])
    (for/fold ([canvas canvas])
              ([w-i w])
      (raart:place-at canvas h-i w-i
                      (raart:char #\space)))))

(define (center-over back front)
  (raart:place-at back
                  (- (quotient (raart:raart-h back) 2)
                     (quotient (raart:raart-h front) 2))
                  (- (quotient (raart:raart-w back) 2)
                     (quotient (raart:raart-w front) 2))
                  front))
