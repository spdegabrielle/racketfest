#lang racket/base

(provide (all-defined-out))

(define LEVEL-WIDTH 50)
(define LEVEL-HEIGHT 13)

(define (oob? x y)
  (or (>= x LEVEL-WIDTH)
      (< x 0)
      (>= y LEVEL-HEIGHT)
      (< y 0)))

(define default-terrain-speed
  6)

(define (terrain-halted? speed)
  (not speed))

(define (terrain-screaming? speed)
  (<= speed 2))

(define (terrain-very-fast? speed)
  (<= speed 4))

(define (terrain-fast? speed)
  (< speed 6))

(define (terrain-normal? speed)
  (eqv? speed 6))

(define (terrain-slow? speed)
  (> speed 6))
