#lang racket/base

(provide (all-defined-out))

;;; Position/rendering/collision info
;;; =================================

(struct posinfo
  (x y              ; x & y coords
   char color       ; character and color
   layer            ; used both for rendering-order and collision category
   bg-color)        ; background color (or #f)
  #:transparent)

(define (make-posinfo x y char color layer [bg-color #f])
  (posinfo x y char color layer bg-color))

(define (recolor-posinfo pinfo color)
  (struct-copy posinfo pinfo
               [color color]))

(module+ simpler-posinfo
  (provide (rename-out [make-posinfo posinfo])
           posinfo?
           posinfo-x posinfo-y
           posinfo-char posinfo-color
           posinfo-layer posinfo-bg-color
           recolor-posinfo))
