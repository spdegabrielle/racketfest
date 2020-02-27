#lang racket/base

(provide no-op)

(define no-op
  (make-keyword-procedure
   (lambda _ 'no-op)))
