#lang racket/base

(provide pwd)

(require racket/runtime-path)

(define-runtime-path pwd
  ".")
