#lang racket/base

(provide read-level-tape open-level-tape ^tape-eater
         list->levels-stream)

(require racket/match
         racket/string
         racket/vector
         racket/stream
         racket/port)

;; Separate level text into flavor lines, tile lines, and count
;; of longest tile line
(define (read-level-tape level-text)
  (define text-lines
    (list->vector (string-split (string-trim level-text "\n"
                                             #:left? #f
                                             #:right? #t
                                             #:repeat? #t)
                                "\n")))
  (define split-line-nums
    (let lp ([lines (vector->list text-lines)]
             [count 0])
      (match lines
        ['() '()]
        [(list this-line rest-lines ...)
         (if (and (> (string-length this-line) 0)
                  (eq? (string-ref this-line 0) #\-))
             (cons count (lp rest-lines (add1 count)))
             (lp rest-lines (add1 count)))])))

  (define-values (flavor-lines tile-lines level-instruction-lines)
    (match split-line-nums
      ['()
       (values (vector) text-lines (vector))]
      [(list first-split)
       (values (vector-copy text-lines 0 first-split)
               (vector-copy text-lines (add1 first-split))
               (vector))]
      [(list first-split second-split)
       (values (vector-copy text-lines 0 first-split)
               (vector-copy text-lines (add1 first-split) second-split)
               (vector-copy text-lines (add1 second-split)))]))
  (define longest-tile-line
    (apply max (for/list ([l tile-lines])
                 (string-length l))))

  (define (flavor-tiles flavors tiles)
    (if (or (null? flavors)
            (null? tiles))
        ;; we've reached the end of either
        ;; so... that's it... the rest aren't flavor.
        (map (lambda (t)
               (cons t #f))
             tiles)
        (match-let ([(cons flavor rest-flavors)
                     flavors]
                    [(cons tile rest-tiles)
                     tiles])
          ;; Now we need to see if this is an "interesting tile"
          (match tile
            [(or #\space #f)
             ;; boring!  just cons it on as #f and
             ;; loop to the next one then
             (cons #f
                   (flavor-tiles flavors rest-tiles))]
            [_
             ;; ok it's an interesting tile.
             (cons (cons tile
                         (match flavor
                           [#\space #f]
                           [_ flavor]))
                   (flavor-tiles rest-flavors
                                 rest-tiles))]))))


  (define (strip-spaces cells)
    (remove #\space cells))

  (define (col-ref i)
    (define (line-cells lines)
      (for/list ([line lines])
        (if (< i (string-length line))
            (string-ref line i)
            #f)))
    (define flavor-cells
      (line-cells flavor-lines))
    (define tile-cells
      (line-cells tile-lines))
    (define level-instruction-cells
      (line-cells level-instruction-lines))
    (vector (flavor-tiles flavor-cells tile-cells)
            (strip-spaces level-instruction-cells)))

  ;; Build up the stream
  (let lp ([i 0])
    (if (= i longest-tile-line)
        empty-stream
        (stream-cons (col-ref i)
                     (lp (add1 i))))))

(define (open-level-tape level-filename)
  (read-level-tape
   (call-with-input-file level-filename
     port->string)))


;; An actor that "eats" the tape one column at a time, until
;; it reaches the end and returns #f
(define ((^tape-eater bcom level-tape))
  (if (stream-empty? level-tape)
      (vector #f #f)
      (values (bcom (^tape-eater bcom
                                 (stream-rest level-tape)))
              (stream-first level-tape))))

(define (list->levels-stream filenames)
  (match filenames
    ['() empty-stream]
    [(cons this-filename rest-filenames)
     (stream-cons (open-level-tape this-filename)
                  (list->levels-stream rest-filenames))]))

(module+ test
  (require goblins
           rackunit)
  (define am (make-actormap))
  (define test-level-tape
    (read-level-tape "\
1
23
  4
--
a e
bc
  f
 d
  g"))

  (define tape-eater
    (actormap-spawn! am ^tape-eater test-level-tape))

  (check-equal?
   (actormap-poke! am tape-eater)
   '#(((#\a . #\1)
       (#\b . #\2)
       #f
       #f
       #f)
      ()))
  (check-equal?
   (actormap-poke! am tape-eater)
   '#((#f
       (#\c . #f)
       #f
       (#\d . #\3)
       #f)
      ()))
  (check-equal?
   (actormap-poke! am tape-eater)
   '#(((#\e . #f)
       #f
       (#\f . #f)
       #f
       (#\g . #\4))
      ()))
  (check-equal?
   (actormap-poke! am tape-eater)
   #f))
