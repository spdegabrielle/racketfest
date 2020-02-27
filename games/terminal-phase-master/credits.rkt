#lang at-exp racket/base

(provide ^credits)

(require racket/match
         racket/string
         racket/port
         (prefix-in raart: raart)
         goblins
         goblins/actor-lib/methods
         "no-op.rkt"
         "loopdown.rkt"
         "pwd.rkt")

(require pk)

(define credits-filename
  (build-path pwd "credits-roll.txt"))

(define (credits-lines)
  (call-with-input-file credits-filename
    (λ (p)
      (string-split (port->string p) "\n"))))

(define (^text-buffer bcom num-lines width)
  (define (next [wip-line '()]
                ;; We store all but one in here...
                ;; that last one is in the wip-line
                [stored-lines
                 (make-vector (sub1 num-lines)
                              (raart:text ""))])
    (define (wip-line->raart)
      (if (pair? wip-line)
          (raart:happend* (reverse wip-line))
          (raart:blank 0 1)))

    (methods
     [(insert-char char color)
      (bcom (next (cons (raart:fg color (raart:char char))
                        wip-line)
                  stored-lines))]
     [(crlf)
      (define new-stored-lines
        (make-vector (sub1 num-lines)
                     (wip-line->raart)))
      (vector-copy! new-stored-lines 1
                    stored-lines
                    0
                    (sub1 (vector-length stored-lines)))
      (bcom (next '() new-stored-lines))]
     [(render [with-cursor? #f])
      (define the-text
        (raart:vappend
         #:halign 'left
         (raart:vappend*
          #:halign 'left
          (reverse (vector->list stored-lines)))
         (wip-line->raart)))
      (define matted
        (raart:matte width num-lines
                     #:halign 'left
                     the-text))
      (define with-cursor
        (if with-cursor?
            (raart:place-at matted
                            (sub1 num-lines) (length wip-line)
                            (raart:fg 'brgreen (raart:char #\█)))  ; or ▒?
            matted))
      with-cursor]))
  (next))


(define (^credits bcom dpr-pushdown)
  (define text-buffer
    (spawn ^text-buffer 20 50))

  (define base-methods
    (methods
     [tick no-op]
     [(handle-event evt)
      (match evt
        [(or "q" "C-M" "C-[" " ")
         ($ dpr-pushdown 'pop)]
        [_
         'no-op])]
     [(render)
      ($ text-buffer 'render #t)]))

  (define hang-until-quit base-methods)

  #;(define (boot)
    'TODO)

  (define (make-blinker-mixin)
    (define-cell on? #t)
    (define toggle-countdown
      (spawn ^loopdown 15))
    (define (mixin base)
      (methods
       #:extends base
       [(tick)
        ;; time to toggle
        (when ($ toggle-countdown 'zero?)
          ($ on? (not ($ on?))))

        ($ toggle-countdown 'sub1)

        (base 'tick)]
       [(render)
        ($ text-buffer 'render ($ on?))]))
    mixin)

  (define (next-line-handler lines)
    (match lines
      ['() hang-until-quit]
      [(cons next-line rest-lines)
       (cond
         [(regexp-match #rx"^> " next-line)
          (read-prompt-line next-line rest-lines)]
         [(regexp-match #rx"^%" next-line)
          (brief-pause rest-lines)]
         [else
          (read-normal-line next-line rest-lines)])]))

  (define (read-prompt-line line rest-lines)
    (define blinker-mixin
      (make-blinker-mixin))
    (define line-chars
      (string->list (substring line 2)))
    ;; insert the prompt and a space
    ($ text-buffer 'insert-char #\> 'bryellow)
    ($ text-buffer 'insert-char #\space 'brwhite)

    (define (pause-before-typing)
      (define countdown
        (spawn ^countdown (* 30 3)))
      (blinker-mixin
       (methods
        #:extends base-methods
        [(tick)
         ($ countdown 'sub1)
         (when ($ countdown 'zero?)
           (bcom (type-it)))])))

    (define (type-it)
      (define (next [chars line-chars])
        (define type-pause
          ;; some variance in how fast we type for realism
          (spawn ^countdown (random 2 10)))

        (blinker-mixin
         (methods
          #:extends base-methods
          [(tick)
           ($ type-pause 'sub1)
           (when ($ type-pause 'zero?)
             (match chars
               ;; we're done, move to next line
               ['()
                ($ text-buffer 'crlf)
                (bcom (next-line-handler rest-lines))]
               ;; otherwise add this character
               [(cons this-char rest-chars)
                ($ text-buffer 'insert-char this-char 'brwhite)
                (bcom (next rest-chars))]))])))
      (next))

    (pause-before-typing))

  ;; code duplication but I am tired
  (define (read-normal-line line rest-lines)
    (define line-chars
      (string->list line))

    #;(define type-pause
      (spawn ^loopdown 0))

    (define (next [chars line-chars])
      (methods
       #:extends base-methods
       [(tick)
        ;; ($ type-pause 'sub1)
        ;; (when ($ type-pause 'zero?))
        (match chars
          ;; we're done, move to next line
          ['()
           ($ text-buffer 'crlf)
           (bcom (next-line-handler rest-lines))]
          ;; otherwise add this character
          [(cons this-char rest-chars)
           ($ text-buffer 'insert-char this-char 'brgreen)
           (bcom (next rest-chars))])]))
    (next))

  (define (brief-pause rest-lines)
    (define type-pause
      (spawn ^loopdown 20))
    (methods
     #:extends base-methods
     [(tick)
      ($ type-pause 'sub1)
      (when ($ type-pause 'zero?)
        ($ text-buffer 'crlf)
        (bcom (next-line-handler rest-lines)))]))

  (next-line-handler (credits-lines)))
