#lang racket/base

(provide braille-configurations
         brailleify-text
         brailleify-text/list
         brailleify-grid/list)

(require racket/string
         racket/set)

(define braille-configurations
  #hash((#(#f #f
           #f #f
           #f #f
           #f #f) . #\⠀)
        (#(SS #f
           #f #f
           #f #f
           #f #f) . #\⠁)
        (#(SS #f
           SS #f
           #f #f
           #f #f) . #\⠃)
        (#(SS #f
           SS #f
           SS #f
           #f #f) . #\⠇)
        (#(SS SS
           SS #f
           SS #f
           #f #f) . #\⠏)
        (#(SS SS
           SS SS
           SS #f
           #f #f) . #\⠟)
        (#(SS SS
           SS SS
           SS SS
           #f #f) . #\⠿)
        (#(SS SS
           SS #f
           SS SS
           #f #f) . #\⠯)
        (#(SS #f
           SS SS
           SS #f
           #f #f) . #\⠗)
        (#(SS #f
           SS SS
           SS SS
           #f #f) . #\⠷)
        (#(SS #f
           SS #f
           SS SS
           #f #f) . #\⠧)
        (#(SS SS
           SS #f
           #f #f
           #f #f) . #\⠋)
        (#(SS SS
           SS SS
           #f #f
           #f #f) . #\⠛)
        (#(SS SS
           SS SS
           #f SS
           #f #f) . #\⠻)
        (#(SS SS
           SS #f
           #f SS
           #f #f) . #\⠫)
        (#(SS #f
           SS SS
           #f #f
           #f #f) . #\⠓)
        (#(SS #f
           SS SS
           #f SS
           #f #f) . #\⠳)
        (#(SS #f
           SS #f
           #f SS
           #f #f) . #\⠣)
        (#(SS #f
           #f #f
           SS #f
           #f #f) . #\⠅)
        (#(SS SS
           #f #f
           SS #f
           #f #f) . #\⠍)
        (#(SS SS
           #f SS
           SS #f
           #f #f) . #\⠝)
        (#(SS SS
           #f SS
           SS SS
           #f #f) . #\⠽)
        (#(SS SS
           #f #f
           SS SS
           #f #f) . #\⠭)
        (#(SS #f
           #f SS
           SS #f
           #f #f) . #\⠕)
        (#(SS #f
           #f SS
           SS SS
           #f #f) . #\⠵)
        (#(SS #f
           #f #f
           SS SS
           #f #f) . #\⠥)
        (#(SS SS
           #f #f
           #f #f
           #f #f) . #\⠉)
        (#(SS SS
           #f SS
           #f #f
           #f #f) . #\⠙)
        (#(SS SS
           #f SS
           #f SS
           #f #f) . #\⠹)
        (#(SS SS
           #f #f
           #f SS
           #f #f) . #\⠩)
        (#(SS #f
           #f SS
           #f #f
           #f #f) . #\⠑)
        (#(SS #f
           #f SS
           #f SS
           #f #f) . #\⠱)
        (#(SS #f
           #f #f
           #f SS
           #f #f) . #\⠡)
        (#(#f #f
           SS #f
           #f #f
           #f #f) . #\⠂)
        (#(#f #f
           SS #f
           SS #f
           #f #f) . #\⠆)
        (#(#f SS
           SS #f
           SS #f
           #f #f) . #\⠎)
        (#(#f SS
           SS SS
           SS #f
           #f #f) . #\⠞)
        (#(#f SS
           SS SS
           SS SS
           #f #f) . #\⠾)
        (#(#f SS
           SS #f
           SS SS
           #f #f) . #\⠮)
        (#(#f #f
           SS SS
           SS #f
           #f #f) . #\⠖)
        (#(#f #f
           SS SS
           SS SS
           #f #f) . #\⠶)
        (#(#f #f
           SS #f
           SS SS
           #f #f) . #\⠦)
        (#(#f SS
           SS #f
           #f #f
           #f #f) . #\⠊)
        (#(#f SS
           SS SS
           #f #f
           #f #f) . #\⠚)
        (#(#f SS
           SS SS
           #f SS
           #f #f) . #\⠺)
        (#(#f SS
           SS #f
           #f SS
           #f #f) . #\⠪)
        (#(#f #f
           SS SS
           #f #f
           #f #f) . #\⠒)
        (#(#f #f
           SS SS
           #f SS
           #f #f) . #\⠲)
        (#(#f #f
           SS #f
           #f SS
           #f #f) . #\⠢)
        (#(#f #f
           #f #f
           SS #f
           #f #f) . #\⠄)
        (#(#f SS
           #f #f
           SS #f
           #f #f) . #\⠌)
        (#(#f SS
           #f SS
           SS #f
           #f #f) . #\⠜)
        (#(#f SS
           #f SS
           SS SS
           #f #f) . #\⠼)
        (#(#f SS
           #f #f
           SS SS
           #f #f) . #\⠬)
        (#(#f #f
           #f SS
           SS #f
           #f #f) . #\⠔)
        (#(#f #f
           #f SS
           SS SS
           #f #f) . #\⠴)
        (#(#f #f
           #f #f
           SS SS
           #f #f) . #\⠤)
        (#(#f SS
           #f #f
           #f #f
           #f #f) . #\⠈)
        (#(#f SS
           #f SS
           #f #f
           #f #f) . #\⠘)
        (#(#f SS
           #f SS
           #f SS
           #f #f) . #\⠸)
        (#(#f SS
           #f #f
           #f SS
           #f #f) . #\⠨)
        (#(#f #f
           #f SS
           #f #f
           #f #f) . #\⠐)
        (#(#f #f
           #f SS
           #f SS
           #f #f) . #\⠰)
        (#(#f #f
           #f #f
           #f SS
           #f #f) . #\⠠)
        (#(#f #f
           #f #f
           #f #f
           SS #f) . #\⡀)
        (#(SS #f
           #f #f
           #f #f
           SS #f) . #\⡁)
        (#(#f #f
           SS #f
           #f #f
           SS #f) . #\⡂)
        (#(SS #f
           SS #f
           #f #f
           SS #f) . #\⡃)
        (#(#f #f
           #f #f
           SS #f
           SS #f) . #\⡄)
        (#(SS #f
           #f #f
           SS #f
           SS #f) . #\⡅)
        (#(#f #f
           SS #f
           SS #f
           SS #f) . #\⡆)
        (#(SS #f
           SS #f
           SS #f
           SS #f) . #\⡇)
        (#(#f SS
           #f #f
           #f #f
           SS #f) . #\⡈)
        (#(SS SS
           #f #f
           #f #f
           SS #f) . #\⡉)
        (#(#f SS
           SS #f
           #f #f
           SS #f) . #\⡊)
        (#(SS SS
           SS #f
           #f #f
           SS #f) . #\⡋)
        (#(#f SS
           #f #f
           SS #f
           SS #f) . #\⡌)
        (#(SS SS
           #f #f
           SS #f
           SS #f) . #\⡍)
        (#(#f SS
           SS #f
           SS #f
           SS #f) . #\⡎)
        (#(SS SS
           SS #f
           SS #f
           SS #f) . #\⡏)
        (#(#f #f
           #f SS
           #f #f
           SS #f) . #\⡐)
        (#(SS #f
           #f SS
           #f #f
           SS #f) . #\⡑)
        (#(#f #f
           SS SS
           #f #f
           SS #f) . #\⡒)
        (#(SS #f
           SS SS
           #f #f
           SS #f) . #\⡓)
        (#(#f #f
           #f SS
           SS #f
           SS #f) . #\⡔)
        (#(SS #f
           #f SS
           SS #f
           SS #f) . #\⡕)
        (#(#f #f
           SS SS
           SS #f
           SS #f) . #\⡖)
        (#(SS #f
           SS SS
           SS #f
           SS #f) . #\⡗)
        (#(#f SS
           #f SS
           #f #f
           SS #f) . #\⡘)
        (#(SS SS
           #f SS
           #f #f
           SS #f) . #\⡙)
        (#(#f SS
           SS SS
           #f #f
           SS #f) . #\⡚)
        (#(SS SS
           SS SS
           #f #f
           SS #f) . #\⡛)
        (#(#f SS
           #f SS
           SS #f
           SS #f) . #\⡜)
        (#(SS SS
           #f SS
           SS #f
           SS #f) . #\⡝)
        (#(#f SS
           SS SS
           SS #f
           SS #f) . #\⡞)
        (#(SS SS
           SS SS
           SS #f
           SS #f) . #\⡟)
        (#(#f #f
           #f #f
           #f SS
           SS #f) . #\⡠)
        (#(SS #f
           #f #f
           #f SS
           SS #f) . #\⡡)
        (#(#f #f
           SS #f
           #f SS
           SS #f) . #\⡢)
        (#(SS #f
           SS #f
           #f SS
           SS #f) . #\⡣)
        (#(#f #f
           #f #f
           SS SS
           SS #f) . #\⡤)
        (#(SS #f
           #f #f
           SS SS
           SS #f) . #\⡥)
        (#(#f #f
           SS #f
           SS SS
           SS #f) . #\⡦)
        (#(SS #f
           SS #f
           SS SS
           SS #f) . #\⡧)
        (#(#f SS
           #f #f
           #f SS
           SS #f) . #\⡨)
        (#(SS SS
           #f #f
           #f SS
           SS #f) . #\⡩)
        (#(#f SS
           SS #f
           #f SS
           SS #f) . #\⡪)
        (#(SS SS
           SS #f
           #f SS
           SS #f) . #\⡫)
        (#(#f SS
           #f #f
           SS SS
           SS #f) . #\⡬)
        (#(SS SS
           #f #f
           SS SS
           SS #f) . #\⡭)
        (#(#f SS
           SS #f
           SS SS
           SS #f) . #\⡮)
        (#(SS SS
           SS #f
           SS SS
           SS #f) . #\⡯)
        (#(#f #f
           #f SS
           #f SS
           SS #f) . #\⡰)
        (#(SS #f
           #f SS
           #f SS
           SS #f) . #\⡱)
        (#(#f #f
           SS SS
           #f SS
           SS #f) . #\⡲)
        (#(SS #f
           SS SS
           #f SS
           SS #f) . #\⡳)
        (#(#f #f
           #f SS
           SS SS
           SS #f) . #\⡴)
        (#(SS #f
           #f SS
           SS SS
           SS #f) . #\⡵)
        (#(#f #f
           SS SS
           SS SS
           SS #f) . #\⡶)
        (#(SS #f
           SS SS
           SS SS
           SS #f) . #\⡷)
        (#(#f SS
           #f SS
           #f SS
           SS #f) . #\⡸)
        (#(SS SS
           #f SS
           #f SS
           SS #f) . #\⡹)
        (#(#f SS
           SS SS
           #f SS
           SS #f) . #\⡺)
        (#(SS SS
           SS SS
           #f SS
           SS #f) . #\⡻)
        (#(#f SS
           #f SS
           SS SS
           SS #f) . #\⡼)
        (#(SS SS
           #f SS
           SS SS
           SS #f) . #\⡽)
        (#(#f SS
           SS SS
           SS SS
           SS #f) . #\⡾)
        (#(SS SS
           SS SS
           SS SS
           SS #f) . #\⡿)
        (#(#f #f
           #f #f
           #f #f
           #f SS) . #\⢀)
        (#(SS #f
           #f #f
           #f #f
           #f SS) . #\⢁)
        (#(#f #f
           SS #f
           #f #f
           #f SS) . #\⢂)
        (#(SS #f
           SS #f
           #f #f
           #f SS) . #\⢃)
        (#(#f #f
           #f #f
           SS #f
           #f SS) . #\⢄)
        (#(SS #f
           #f #f
           SS #f
           #f SS) . #\⢅)
        (#(#f #f
           SS #f
           SS #f
           #f SS) . #\⢆)
        (#(SS #f
           SS #f
           SS #f
           #f SS) . #\⢇)
        (#(#f SS
           #f #f
           #f #f
           #f SS) . #\⢈)
        (#(SS SS
           #f #f
           #f #f
           #f SS) . #\⢉)
        (#(#f SS
           SS #f
           #f #f
           #f SS) . #\⢊)
        (#(SS SS
           SS #f
           #f #f
           #f SS) . #\⢋)
        (#(#f SS
           #f #f
           SS #f
           #f SS) . #\⢌)
        (#(SS SS
           #f #f
           SS #f
           #f SS) . #\⢍)
        (#(#f SS
           SS #f
           SS #f
           #f SS) . #\⢎)
        (#(SS SS
           SS #f
           SS #f
           #f SS) . #\⢏)
        (#(#f #f
           #f SS
           #f #f
           #f SS) . #\⢐)
        (#(SS #f
           #f SS
           #f #f
           #f SS) . #\⢑)
        (#(#f #f
           SS SS
           #f #f
           #f SS) . #\⢒)
        (#(SS #f
           SS SS
           #f #f
           #f SS) . #\⢓)
        (#(#f #f
           #f SS
           SS #f
           #f SS) . #\⢔)
        (#(SS #f
           #f SS
           SS #f
           #f SS) . #\⢕)
        (#(#f #f
           SS SS
           SS #f
           #f SS) . #\⢖)
        (#(SS #f
           SS SS
           SS #f
           #f SS) . #\⢗)
        (#(#f SS
           #f SS
           #f #f
           #f SS) . #\⢘)
        (#(SS SS
           #f SS
           #f #f
           #f SS) . #\⢙)
        (#(#f SS
           SS SS
           #f #f
           #f SS) . #\⢚)
        (#(SS SS
           SS SS
           #f #f
           #f SS) . #\⢛)
        (#(#f SS
           #f SS
           SS #f
           #f SS) . #\⢜)
        (#(SS SS
           #f SS
           SS #f
           #f SS) . #\⢝)
        (#(#f SS
           SS SS
           SS #f
           #f SS) . #\⢞)
        (#(SS SS
           SS SS
           SS #f
           #f SS) . #\⢟)
        (#(#f #f
           #f #f
           #f SS
           #f SS) . #\⢠)
        (#(SS #f
           #f #f
           #f SS
           #f SS) . #\⢡)
        (#(#f #f
           SS #f
           #f SS
           #f SS) . #\⢢)
        (#(SS #f
           SS #f
           #f SS
           #f SS) . #\⢣)
        (#(#f #f
           #f #f
           SS SS
           #f SS) . #\⢤)
        (#(SS #f
           #f #f
           SS SS
           #f SS) . #\⢥)
        (#(#f #f
           SS #f
           SS SS
           #f SS) . #\⢦)
        (#(SS #f
           SS #f
           SS SS
           #f SS) . #\⢧)
        (#(#f SS
           #f #f
           #f SS
           #f SS) . #\⢨)
        (#(SS SS
           #f #f
           #f SS
           #f SS) . #\⢩)
        (#(#f SS
           SS #f
           #f SS
           #f SS) . #\⢪)
        (#(SS SS
           SS #f
           #f SS
           #f SS) . #\⢫)
        (#(#f SS
           #f #f
           SS SS
           #f SS) . #\⢬)
        (#(SS SS
           #f #f
           SS SS
           #f SS) . #\⢭)
        (#(#f SS
           SS #f
           SS SS
           #f SS) . #\⢮)
        (#(SS SS
           SS #f
           SS SS
           #f SS) . #\⢯)
        (#(#f #f
           #f SS
           #f SS
           #f SS) . #\⢰)
        (#(SS #f
           #f SS
           #f SS
           #f SS) . #\⢱)
        (#(#f #f
           SS SS
           #f SS
           #f SS) . #\⢲)
        (#(SS #f
           SS SS
           #f SS
           #f SS) . #\⢳)
        (#(#f #f
           #f SS
           SS SS
           #f SS) . #\⢴)
        (#(SS #f
           #f SS
           SS SS
           #f SS) . #\⢵)
        (#(#f #f
           SS SS
           SS SS
           #f SS) . #\⢶)
        (#(SS #f
           SS SS
           SS SS
           #f SS) . #\⢷)
        (#(#f SS
           #f SS
           #f SS
           #f SS) . #\⢸)
        (#(SS SS
           #f SS
           #f SS
           #f SS) . #\⢹)
        (#(#f SS
           SS SS
           #f SS
           #f SS) . #\⢺)
        (#(SS SS
           SS SS
           #f SS
           #f SS) . #\⢻)
        (#(#f SS
           #f SS
           SS SS
           #f SS) . #\⢼)
        (#(SS SS
           #f SS
           SS SS
           #f SS) . #\⢽)
        (#(#f SS
           SS SS
           SS SS
           #f SS) . #\⢾)
        (#(SS SS
           SS SS
           SS SS
           #f SS) . #\⢿)
        (#(#f #f
           #f #f
           #f #f
           SS SS) . #\⣀)
        (#(SS #f
           #f #f
           #f #f
           SS SS) . #\⣁)
        (#(#f #f
           SS #f
           #f #f
           SS SS) . #\⣂)
        (#(SS #f
           SS #f
           #f #f
           SS SS) . #\⣃)
        (#(#f #f
           #f #f
           SS #f
           SS SS) . #\⣄)
        (#(SS #f
           #f #f
           SS #f
           SS SS) . #\⣅)
        (#(#f #f
           SS #f
           SS #f
           SS SS) . #\⣆)
        (#(SS #f
           SS #f
           SS #f
           SS SS) . #\⣇)
        (#(#f SS
           #f #f
           #f #f
           SS SS) . #\⣈)
        (#(SS SS
           #f #f
           #f #f
           SS SS) . #\⣉)
        (#(#f SS
           SS #f
           #f #f
           SS SS) . #\⣊)
        (#(SS SS
           SS #f
           #f #f
           SS SS) . #\⣋)
        (#(#f SS
           #f #f
           SS #f
           SS SS) . #\⣌)
        (#(SS SS
           #f #f
           SS #f
           SS SS) . #\⣍)
        (#(#f SS
           SS #f
           SS #f
           SS SS) . #\⣎)
        (#(SS SS
           SS #f
           SS #f
           SS SS) . #\⣏)
        (#(#f #f
           #f SS
           #f #f
           SS SS) . #\⣐)
        (#(SS #f
           #f SS
           #f #f
           SS SS) . #\⣑)
        (#(#f #f
           SS SS
           #f #f
           SS SS) . #\⣒)
        (#(SS #f
           SS SS
           #f #f
           SS SS) . #\⣓)
        (#(#f #f
           #f SS
           SS #f
           SS SS) . #\⣔)
        (#(SS #f
           #f SS
           SS #f
           SS SS) . #\⣕)
        (#(#f #f
           SS SS
           SS #f
           SS SS) . #\⣖)
        (#(SS #f
           SS SS
           SS #f
           SS SS) . #\⣗)
        (#(#f SS
           #f SS
           #f #f
           SS SS) . #\⣘)
        (#(SS SS
           #f SS
           #f #f
           SS SS) . #\⣙)
        (#(#f SS
           SS SS
           #f #f
           SS SS) . #\⣚)
        (#(SS SS
           SS SS
           #f #f
           SS SS) . #\⣛)
        (#(#f SS
           #f SS
           SS #f
           SS SS) . #\⣜)
        (#(SS SS
           #f SS
           SS #f
           SS SS) . #\⣝)
        (#(#f SS
           SS SS
           SS #f
           SS SS) . #\⣞)
        (#(SS SS
           SS SS
           SS #f
           SS SS) . #\⣟)
        (#(#f #f
           #f #f
           #f SS
           SS SS) . #\⣠)
        (#(SS #f
           #f #f
           #f SS
           SS SS) . #\⣡)
        (#(#f #f
           SS #f
           #f SS
           SS SS) . #\⣢)
        (#(SS #f
           SS #f
           #f SS
           SS SS) . #\⣣)
        (#(#f #f
           #f #f
           SS SS
           SS SS) . #\⣤)
        (#(SS #f
           #f #f
           SS SS
           SS SS) . #\⣥)
        (#(#f #f
           SS #f
           SS SS
           SS SS) . #\⣦)
        (#(SS #f
           SS #f
           SS SS
           SS SS) . #\⣧)
        (#(#f SS
           #f #f
           #f SS
           SS SS) . #\⣨)
        (#(SS SS
           #f #f
           #f SS
           SS SS) . #\⣩)
        (#(#f SS
           SS #f
           #f SS
           SS SS) . #\⣪)
        (#(SS SS
           SS #f
           #f SS
           SS SS) . #\⣫)
        (#(#f SS
           #f #f
           SS SS
           SS SS) . #\⣬)
        (#(SS SS
           #f #f
           SS SS
           SS SS) . #\⣭)
        (#(#f SS
           SS #f
           SS SS
           SS SS) . #\⣮)
        (#(SS SS
           SS #f
           SS SS
           SS SS) . #\⣯)
        (#(#f #f
           #f SS
           #f SS
           SS SS) . #\⣰)
        (#(SS #f
           #f SS
           #f SS
           SS SS) . #\⣱)
        (#(#f #f
           SS SS
           #f SS
           SS SS) . #\⣲)
        (#(SS #f
           SS SS
           #f SS
           SS SS) . #\⣳)
        (#(#f #f
           #f SS
           SS SS
           SS SS) . #\⣴)
        (#(SS #f
           #f SS
           SS SS
           SS SS) . #\⣵)
        (#(#f #f
           SS SS
           SS SS
           SS SS) . #\⣶)
        (#(SS #f
           SS SS
           SS SS
           SS SS) . #\⣷)
        (#(#f SS
           #f SS
           #f SS
           SS SS) . #\⣸)
        (#(SS SS
           #f SS
           #f SS
           SS SS) . #\⣹)
        (#(#f SS
           SS SS
           #f SS
           SS SS) . #\⣺)
        (#(SS SS
           SS SS
           #f SS
           SS SS) . #\⣻)
        (#(#f SS
           #f SS
           SS SS
           SS SS) . #\⣼)
        (#(SS SS
           #f SS
           SS SS
           SS SS) . #\⣽)
        (#(#f SS
           SS SS
           SS SS
           SS SS) . #\⣾)
        (#(SS SS
           SS SS
           SS SS
           SS SS) . #\⣿)))

(module+ test
  (require rackunit)

  (define (generate-permutations len)
    (define all-permutations
      (let lp ([len len])
        (if (= len 1)
            '((SS) (#f))
            (foldl
             (lambda (item prev)
               (cons (cons 'SS item)
                     (cons (cons #f item)
                           prev)))
             '()
             (lp (sub1 len))))))
    (map list->vector all-permutations))

  (define all-permutations
    (list->set (generate-permutations 8)))

  (define missing-permutations
    (set-subtract all-permutations
                  (list->set (hash-keys braille-configurations))))

  (test-equal?
   "No missing permutations"
   missing-permutations (set)))

(define (brailleify-grid/list grid)
  (define grid-height
    (vector-length grid))
  (define grid-width
    (vector-length (vector-ref grid 0)))

  (define (grid-ref x y)
    (if (>= y grid-height)
        #f
        (if (>= x grid-width)
            #f
            (and (vector-ref (vector-ref grid y) x) 'SS))))
  (define (configuration-at x y)
    (hash-ref braille-configurations
              (vector (grid-ref x y) (grid-ref (add1 x) y)
                      (grid-ref x (+ y 1)) (grid-ref (add1 x) (+ y 1))
                      (grid-ref x (+ y 2)) (grid-ref (add1 x) (+ y 2))
                      (grid-ref x (+ y 3)) (grid-ref (add1 x) (+ y 3)))))
  (for/list ([top-y (in-range 0 grid-height 4)])
    (for/fold ([result-lst '()]
               #:result (list->string (reverse result-lst)))
              ([left-x (in-range 0 grid-width 2)])
      (cons (configuration-at left-x top-y) result-lst))))

;; Split text into a grid of vectors of vectors.
(define (vectorize-text text)
  (define split-text
    (string-split text "\n"))
  (define longest-line
    (apply max (map string-length split-text)))
  (for/vector ([line split-text])
    (define line-vec
      (make-vector longest-line #f))
    (for ([char line]
          [i (in-naturals)])
      (unless (eq? char #\space)
        (vector-set! line-vec i 'SS)))
    line-vec))

(define (brailleify-text/list text)
  (define vectext
    (vectorize-text text))
  (brailleify-grid/list vectext))

(define (brailleify-text text)
  (string-join (brailleify-text/list text) "\n"))


(module+ main
  (define everything-is-fine-periods
    "\
 .... .   . .... ...  .   .
 .    .   . .    .  . .   .
 ...  .   . ...  ...   ...
 .     . .  .    .  .   .
 ....   .   .... .  .   .

  ..... .  . . .   .  ...
    .   .  . . ..  . .
    .   .... . . . . . ...
    .   .  . . .  .. .   .
    .   .  . . .   .  ...

            ......
          .........
        .....    ....
       ...         ...
      ..   ..   ..   ..
      ..   ..   ..   ..
      ..             ..
      .. .         . ..
      ..  ...   ...  ..
       ...   ...   ...
        .....   .....
          .........
            .....

           .   ...
           .  .
           .   ...
           .      .
           .  ....

      .... . .   . ....
      .    . ..  . .
      ...  . . . . ...
      .    . .  .. .
      .    . .   . ....")

  (display (brailleify-text everything-is-fine-periods))
  (newline))

