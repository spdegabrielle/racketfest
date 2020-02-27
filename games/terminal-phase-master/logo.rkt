#lang at-exp racket/base

(provide logo-lines logo-raart)

(require racket/string
         (prefix-in raart: raart))

(define (text-lines . strs)
  (string-split (string-join strs "") "\n"))


(define logo-lines
  @text-lines{
  ______ _____ ______  _  _    _____ _   __ __    __
 /_  __// ___//  >  / / |/ |  /_ __// | / //  |  / /
  / /  / /_  /  .--' /     |   //  /  |/ // < | / /
 / /  / __/ / /\ \  / /|/| | _//_ / /|  // _  |/ /_
/_/  /____//_/  \_\/_/   |_|/___//_/ |_//_/ |_/___/

            ****  *  *  ****  ****  ****
           *  *  *  *  *  *  **    ***
          ****  ****  *****    ** *
         *     *  *  *   *  **** ****})

(define logo-raart
  (raart:vappend*
   #:halign 'left
   (map raart:text logo-lines)))
