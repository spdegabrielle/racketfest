#lang slideshow
(require 
  (prefix-in image: 2htdp/image)
  slideshow/text
  mred
  slideshow/step
  slideshow/code
  pict/balloon
  pict/face
  )          

(define fish (standard-fish 200 100 #:direction 'right))

(slide
 (t "All about the 2019 Racket Gamejam ")
 (t "& Standard Fish Competition")
 )

(slide
 #:title "1stRacket Gamejam "
 (scale (bitmap  "gamejam/gamejam.png") 2.7)
 (t "November 2019")
 )

(slide #:title "Best package:"
       (t "Racket code generator for Vulkan by Sage Gerard")
       (scale (bitmap  "gamejam/racket-vulkan.png") 0.4)
       (tt "https://zyrolasting.itch.io/racket-vulkan")
       (t "package that generates Racket bindings for Vulkan")
       (t "not just games; simulations, visualisation, research applications.")
       (t "available on GitHub along with detailed documentation.")
       )

(slide #:title "Best Retrogame(joint):"
       (t "Terminal Phase by Christopher Lemmer Webber")
       (scale (bitmap  "gamejam/terminal.gif") 0.8)
       (tt "https://cwebber.itch.io/terminal-phase")
       )

(slide #:title "Best Retrogame(joint):"
       (t "C64 robotfindskitten by Ross McKinlay")
       (scale (bitmap  "gamejam/robot.png") 0.4)
       (tt "https://pezi-pink.itch.io/c64-robotfindskitten")
       (tt "https://docs.racket-lang.org/asi64/index.html")
       )

(slide #:title "Best Boardgame:"
       (t "Racket-Onitama by Dustin Wagner")
       (scale (bitmap  "gamejam/onitama.png") 0.8)
       (tt "https://dcsw.itch.io/onitama")
       (t "A great Racket implementation of the popular Onitama boardgame."))

(slide #:title "Best Puzzle Game"
       (t "Groovee by tjm25225")
       (scale (bitmap  "gamejam/groovee.png") 0.6)
       (tt "https://tjm25225.itch.io/groovee")
       )

(slide #:title "Honorable Mention"
       (t "RacketTown by Hendrik Boom")
       (tt "https://github.com/hendrikboom3/rackettown")
       (t "An Urban landscape generation library!")
       )

(slide
 (t "Pause for glass of water"))


(slide
 (t "The Pictures"))


(slide
 (scale (bitmap  "images/racket-lightsaber.png") 0.5)
 (blank)
 (t "lightsaber by Justin Zamora ")) 

(slide
 (scale (bitmap  "images/quilt.png") 0.8)
 (blank)
 (t "quilt by Daniel Prager"))

(slide
 (scale (bitmap  "images/turnstile.png") 0.5)
 (blank)
 (t "turnstile from metapict-examples by Jens Axel Søgaard"))

(slide
 (bitmap  "images/lightbulb.png")
 (blank)
 (t "standard lightbulb by Philip McGrath"))

(slide
 (scale (bitmap  "images/world.png") 1.0)
 (t "world-map.rkt by Alex Harsányi ")
 (t "https://gist.github.com/alex-hhh/2c0f5a02d9e795cbedf90cf84ef84281"))

(slide
 (scale (bitmap  "images/baseball-cap.png") 0.3)
 (t "baseball-cap by Justin Zamora "))

(slide
 (scale (bitmap  "images/logo-plot.png") 1.0)
 (blank)
 (t "racket-logo-plot.rkt by Laurent Orseau"))


(slide
 (t "square")
 (blank)
 (scale (bitmap  "images/square.png") 0.5)
 (blank)
 (t "metapict-examples by Jens Axel Søgaard "))


(slide
 (t "Tessellation:")
 (t "A library for aiding in the creation of tessellated geometric patterns")
 (blank)
 (scale (bitmap  "images/example1.png") 0.5)
 (blank)
 (t "by Zachary Romero")
 (tt "https://github.com/zkry/tessellation"))


(slide
 (t "Pixel Fish")
 (blank)
 (scale (bitmap  "images/pixel-fish.png") 0.7)
 (blank)
 (t " by Sam Phillips https://github.com/samdphillips/pixel-fish"))

(slide
 (t "Face")
 (blank)
 (bitmap  "images/face.png")
 (blank)
 (t "face.rkt by Bert (plot)"))

(slide
 (t "waffle-racket")
 (scale (bitmap  "images/waffle.png") 2.5)

 (scale (codeblock-pict
         #:keep-lang-line? #f
         (string-join
          '("#lang slideshow"
            "(require 2htdp/image)"
            "(define (waffle img)"
            "(define two-p (hc-append img (rectangle 10 10 \"solid\" \"PaleGoldenrod\") img (rectangle 10 10 \"solid\" \"PaleGoldenrod\") img))"
            "(vc-append two-p (rectangle 10 10 \"solid\" \"PaleGoldenrod\") two-p (rectangle 10 10 \"solid\" \"PaleGoldenrod\") two-p))"
            "(cc-superimpose"
            "(filled-rounded-rectangle 65 65 #:color \"PaleGoldenrod\" #:border-color \"PaleGoldenrod\")"
            "(waffle (rectangle 10 10 \"solid\" \"Gold\")))"
            )
          "\n")) 0.4)

 
 (t "Connie, age 11")
 )


(slide 
 (t "Community choice winners!")
 (blank)
 (t "1st: tessellation by Zachary Romero https://github.com/zkry/tessellation")
 (t "2nd: lightsaber by Justin Zamora https://github.com/standard-fish/lightsaber")
 (t "Best Entry with Butter and Local Maine Maple Syrup:")
 (t "waffle-racket by Connie https://github.com/conniepocky/waffle-racket")
 (blank)
 (t "Thank you to all who participated.")
 )

(slide
 (t "thats not all..."))

(slide
 (t "a late entry")
 (scale (bitmap  "images/racket-town.png") 0.3)
 (t "an early preview of some procedural generation")
 (t "by Hendrik Boom ")
 )


(slide
 (t "Rhombus")
 (blank)
 (bitmap (image:freeze (image:rotate 65 (image:rhombus 200 300 "solid" "mediumpurple"))))
 )


(slide
 (code (standard-fish 200 100 #:direction 'right))
 ;(blank)
 (blank)
 (scale (pin-balloon (wrap-balloon (with-size 32 (text "Hello!")) 'sw 15 15)
                     (cc-superimpose (blank 200 100) fish)
                     fish
                     rt-find) 1.5))


(slide
 (scale (bitmap  "images/lightbulb.png") 0.7)
 (face* 'normal 'huge #f default-face-color 0 -3)
 (blank)
 (t "How about a picture competition?")
 )

(slide 

 (blank)
 (t "I'll see you this afternoon for 'Play time: Racket games!'")
 (blank)
 (t "danke")
 )
       

