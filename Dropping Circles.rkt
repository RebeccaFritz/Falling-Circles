;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Dropping Circles|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Rebecca Fritz
;draw any number of balls up to 5

;define worldstate
(define-struct Ball [x y radius speed color])
(define-struct WS [lob score misses])

;define global variables CAPITAL BY CONVENTION
(define BOARDWIDTH 750)
(define BOARDHEIGHT 400)
(define BOARDCOLOR 'yellow)
(define FAILCOLOR 'grey)
(define SCORESIZE 20)
(define SCOREX 100)
(define SCOREY 20)
(define FAILSCORESIZE 40)
(define FAILSCOREX (/ BOARDWIDTH 2))
(define FAILSCOREY (/ BOARDHEIGHT 2))

;define initial WS
(define initial-lob (cons (make-Ball (- BOARDWIDTH 50) 60 30 2 "blue") empty))
(define initial-ws (make-WS initial-lob 0 0))

;Worldstate -> Image
;write a render function to draw everything
(define (render ws)
  (cond [(= (WS-misses ws) 10) (draw-fail ws (rectangle BOARDWIDTH BOARDHEIGHT "solid" FAILCOLOR))]
        [else (draw-score ws (draw-ball ws (rectangle BOARDWIDTH BOARDHEIGHT "solid" BOARDCOLOR)))]))

;World-state, Image -> Image
;Draws the ball (if any) in WorldState ws atop the image img.
(define (draw-ball ws img)
  (cond [(empty? (WS-lob ws)) img]
        [(= (length (WS-lob ws)) 1)
         (place-image (circle (Ball-radius (first (WS-lob ws))) "solid" (Ball-color (first (WS-lob ws))))
                      (Ball-x (first (WS-lob ws)))
                      (Ball-y (first (WS-lob ws)))
                      img)]))

;World-state, Image -> Image
;draws the score from the Worldstate ws atop the Image img
(define (draw-score ws img)
  (place-image (text (string-append "Score: " (number->string (WS-score ws)) "    Lives: " (number->string (- 10 (WS-misses ws)))) SCORESIZE 'black)
                                      SCOREX
                                      SCOREY
                                      img))

;World-state, Image -> Image
;draws the fail screen with the final score
(define (draw-fail ws img) (place-image (above (text "Game Over" FAILSCORESIZE 'black) (text (string-append "Score: " (number->string (WS-score ws))) FAILSCORESIZE 'black))
                                        FAILSCOREX
                                        FAILSCOREY
                                        img))

;write the tock function
;Worldstate -> Worldstate
;Moves the balls down the screen
(define (tock ws)
  (cond [(ballmiss? ws) (make-WS (cons (make-Ball (random BOARDWIDTH) 60 (+ 10 (random 35)) (+ (random 10) 1) (randomcolor (random 7))) empty)
                                 (WS-score ws)
                                 (+ 1 (WS-misses ws)))]
        [else (make-WS (cons (make-Ball (Ball-x (first (WS-lob ws)))
                                        (+ (Ball-y (first (WS-lob ws)))
                                           (Ball-speed (first (WS-lob ws))))
                                        (Ball-radius (first (WS-lob ws)))
                                        (Ball-speed (first (WS-lob ws)))
                                        (Ball-color (first (WS-lob ws))))
                             empty)
                       (WS-score ws)
                       (WS-misses ws))]))

;Check if the ball fell off the screen 
;Worldstate -> bollean
(define (ballmiss? ws) (cond [(> (Ball-y (first (WS-lob ws))) (+ BOARDHEIGHT (Ball-radius (first (WS-lob ws)))))
                              #true]
                             [else #false]))

;Produce a random color
;Number -> string
(define (randomcolor num) (cond [(= num 0) "red"]
                                [(= num 1) "orange"]
                                [(= num 2) "gold"]
                                [(= num 3) "green"]
                                [(= num 4) "blue"]
                                [(= num 5) "indigo"]
                                [else "violet"]))

; write the mouse handler
; Worldstate, x, y, mouse-event -> Worldstate
; When players clicks a ball
;   award points
;   remove clicked ball
; Else return the current WS
(define (click ws x y evt) (cond [(string=? "button-up" evt)
                                  (cond [(mouse-over-ball ws x y)
                                         (make-WS (cons (make-Ball (random BOARDWIDTH) 60 (+ 10 (random 35)) (+ (random 10) 1) (randomcolor (random 7))) empty)
                                                  (+ (WS-score ws) 1)
                                                  (WS-misses ws))]
                                        [else ws])]
                                 [else ws]))

;Write a function that checks if the mouse is over the ball
;Worldstate -> boolean
(define (mouse-over-ball ws x y) (cond [(> (Ball-radius (first (WS-lob ws)))
                                          (sqrt (+ (expt (- (Ball-x (first (WS-lob ws))) x) 2) (expt (- (Ball-y (first (WS-lob ws))) y) 2))))
                                        #true]
                                       [else #false]))

;add big-bang
;declare a tock, mouse-handler, and render
(big-bang initial-ws
  (to-draw render)
  (on-tick tock)
  (on-mouse click); every tick it checks for something happening in the mouse
  )



