#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define (draw-board board)
  (overlay (rectangle 720 720 'outline (make-color 200 200 200))

           (cond [(equal? (list-ref board 0) "X")
                    (place-image (circle 80 "solid" "blue")
                                 120 120 (empty-scene 720 720))]

                   [(equal? (list-ref board 0) "O")
                    (place-image (circle 80 "solid" "red")
                                 120 120 (empty-scene 720 720))]

                   [else (place-image (text "-" 80 "black")
                                      120 120 (empty-scene 720 720))])

           (cond [(equal? (list-ref board 1) "X")
                    (place-image (circle 80 "solid" "blue")
                                 240 120 (empty-scene 720 720))]

                   [(equal? (list-ref board 1) "O")
                    (place-image (circle 80 "solid" "red")
                                 240 120 (empty-scene 720 720))]

                   [else (place-image (text "-" 80 "black")
                                      240 120 (empty-scene 720 720))])

           (cond [(equal? (list-ref board 2) "X")
                    (place-image (circle 80 "solid" "blue")
                                 360 120 (empty-scene 720 720))]

                   [(equal? (list-ref board 2) "O")
                    (place-image (circle 80 "solid" "red")
                                 360 120 (empty-scene 720 720))]

                   [else (place-image (text "-" 80 "black")
                                      360 120 (empty-scene 720 720))])

           (cond [(equal? (list-ref board 3) "X")
                    (place-image (circle 80 "solid" "blue")
                                 120 240 (empty-scene 720 720))]

                   [(equal? (list-ref board 3) "O")
                    (place-image (circle 80 "solid" "red")
                                 120 240 (empty-scene 720 720))]

                   [else (place-image (text "-" 80 "black")
                                      120 240 (empty-scene 720 720))])))
 

 ; Faltan los otros 6 condicionales para completar el tablero
 ;El tablero solo se dibuja una vez, no se actualiza
 ;El problema es que no se puede modificar el tablero debido a que se envia un empty-scene en cada condicional

(define initial-board '("O" "X" "-"
                         "-" "O" "-"
                         "-" "-" "X"))

(big-bang initial-board
  (to-draw draw-board))