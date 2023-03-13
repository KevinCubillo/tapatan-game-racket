#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define (draw-board board)
  (let ((img (rectangle 720 720 'outline (make-color 200 200 200))))
    (set! img
          (overlay img
                    (cond [(equal? (list-ref board 0) "X")                          (place-image (circle 40 "solid" "blue") 120 120 img)]
                         [(equal? (list-ref board 0) "O")                          (place-image (circle 40 "solid" "red") 120 120 img)]
                         [else (place-image (text "-" 40 "black") 120 120 img)])
                   
                   
                    (cond [(equal? (list-ref board 1) "X")                          (place-image (circle 40 "solid" "blue") 240 120 img)]
                         [(equal? (list-ref board 1) "O")                          (place-image (circle 40 "solid" "red") 240 120 img)]
                         [else (place-image (text "-" 40 "black") 240 120 img)])
                   
                   
                    (cond [(equal? (list-ref board 2) "X")                          (place-image (circle 40 "solid" "blue") 360 120 img)]
                         [(equal? (list-ref board 2) "O")                          (place-image (circle 40 "solid" "red") 360 120 img)]
                         [else (place-image (text "-" 40 "black") 360 120 img)])
                   
                   
                    (cond [(equal? (list-ref board 3) "X")                          (place-image (circle 40 "solid" "blue") 120 240 img)]
                         [(equal? (list-ref board 3) "O")                          (place-image (circle 40 "solid" "red") 120 240 img)]
                         [else (place-image (text "-" 40 "black") 120 240 img)])
                    
                    (cond [(equal? (list-ref board 4) "X")                          (place-image (circle 40 "solid" "blue") 240 240 img)]
                          [(equal? (list-ref board 4) "O")                          (place-image (circle 40 "solid" "red") 240 240 img)]
                          [else (place-image (text "-" 40 "black") 240 240 img)])
                    
                    (cond [(equal? (list-ref board 5) "X")                          (place-image (circle 40 "solid" "blue") 360 240 img)]
                          [(equal? (list-ref board 5) "O")                          (place-image (circle 40 "solid" "red") 360 240 img)]
                          [else (place-image (text "-" 40 "black") 360 240 img)])
                    
                    (cond [(equal? (list-ref board 6) "X")                          (place-image (circle 40 "solid" "blue") 120 360 img)]
                          [(equal? (list-ref board 6) "O")                          (place-image (circle 40 "solid" "red") 120 360 img)]
                          [else (place-image (text "-" 40 "black") 120 360 img)])

                    (cond [(equal? (list-ref board 7) "X")                          (place-image (circle 40 "solid" "blue") 240 360 img)]
                          [(equal? (list-ref board 7) "O")                          (place-image (circle 40 "solid" "red") 240 360 img)]
                          [else (place-image (text "-" 40 "black") 240 360 img)])

                    (cond [(equal? (list-ref board 8) "X")                          (place-image (circle 40 "solid" "blue") 360 360 img)]
                          [(equal? (list-ref board 8) "O")                          (place-image (circle 40 "solid" "red") 360 360 img)]
                          [else (place-image (text "-" 40 "black") 360 360 img)])))
                   


    img))


 ; Faltan los otros 6 condicionales para completar el tablero
 ;El tablero solo se dibuja una vez, no se actualiza
 ;El problema es que no se puede modificar el tablero debido a que se envia un empty-scene en cada condicional

(define initial-board '("X" "O" "X"
                         "O" "X" "O"
                         "X" "O" "X"))

(big-bang initial-board
  (to-draw draw-board))