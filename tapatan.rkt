#lang racket

(require 2htdp/image)
(require 2htdp/universe)



(define (draw-board board)
  (let ((img (rectangle 720 720 'outline (make-color 200 200 200))))
    (set! img
          (overlay img

                    (cond [(equal? (list-ref board 0) "X")                         (place-image (circle 30 "solid" "blue") 180 60 img)]
                         [(equal? (list-ref board 0) "O")                          (place-image (circle 30 "solid" "red") 180 60 img)]
                         [else (place-image (text "-" 30 "black") 180 60 img)])
                   
                    (cond [(equal? (list-ref board 1) "X")                          (place-image (circle 30 "solid" "blue") 360 60 img)]
                         [(equal? (list-ref board 1) "O")                          (place-image (circle 30 "solid" "red") 360 60 img)]
                         [else (place-image (text "-" 30 "black") 360 60 img)])
                   
                    (cond [(equal? (list-ref board 2) "X")                          (place-image (circle 30 "solid" "blue") 540 60 img)]
                         [(equal? (list-ref board 2) "O")                          (place-image (circle 30 "solid" "red") 540 60 img)]
                         [else (place-image (text "-" 30 "black") 540 60 img)])       
    
                    (cond [(equal? (list-ref board 3) "X")                          (place-image (circle 30 "solid" "blue") 180 240 img)]
                         [(equal? (list-ref board 3) "O")                          (place-image (circle 30 "solid" "red") 180 240 img)]
                         [else (place-image (text "-" 30 "black") 180 240 img)])
                    
                    (cond [(equal? (list-ref board 4) "X")                          (place-image (circle 30 "solid" "blue") 360 240 img)]
                          [(equal? (list-ref board 4) "O")                          (place-image (circle 30 "solid" "red") 360 240 img)]
                          [else (place-image (text "-" 30 "black") 360 240 img)])
                    
                    (cond [(equal? (list-ref board 5) "X")                          (place-image (circle 30 "solid" "blue") 540 240 img)]
                          [(equal? (list-ref board 5) "O")                          (place-image (circle 30 "solid" "red") 540 240 img)]
                          [else (place-image (text "-" 30 "black") 540 240 img)])
                    
                    (cond [(equal? (list-ref board 6) "X")                          (place-image (circle 30 "solid" "blue") 180 420 img)]
                          [(equal? (list-ref board 6) "O")                          (place-image (circle 30 "solid" "red") 180 420 img)]
                          [else (place-image (text "-" 30 "black") 180 420 img)])

                    (cond [(equal? (list-ref board 7) "X")                          (place-image (circle 30 "solid" "blue") 360 420 img)]
                          [(equal? (list-ref board 7) "O")                          (place-image (circle 30 "solid" "red") 360 420 img)]
                          [else (place-image (text "-" 30 "black") 360 420 img)])

                    (cond [(equal? (list-ref board 8) "X")                          (place-image (circle 30 "solid" "blue") 540 420 img)]
                          [(equal? (list-ref board 8) "O")                          (place-image (circle 30 "solid" "red") 540 420 img)]
                          [else (place-image (text "-" 30 "black") 540 420 img)])
                    
                    (place-image (circle 13 "solid" "black") 180 60 img)
                    (place-image (circle 13 "solid" "black") 360 60 img)
                    (place-image (circle 13 "solid" "black") 540 60 img)
                    (place-image (circle 13 "solid" "black") 180 240 img)
                    (place-image (circle 13 "solid" "black") 360 240 img)
                    (place-image (circle 13 "solid" "black") 540 240 img)
                    (place-image (circle 13 "solid" "black") 180 420 img)
                    (place-image (circle 13 "solid" "black") 360 420 img)
                    (place-image (circle 13 "solid" "black") 360 420 img)
                    (place-image (circle 13 "solid" "black") 540 420 img)
                   
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 60 img)
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 240 img)
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 420 img)
                 
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 180 240 img)
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 360 240 img)
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 540 240 img)
                    (place-image (rotate -45 (rectangle 5 509 'solid "black")) 360 240 img)
                    (place-image (rotate 45 (rectangle 5 509 'solid "black")) 360 240 img)))
    img))




;Manejo de mouse
(define (mouse s x y event)
  (cond
    [(mouse=? event "button-down") x]
    [(mouse=? event "button-down") y]
    [else s]
  )
)



(define initial-board '("" "X" ""
                         "" "" ""
                         "" "" ""))

(big-bang initial-board
  (to-draw draw-board))

(big-bang initial-board
  (to-draw draw-board))