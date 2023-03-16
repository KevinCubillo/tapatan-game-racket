#lang racket

(require 2htdp/image)
(require 2htdp/universe)



;Varibles globales
(define initial-board '("" "X" ""
                        "" "" ""
                        "" "" ""))
(define POS -1)
(define PLAYER "X")




(define (draw-board board)

 (define scene (empty-scene 720 720))

 (set! scene ((overlay img

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
                    (place-image (rotate 45 (rectangle 5 509 'solid "black")) 360 240 img)) scene )))

  




;Funcion que determina el movimiento de la ficha
  

  (define (handle-mouse s x y event)
   
  (if (equal? event "button-down")
      (begin
        (let ((box-x 180) (box-y 60) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 0))
                  (if (isEmpty 0) 
                    (begin (move POS 0 PLAYER) (change-player) (set! POS -1) (display "Entra"))
                    s)
              )
        s))

        (let ((box-x 360) (box-y 60) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 1))
                  (if (isEmpty 1) 
                    (begin (move POS 1 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 60) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 2))
                  (if (isEmpty 2) 
                    (begin (move POS 2 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 180) (box-y 240) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 3))
                  (if (isEmpty 3) 
                    (begin (move POS 3 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 360) (box-y 240) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 4))
                  (if (isEmpty 4) 
                    (begin (move POS 4 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 240) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 5))
                  (if (isEmpty 5) 
                    (begin (move POS 5 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 180) (box-y 420) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 6))
                  (if (isEmpty 6) 
                    (begin (move POS 6 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 360) (box-y 420) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 7))
                  (if (isEmpty 7) 
                    (begin (move POS 7 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 420) (box-w 40) (box-h 40))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 8))
                  (if (isEmpty 8) 
                    (begin (move POS 8 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
      )
      s
    )
 )


             
;Funcion que cambia un elemento de una lista
(define (modify-list lst index new-value)
  (cond
    [(empty? lst) '()]
    [(= index 0) (cons new-value (cdr lst))]
    [else (cons (car lst) (modify-list (cdr lst) (- index 1) new-value))]))


;Funcion que mueve una ficha de una poscion a otra
(define (move oldpos newpos player)
  (set! initial-board (modify-list initial-board newpos player))
  (set! initial-board (modify-list initial-board oldpos ""))
)

;Es posicion vacia
(define(isEmpty pos)
    (cond [(equal? (list-ref initial-board pos) "")]
     [else #f]))


;Cambiar jugador actual
(define (change-player)
 (if (equal? PLAYER "X")
     (set! PLAYER "O")
     (set! PLAYER "X"))
 )


 (big-bang initial-board
    [to-draw draw-board]   ;Se carga la escena inicial
    [on-mouse handle-mouse] ;Eventos con el mouse
  )
