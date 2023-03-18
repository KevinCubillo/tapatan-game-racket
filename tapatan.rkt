#lang racket

(require 2htdp/image)
(require 2htdp/universe)



;Varibles globales
(define initial-board '("" "" ""
                        "" "" ""
                        "" "" ""))
(define POS -1)

(define PLAYER "X")

(define initial-token 6)
 
(define (draw-board a)
  (let ((img (rectangle 720 720 'outline (make-color 200 200 200))))
    (set! img
          (overlay img

                    (cond [(equal? (list-ref initial-board 0) "X") (place-image (circle 30 "solid" "blue") 180 60 img)]
                         [(equal? (list-ref initial-board 0) "O") (place-image (circle 30 "solid" "red") 180 60 img)]
                         [else (place-image (text "-" 30 "black") 180 60 img)])
                   
                    (cond [(equal? (list-ref initial-board 1) "X")                          (place-image (circle 30 "solid" "blue") 360 60 img)]
                         [(equal? (list-ref initial-board 1) "O")                          (place-image (circle 30 "solid" "red") 360 60 img)]
                         [else (place-image (text "-" 30 "black") 360 60 img)])
                   
                    (cond [(equal? (list-ref initial-board 2) "X")                          (place-image (circle 30 "solid" "blue") 540 60 img)]
                         [(equal? (list-ref initial-board 2) "O")                          (place-image (circle 30 "solid" "red") 540 60 img)]
                         [else (place-image (text "-" 30 "black") 540 60 img)])       
    
                    (cond [(equal? (list-ref initial-board 3) "X")                          (place-image (circle 30 "solid" "blue") 180 240 img)]
                         [(equal? (list-ref initial-board 3) "O")                          (place-image (circle 30 "solid" "red") 180 240 img)]
                         [else (place-image (text "-" 30 "black") 180 240 img)])
                    
                    (cond [(equal? (list-ref initial-board 4) "X")                          (place-image (circle 30 "solid" "blue") 360 240 img)]
                          [(equal? (list-ref initial-board 4) "O")                          (place-image (circle 30 "solid" "red") 360 240 img)]
                          [else (place-image (text "-" 30 "black") 360 240 img)])
                    
                    (cond [(equal? (list-ref initial-board 5) "X")                          (place-image (circle 30 "solid" "blue") 540 240 img)]
                          [(equal? (list-ref initial-board 5) "O")                          (place-image (circle 30 "solid" "red") 540 240 img)]
                          [else (place-image (text "-" 30 "black") 540 240 img)])
                    
                    (cond [(equal? (list-ref initial-board 6) "X")                          (place-image (circle 30 "solid" "blue") 180 420 img)]
                          [(equal? (list-ref initial-board 6) "O")                          (place-image (circle 30 "solid" "red") 180 420 img)]
                          [else (place-image (text "-" 30 "black") 180 420 img)])

                    (cond [(equal? (list-ref initial-board 7) "X")                          (place-image (circle 30 "solid" "blue") 360 420 img)]
                          [(equal? (list-ref initial-board 7) "O")                          (place-image (circle 30 "solid" "red") 360 420 img)]
                          [else (place-image (text "-" 30 "black") 360 420 img)])

                    (cond [(equal? (list-ref initial-board 8) "X")                          (place-image (circle 30 "solid" "blue") 540 420 img)]
                          [(equal? (list-ref initial-board 8) "O")                          (place-image (circle 30 "solid" "red") 540 420 img)]
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

  
;Funcion que determina el movimiento de la ficha
  

  (define (handle-mouse s x y event)
   
  (if (equal? event "button-down")
      (begin
        (let ((box-x 180) (box-y 60) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 0))
                  (if (isEmpty 0) 
                    (begin (move POS 0 PLAYER) (change-player) (set! POS -1) (display "Entra"))
                    s)
              )
        s))

        (let ((box-x 360) (box-y 60) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 1))
                  (if (isEmpty 1) 
                    (begin (move POS 1 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 60) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 2))
                  (if (isEmpty 2) 
                    (begin (move POS 2 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 180) (box-y 260) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 3))
                  (if (isEmpty 3) 
                    (begin (move POS 3 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 360) (box-y 260) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 4))
                  (if (isEmpty 4) 
                    (begin (move POS 4 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 260) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 5))
                  (if (isEmpty 5) 
                    (begin (move POS 5 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 180) (box-y 420) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 6))
                  (if (isEmpty 6) 
                    (begin (move POS 6 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 360) (box-y 420) (box-w 60) (box-h 60))
          (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                   (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
              (if (equal? POS -1)
                  (begin (set! POS 7))
                  (if (isEmpty 7) 
                    (begin (move POS 7 PLAYER) (change-player) (set! POS -1))
                    s)
              )
        s))
        (let ((box-x 540) (box-y 420) (box-w 60) (box-h 60))
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

;Colocar ficha
(define (put-token pos player)
  (if (not (equal? initial-token 0))
      (begin
        (set! initial-board (modify-list initial-board pos player))
        (change-player)
        (set! initial-token (- initial-token 1))
        (display "ficha colocada"))  ; retornar true si se ejecutan las acciones dentro del if
      (display "ficha no colocada")))  ; retornar false si no se ejecuta el if

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

;Funcion para verificar si alguien gano en el tablero

(define(check-winner)
  ;Verifica la primera fila
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 1)) (equal? (list-ref initial-board 1) (list-ref initial-board 2)))
      (if (equal?(list-ref initial-board 0) "X")
          (display "Gano el azul en la primera fila") 
          (if (equal?(list-ref initial-board 0) "O")
          (display "Gano el rojo en la primera fila")
          #f)
          )
      #f
      )
  ;Verifica la segunda fila 
  (if (and (equal? (list-ref initial-board 3) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 5)))
      (if (equal?(list-ref initial-board 3) "X")
          (display "Gano el azul en la segunda fila") 
          (if (equal?(list-ref initial-board 3) "O")
          (display "Gano el rojo en la segunda fila")
          #f)
          )
      #f
      )

  ;Verifica la tercera fila 
  (if (and (equal? (list-ref initial-board 6) (list-ref initial-board 7)) (equal? (list-ref initial-board 7) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 6) "X")
          (display "Gano el azul en la tercera fila") 
          (if (equal?(list-ref initial-board 6) "O")
          (display "Gano el rojo en la tercera fila")
          #f)
          )
      #f
      )

  ;Verifica pimera columna 
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 2)) (equal? (list-ref initial-board 2) (list-ref initial-board 6)))
      (if (equal?(list-ref initial-board 0) "X")
          (display "Gano el azul en la primera columna") 
          (if (equal?(list-ref initial-board 0) "O")
          (display "Gano el rojo en la primera columna")
          #f)
          )
      #f
      )
  ;Verifica la segunda columna 
  (if (and (equal? (list-ref initial-board 1) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 7)))
      (if (equal?(list-ref initial-board 1) "X")
          (display "Gano el azul en la primera columna") 
          (if (equal?(list-ref initial-board 1) "O")
          (display "Gano el rojo en la primera columna")
          #f)
          )
      #f
      )

   ;Verifica la tercera columna 
  (if (and (equal? (list-ref initial-board 2) (list-ref initial-board 5)) (equal? (list-ref initial-board 5) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 2) "X")
          (display "Gano el azul en la primera columna") 
          (if (equal?(list-ref initial-board 2) "0")
          (display "Gano el rojo en la primera columna")
          #f)
          )
      #f
      )

  ;Verifica la diagonal derecha 
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 0) "X")
          (display "Gano el azul en la primera columna") 
          (if (equal?(list-ref initial-board 0) "0")
          (display "Gano el rojo en la primera columna")
          #f)
          )
      #f
      )

  ;Verifica la diagonal izquierda 
  (if (and (equal? (list-ref initial-board 2) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 6)))
      (if (equal?(list-ref initial-board 2) "X")
          (display "Gano el azul en la primera columna") 
          (if (equal?(list-ref initial-board 2) "0")
          (display "Gano el rojo en la primera columna")
          #f)
          )
      #f
      )
  )
 


;[(equal? (list-ref initial-board 0) "X") (place-image (circle 30 "solid" "blue") 180 60 img)


;(if (and (> numero 5) (< numero 10))
 ;   (cuerda)
  ;  'otro-valor)





;(put-token 0 PLAYER)
;(put-token 1 PLAYER)
;(put-token 2 PLAYER)
;(put-token 3 PLAYER)
;(put-token 4 PLAYER)
;(put-token 5 PLAYER)
;(put-token 6 PLAYER)
;(put-token 7 PLAYER)

(check-winner)
 (big-bang 0
    [to-draw draw-board]   ;Se carga la escena inicial
    [on-mouse handle-mouse] ;Eventos con el mouse
  )