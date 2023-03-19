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

                  (cond [(and(equal? check-winners #t) (equal? PLAYER "X"))  (place-image (text "Ganaste jugador rojo" 30 "black") 360 600 img)]
                        [(and(equal? check-winners #t) (equal? PLAYER "O"))  (place-image (text "Ganaste jugador azul" 30 "black") 360 600 img)]
                         [else (place-image (text "-" 30 "black") 360 600 img)])


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



(define check-winners #f)
;Funcion que determina el movimiento de la ficha
(define (handle-mouse s x y event)
  (if (equal? event "button-down")
      (begin
      ;Fila 0 espacio 0
      (let ((box-x 180) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 0)
                    (if (not (equal? POS -1)) 
                        (begin (move POS 0 PLAYER)       
                         (set! POS -1)
                        )
                    s)
                (set! POS 0))
                (if (isEmpty 0)
                    (begin
                      (put-token 0 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 360) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 1)
                    (if (not (equal? POS -1))
                        (begin (move POS 1 PLAYER)               
                         (set! POS -1)
                        )
                    s)
                (set! POS 1))
                (if (isEmpty 1)
                    (begin
                      (put-token 1 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 540) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 2)
                    (if (not (equal? POS -1))
                        (begin (move POS 2 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 2))
                (if (isEmpty 2)
                    (begin
                      (put-token 2 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 180) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 3)
                    (if (not (equal? POS -1))
                        (begin (move POS 3 PLAYER)  
                         (set! POS -1)
                        )
                    s)
                (set! POS 3))
                (if (isEmpty 3)
                    (begin
                      (put-token 3 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 360) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 4)
                    (if (not (equal? POS -1))
                        (begin (move POS 4 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 4))
                (if (isEmpty 4)
                    (begin
                      (put-token 4 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 540) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 5)
                    (if (not (equal? POS -1))
                        (begin (move POS 5 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 5))
                (if (isEmpty 5)
                    (begin
                      (put-token 5 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 180) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 6)
                    (if (not (equal? POS -1))
                        (begin (move POS 6 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 6))
                (if (isEmpty 6)
                    (begin
                      (put-token 6 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 360) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 7)
                    (if (not (equal? POS -1))
                        (begin (move POS 7 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 7))
                (if (isEmpty 7)
                    (begin
                      (put-token 7 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
        (let ((box-x 540) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))))
            (if (equal? initial-token 0)
                (if (isEmpty 8)
                    (if (not (equal? POS -1))
                        (begin(move POS 8 PLAYER)
                         (set! POS -1)
                        )
                    s)
                (set! POS 8))
                (if (isEmpty 8)
                    (begin
                      (put-token 8 PLAYER)
                    )
                    s)
            )s)  
        ); cierre del let
    s); cierre del if principal
  s); cierre de la funcion en si
)


        
             
;Funcion que cambia un elemento de una lista
(define (modify-list lst index new-value)
  (cond
    [(empty? lst) '()]
    [(= index 0) (cons new-value (cdr lst))]
    [else (cons (car lst) (modify-list (cdr lst) (- index 1) new-value))]))


;Funcion que mueve una ficha de una poscion a otra
(define (adjacent-cells index)
  (cond ((= index 0) '(1 3 4))
        ((= index 1) '(0 2 3 4 5))
        ((= index 2) '(1 4 5))
        ((= index 3) '(0 1 4 6 7))
        ((= index 4) '(0 1 2 3 5 6 7 8))
        ((= index 5) '(1 2 4 7 8))
        ((= index 6) '(3 4 7))
        ((= index 7) '(3 4 5 6 8))
        ((= index 8) '(4 5 7))
        (else '())))

(define (move oldpos newpos player)
  (if (equal? (list-ref initial-board oldpos) player)
      (let ((adj-cells (adjacent-cells oldpos)))
        (if (member newpos adj-cells)
            (begin  (set! initial-board (modify-list initial-board newpos player))
                    (set! initial-board (modify-list initial-board oldpos ""))
                    (check-winner)
                    (change-player))
            #f))
      #f))

;Colocar ficha
(define (put-token pos player)
  (set! initial-token(- initial-token 1))
  (set! initial-board (modify-list initial-board pos player))
  (check-winner)
  (change-player)
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

;Funcion para verificar si alguien gano en el tablero

(define(check-winner)
  ;Verifica la primera fila
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 1)) (equal? (list-ref initial-board 1) (list-ref initial-board 2)))
      (if (equal?(list-ref initial-board 0) "X")
          (set! check-winners #t) 
          (if (equal?(list-ref initial-board 0) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  ;Verifica la segunda fila 
  (if (and (equal? (list-ref initial-board 3) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 5)))
      (if (equal?(list-ref initial-board 3) "X")
          (set! check-winners #t) 
          (if (equal?(list-ref initial-board 3) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )

  ;Verifica la tercera fila 
  (if (and (equal? (list-ref initial-board 6) (list-ref initial-board 7)) (equal? (list-ref initial-board 7) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 6) "X")
          (set! check-winners #t)
          (if (equal?(list-ref initial-board 6) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )

  ;Verifica pimera columna 
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 3)) (equal? (list-ref initial-board 3) (list-ref initial-board 6)))
      (if (equal?(list-ref initial-board 0) "X")
          (set! check-winners #t)
          (if (equal?(list-ref initial-board 0) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  ;Verifica la segunda columna 
  (if (and (equal? (list-ref initial-board 1) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 7)))
      (if (equal?(list-ref initial-board 1) "X")
          (set! check-winners #t)
          (if (equal?(list-ref initial-board 1) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )

   ;Verifica la tercera columna 
  (if (and (equal? (list-ref initial-board 2) (list-ref initial-board 5)) (equal? (list-ref initial-board 5) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 2) "X")
          (set! check-winners #t) 
          (if (equal?(list-ref initial-board 2) "0")
          (set! check-winners #t)
          #f)
          )
      #f
      )

  ;Verifica la diagonal derecha 
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 0) "X")
          (set! check-winners #t) 
          (if (equal?(list-ref initial-board 0) "0")
          (set! check-winners #t)
          #f)
          )
      #f
      )

  ;Verifica la diagonal izquierda 
  (if (and (equal? (list-ref initial-board 2) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 6)))
      (if (equal?(list-ref initial-board 2) "X")
          (set! check-winners #t)
          (if (equal?(list-ref initial-board 2) "0")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  )
 
(define (set-player s key)
    (display "Hello, world!")   
  (cond
    ((key=? key "1") (set! PLAYER "X"))
    ((key=? key "2") (set! PLAYER "O"))))


(check-winner)
 (big-bang 0
    [to-draw draw-board]   ;Se carga la escena inicial
    [on-mouse handle-mouse] ;Eventos con el mouse
    [on-key set-player]     ;Eventos con el teclado
  )