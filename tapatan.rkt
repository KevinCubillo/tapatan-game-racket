
;Juego de Tapatan. Creado por: Kevin Cubillo y Brandon Retana
#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;Varibles globales
(define initial-board '("" "" ""
                        "" "" ""
                        "" "" "")) ;Tablero de juego

(define POS -1) ; Poscicion seleccionada
(define PLAYER "X") ;Jugador actual
(define initial-token 6); Fichas restantes por colocar



;Funcion que muestra el estado del juego
(define (draw-board a)
  (let ((img (rectangle 720 720 'outline (make-color 200 200 200))))
    (set! img
          (overlay img

                   (cond [(equal? initial-token 6) (place-image (text "tecla 1: inicia el jugador azul \n \n tecla 2: inicia el jugador rojo" 25 "black") 360 600 img)]
                        [else (place-image (text "" 15 "black") 360 660 img)]) ;Muestra el mensaje inicial para seleccionar el jugador que inicia

                  (cond [(and(equal? check-winners #t) (equal? PLAYER "X"))  (place-image (text "¡Ganaste jugador rojo!" 30 "black") 360 600 img)]
                        [(and(equal? check-winners #t) (equal? PLAYER "O"))  (place-image (text "¡Ganaste jugador azul!" 30 "black") 360 600 img)]
                         [else (place-image (text "" 30 "black") 360 600 img)]) ;Mensaje si algun jugador gana
                  
                  
                    (cond [(equal? (list-ref initial-board 0) "X") (place-image (circle 30 "solid" "blue") 180 60 img)]
                         [(equal? (list-ref initial-board 0) "O") (place-image (circle 30 "solid" "red") 180 60 img)]
                         [else (place-image (text "-" 30 "black") 180 60 img)]); Muestra una ficha en la posicion 0
                   
                    (cond [(equal? (list-ref initial-board 1) "X")                          (place-image (circle 30 "solid" "blue") 360 60 img)]
                         [(equal? (list-ref initial-board 1) "O")                          (place-image (circle 30 "solid" "red") 360 60 img)]
                         [else (place-image (text "-" 30 "black") 360 60 img)]); Muestra una ficha en la posicion 1
                   
                    (cond [(equal? (list-ref initial-board 2) "X")                          (place-image (circle 30 "solid" "blue") 540 60 img)]
                         [(equal? (list-ref initial-board 2) "O")                          (place-image (circle 30 "solid" "red") 540 60 img)]
                         [else (place-image (text "-" 30 "black") 540 60 img)]) ; Muestra una ficha en la posicion 2
    
                    (cond [(equal? (list-ref initial-board 3) "X")                          (place-image (circle 30 "solid" "blue") 180 240 img)]
                         [(equal? (list-ref initial-board 3) "O")                          (place-image (circle 30 "solid" "red") 180 240 img)]
                         [else (place-image (text "-" 30 "black") 180 240 img)]); Muestra una ficha en la posicion 3
                    
                    (cond [(equal? (list-ref initial-board 4) "X")                          (place-image (circle 30 "solid" "blue") 360 240 img)]
                          [(equal? (list-ref initial-board 4) "O")                          (place-image (circle 30 "solid" "red") 360 240 img)]
                          [else (place-image (text "-" 30 "black") 360 240 img)]); Muestra una ficha en la posicion 4
                    
                    (cond [(equal? (list-ref initial-board 5) "X")                          (place-image (circle 30 "solid" "blue") 540 240 img)]
                          [(equal? (list-ref initial-board 5) "O")                          (place-image (circle 30 "solid" "red") 540 240 img)]
                          [else (place-image (text "-" 30 "black") 540 240 img)]); Muestra una ficha en la posicion 5
                    
                    (cond [(equal? (list-ref initial-board 6) "X")                          (place-image (circle 30 "solid" "blue") 180 420 img)]
                          [(equal? (list-ref initial-board 6) "O")                          (place-image (circle 30 "solid" "red") 180 420 img)]
                          [else (place-image (text "-" 30 "black") 180 420 img)]); Muestra una ficha en la posicion 6

                    (cond [(equal? (list-ref initial-board 7) "X")                          (place-image (circle 30 "solid" "blue") 360 420 img)]
                          [(equal? (list-ref initial-board 7) "O")                          (place-image (circle 30 "solid" "red") 360 420 img)]
                          [else (place-image (text "-" 30 "black") 360 420 img)]); Muestra una ficha en la posicion 7

                    (cond [(equal? (list-ref initial-board 8) "X")                          (place-image (circle 30 "solid" "blue") 540 420 img)]
                          [(equal? (list-ref initial-board 8) "O")                          (place-image (circle 30 "solid" "red") 540 420 img)]
                          [else (place-image (text "-" 30 "black") 540 420 img)]); Muestra una ficha en la posicion 8
                    
                    (place-image (circle 13 "solid" "black") 180 60 img)
                    (place-image (circle 13 "solid" "black") 360 60 img)
                    (place-image (circle 13 "solid" "black") 540 60 img)
                    (place-image (circle 13 "solid" "black") 180 240 img)
                    (place-image (circle 13 "solid" "black") 360 240 img)
                    (place-image (circle 13 "solid" "black") 540 240 img)
                    (place-image (circle 13 "solid" "black") 180 420 img)
                    (place-image (circle 13 "solid" "black") 360 420 img)
                    (place-image (circle 13 "solid" "black") 360 420 img)
                    (place-image (circle 13 "solid" "black") 540 420 img) ; Dibuja los circulos del tablero
                   
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 60 img)
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 240 img)
                    (place-image (rectangle 360 5 'solid (make-color 0 0 0)) 360 420 img) ;Dibuja las rayas verticales del tablero
                 
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 180 240 img)
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 360 240 img)
                    (place-image (rectangle 5 360 'solid (make-color 0 0 0)) 540 240 img);Dibuja las rayas horizontales del tablero
                    
                    (place-image (rotate -45 (rectangle 5 509 'solid "black")) 360 240 img)
                    (place-image (rotate 45 (rectangle 5 509 'solid "black")) 360 240 img))) ;Dibuja las rayas diagonales del tablero               
    img))


;Funcion que determina la celda en la que se va a colocar una ficha
(define check-winners #f)
(define (handle-mouse s x y event)
  (if (equal? event "button-down")
      (begin
      (let ((box-x 180) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2)))) ;Determina si se clickeo en la poscion 0
            (if (equal? initial-token 0); Si no hay fichas por poner solo permite mover las existentes
                (if (isEmpty 0); Si la celda 0 está vacia 
                    (if (not (equal? POS -1)) ;Si previamente se seleccionó una ficha
                        (begin (move POS 0 PLAYER) ;Realiza el movimiento      
                         (set! POS -1); 
                        )
                    s)
                (set! POS 0))
                (if (isEmpty 0); 
                    (begin
                      (put-token 0 PLAYER); Se coloca una ficha en la celda si está vacia y aún quedan fichas disponibles
                    )
                    s)
            )s)  
        )
        (let ((box-x 360) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 1
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
        )
        (let ((box-x 540) (box-y 60) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 2
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
        )
        (let ((box-x 180) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 3
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
        )
        (let ((box-x 360) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 4
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
        )
        (let ((box-x 540) (box-y 240) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 5
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
        )
        (let ((box-x 180) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 6
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
        )
        (let ((box-x 360) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 7
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
        )
        (let ((box-x 540) (box-y 420) (box-w 60) (box-h 60))
        (if (and (>= x (- box-x (/ box-w 2))) (<= x (+ box-x (/ box-w 2)))
                 (>= y (- box-y (/ box-h 2))) (<= y (+ box-y (/ box-h 2))));Determina si se clickeo en la poscion 8
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
        )
    s)
  s)
)

          
;Funcion que reemplaza un elemento de una lista
(define (modify-list lst index new-value)
  (cond
    [(empty? lst) '()]
    [(= index 0) (cons new-value (cdr lst))]
    [else (cons (car lst) (modify-list (cdr lst) (- index 1) new-value))]))


;Funcion auxiliar de move, la cual determina las posiciones validas de movimiento
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


;Funcion que mueve una ficha de una poscion a otra
(define (move oldpos newpos player)
  (if (equal? check-winners #t) #f (if (equal? (list-ref initial-board oldpos) player); Si no hay ganador continua con el movimiento
      (let ((adj-cells (adjacent-cells oldpos)))
        (if (member newpos adj-cells);Comprueba que sea un movimiento valido
            (begin  (set! initial-board (modify-list initial-board newpos player));Coloca la ficah en la celda de destino
                    (set! initial-board (modify-list initial-board oldpos "")); Borra la ficha en la celda de origen 
                    (check-winner);Comprueba si hay ganador
                    (change-player)); Cambia el jugador actual
            #f))
     #f))
  )
  
 
;Funcion que coloca una ficha en una celda 
(define (put-token pos player)
  (set! initial-token(- initial-token 1)); Disminuye la cantidad de fichas disponibles
  (set! initial-board (modify-list initial-board pos player)); Coloca la ficha en el tablero
  (check-winner);Comprueba si hay ganador
  (change-player); Cambia el jugador actual
)

;Funcion que determina si una celda está vacia
(define(isEmpty pos)
    (cond [(equal? (list-ref initial-board pos) "")]
     [else #f]))


;Funcion que cambia el jugador actual
(define (change-player)
 (if (equal? PLAYER "X")
     (set! PLAYER "O")
     (set! PLAYER "X"))
 )

;Funcion para verificar si alguien gano en el juego
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
          (if (equal?(list-ref initial-board 2) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  ;Verifica la diagonal derecha 
  (if (and (equal? (list-ref initial-board 0) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 8)))
      (if (equal?(list-ref initial-board 0) "X")
          (set! check-winners #t) 
          (if (equal?(list-ref initial-board 0) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  ;Verifica la diagonal izquierda 
  (if (and (equal? (list-ref initial-board 2) (list-ref initial-board 4)) (equal? (list-ref initial-board 4) (list-ref initial-board 6)))
      (if (equal?(list-ref initial-board 2) "X")
          (set! check-winners #t)
          (if (equal?(list-ref initial-board 2) "O")
          (set! check-winners #t)
          #f)
          )
      #f
      )
  )

;Funcion que establece el jugador que inicia la partida al presionar la techa 1 o 2.
(define (set-player s key)
 (if (equal? initial-token 6) 
     (if (key=? key "1") (set! PLAYER "X") 
     (if (key=? key "2") (set! PLAYER "O") s))   
  s)
)


;Funcion Big-bang 
 (big-bang 0
    [to-draw draw-board]   ;Se carga la escena inicial
    [on-mouse handle-mouse] ;Eventos con el mouse
    [on-key set-player]     ;Eventos con el teclado
  )