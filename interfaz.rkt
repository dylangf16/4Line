#lang racket/gui
(require 2htdp/image)
(require racket/math)
(require "Manipulacion_matriz.rkt")
(require "Miscelaneos.rkt")
(require "Greedy_algorithm.rkt")

(define ventana (new frame%
                   [label "4Line"]
                   [width 400]
                   [height 300]))


(define canva (new canvas% [parent ventana]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1 1)
                (send dc set-text-foreground "black")
                (send dc draw-text "Ingrese los valores de la matriz que desea" 40 130)
                (send dc set-scale 5 5)
                (send dc set-text-foreground "red")
                (send dc draw-text "4Line" 20 4))]))

(define choiceX (new choice% [parent ventana]
                         [label "Valor x"]
                         [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                         ))
(define choiceY (new choice% [parent ventana]
                         [label "Valor y"]
                         [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                         ))

(define msg (new message% [parent ventana]
                          [label "                 "]))

(new button% [parent ventana]
             [label "Empezar"]
             [callback (lambda (button event)
                         (send msg set-label "¡A Jugar!")
                         (send ventana show #f)
                         (PantallaJuegos (send choiceY get-selection) (send choiceY get-selection))
                         )])

;;------------------------------------------------
(define (PantallaJuegos fila columna)
  
  (define matriz (buildMatrix '() (+ (send choiceX get-selection) 8) (+ (send choiceY get-selection) 8)))
  
  
  (define ventana2 (new frame%
                        [label "4Line"]
                        [width 1000]
                        [height 800]))

  (define canva2 (new canvas% [parent ventana2]
                      [paint-callback
                       (lambda (canvas dc)
                         (send dc set-pen "black" 2 'solid)
                         (define ancho 50)
                         (define alto 50)
                         (define espacio 10)
                         (define posX 20)
                         (define posY 20)
                         (for ([i (+ 8 (send choiceX get-selection))])
                           (define x posX)
                           (define y (+ posY (* i (+ alto espacio))))
                           (for ([j (+ 8 (send choiceY get-selection))])
                             (send dc draw-rectangle x y ancho alto)
                             (set! x (+ x ancho espacio))))
                         )]
                      ))
  (define choice (new choice%
                      (label "Columna")
                      (parent ventana2)
                      (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"))
                      ))

  (define (ficha1 x y)
    (define dc (send canva2 get-dc))
    (send dc set-pen "red" 5 'solid)
    (send dc draw-ellipse x y 30 30))

  (define (fichaAI x y)
    (define dc (send canva2 get-dc))
    (send dc set-pen "blue" 5 'solid)
    (send dc draw-ellipse x y 30 30))

  (define (recorridoInterfaz fila columna x y contadorX contadorY)
    (cond
      ((and (equal? fila contadorX) (equal? columna contadorY)) (ficha1 y x))
      ((equal? fila contadorX) (recorridoInterfaz fila columna x (+ y 60) contadorX (+ contadorY 1)))
      (else (recorridoInterfaz fila columna (+ x 60) y (+ contadorX 1) contadorY))))

  (define (recorridoInterfaz2 fila columna x y contadorX contadorY)
    (cond
      ((and (equal? fila contadorX) (equal? columna contadorY)) (fichaAI y x))
      ((equal? fila contadorX) (recorridoInterfaz2 fila columna x (+ y 60) contadorX (+ contadorY 1)))
      (else (recorridoInterfaz2 fila columna (+ x 60) y (+ contadorX 1) contadorY))))

  ;; 30 / 30 es el (1,1)
  ;; +60 para moverse entre casillas
  (define (insertarFicha1 fila columna)
    (set! matriz (replaceInMatrix matriz (- (encontrarFila matriz columna 1) 1) (- columna 1) 1 0))
    (cond
      ((<= (string->number (send choice get-string-selection)) (+ 8 (send choiceX get-selection)))
       (recorridoInterfaz (+(encontrarFila matriz columna 1) 1) columna 30 30 1 1))   
      (else 0))
    
    (insertarFichaIA))

  (define (insertarFichaIA)
    (define fila (car (conjuntoCandidatos matriz matriz (buildList '() (+ 8 (send choiceY get-selection))) 1 (+ 8 (send choiceX get-selection)) (+ 8 (send choiceY get-selection)))))
    (define columna (cadr (conjuntoCandidatos matriz matriz (buildList '() (+ 8 (send choiceY get-selection))) 1 (+ 8 (send choiceX get-selection)) (+ 8 (send choiceY get-selection)))))
    (display fila)
    (display columna)
    (display matriz)
    (set! matriz (replaceInMatrix matriz (- (encontrarFila matriz columna 1) 5) (- columna 1) 12545 0))
    (recorridoInterfaz2 fila columna 30 30 1 1))   
    
      (new button% [parent ventana2]
       [label "yey"]
       [callback (lambda (button event) (insertarFicha1 (encontrarFila matriz (- (string->number (send choice get-string-selection)) 1) 1) (string->number (send choice get-string-selection))))])

  (send ventana2 show #t)

  
  )

                    



(send ventana show #t)