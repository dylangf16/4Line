#lang racket/gui
(require 2htdp/image)
(require racket/math)
(require "Manipulacion_matriz.rkt")
(require "Miscelaneos.rkt")
(require "Greedy_algorithm.rkt")

;;Definición de la ventana de selección de Filas y Columnas
(define ventana (new frame%
                     [label "4Line"]
                     [width 400]
                     [height 300]))

;;Parte visual del frame ventana
(define canva (new canvas% [parent ventana]
                   [paint-callback
                    (lambda (canvas dc)
                      (send dc set-scale 1 1)
                      (send dc set-text-foreground "black")
                      (send dc draw-text "Ingrese los valores de la matriz que desea" 40 130)
                      (send dc set-scale 5 5)
                      (send dc set-text-foreground "red")
                      (send dc draw-text "4Line" 20 4))]))

;;Selección de las filas
(define choiceX (new choice% [parent ventana]
                     [label "Valor X"]
                     [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                     ))

;;Selección de las columnas
(define choiceY (new choice% [parent ventana]
                     [label "Valor Y"]
                     [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                     ))

(define msg (new message% [parent ventana]
                 [label "                 "]))

;;Botón para iniciar el juego
(new button% [parent ventana]
     [label "Empezar"]
     [callback (lambda (button event)
                 (send msg set-label "¡A Jugar!")
                 (send ventana show #f)
                 (PantallaJuegos (send choiceY get-selection) (send choiceY get-selection))
                 )])

;;Input: número máximo de filas, número máximo de columnas
;;Esta es la función donde está todo el código relacionado a la jugabilidad del programa
;;Output: ventana de juego
(define (PantallaJuegos fila columna)

  ;;Creación de la matriz que se usará en el código, y será paralela a la matriz de la interfaz
  (define matriz (buildMatrix '() (+ (send choiceX get-selection) 8) (+ (send choiceY get-selection) 8)))
  
  ;;Definición ventana de juego
  (define ventana2 (new frame%
                        [label "4Line"]
                        [width 1000]
                        [height 800]))

  ;;Parte visual del frame ventana2 (ventana de juego)
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
  
  ;;Selección de la columna donde el jugador soltará una ficha
  (define choice (new choice%
                      (label "Columna")
                      (parent ventana2)
                      (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"))
                      ))

  ;;Input: coordenadas "X" y "Y"
  ;;Generación de ficha de jugador
  ;;Output: ficha roja
  (define (ficha1 x y)
    (define dc (send canva2 get-dc))
    (send dc set-pen "red" 5 'solid)
    (send dc draw-ellipse x y 30 30))

  ;;Input: coordenadas "X" y "Y"
  ;;Generación de ficha del algoritmo
  ;;Output: ficha azul
  (define (fichaAI x y)
    (define dc (send canva2 get-dc))
    (send dc set-pen "blue" 5 'solid)
    (send dc draw-ellipse x y 30 30))

  ;;Input: fila, columna, 30, 30, 1, 1
  ;;Función que calcula donde poner la ficha, según las coordenadas dadas por el jugador
  ;;Output: llamada a ficha roja en posición solicitada
  (define (recorridoInterfaz fila columna x y contadorX contadorY)
    (cond
      ((and (equal? fila contadorX) (equal? columna contadorY)) (ficha1 y x))
      ((equal? fila contadorX) (recorridoInterfaz fila columna x (+ y 60) contadorX (+ contadorY 1)))
      (else (recorridoInterfaz fila columna (+ x 60) y (+ contadorX 1) contadorY))))

  ;;Input: fila, columna, 30, 30, 1, 1
  ;;Función que calcula donde poner la ficha, según las coordenadas dadas por el algoritmo
  ;;Output: llamada a ficha azul en posición solicitada
  (define (recorridoInterfaz2 fila columna x y contadorX contadorY)
    (cond
      ((and (equal? fila contadorX) (equal? columna contadorY)) (fichaAI y x))
      ((equal? fila contadorX) (recorridoInterfaz2 fila columna x (+ y 60) contadorX (+ contadorY 1)))
      (else (recorridoInterfaz2 fila columna (+ x 60) y (+ contadorX 1) contadorY))))


  ;;Input: fila y columna donde se pondrá la ficha
  ;;Esta función se encarga de actualizar la matriz de código y llamar la ejecución de la ficha del algoritmo y verificación
  ;;Output: coordenadas para la matriz de juego
  (define (insertarFicha1 fila columna)
    (set! matriz (replaceInMatrix matriz (- (encontrarFila matriz columna 1) 1) (- columna 1) 1 0))
    (cond
      ((<= (string->number (send choice get-string-selection)) (+ 8 (send choiceX get-selection)))
       (recorridoInterfaz (+(encontrarFila matriz columna 1) 1) columna 30 30 1 1))   
      (else 0))
    (verificacion)
    (insertarFichaIA))

  ;;Input: fila y columna donde se pondrá la ficha
  ;;Esta función se encarga de actualizar la matriz de código con respecto a la posición dada por el algoritmo
  ;;Output: coordenadas para la matriz de juego
  (define (insertarFichaIA)
    (define fila (car (conjuntoCandidatos matriz matriz (buildList '() (+ 8 (send choiceY get-selection))) 1 (+ 8 (send choiceX get-selection)) (+ 8 (send choiceY get-selection)))))
    (define columna (cadr (conjuntoCandidatos matriz matriz (buildList '() (+ 8 (send choiceY get-selection))) 1 (+ 8 (send choiceX get-selection)) (+ 8 (send choiceY get-selection)))))
    (println  "fila: ")
    (displayln fila)
    (println "columna: ")
    (displayln columna)
    (set! matriz (replaceInMatrix matriz (- (encontrarFila matriz columna 1) 1) (- columna 1) 2 0))
    (recorridoInterfaz2 fila columna 30 30 1 1)
    (verificacion))
    
  (new button% [parent ventana2]
       [label "Seleccionar columna"]
       [callback (lambda (button event) (insertarFicha1 (encontrarFila matriz (- (string->number (send choice get-string-selection)) 1) 1) (string->number (send choice get-string-selection))))])

  ;;Input: null
  ;;Función que se encarga de llamar la función que verifica si hay un 4 en línea, y decir cuales fichas ganaron
  ;;Output: muestra la ventana del ganador, si no hay retorna 0
  (define (verificacion)
    (cond
      ((equal? (4inLine matriz 1) #t) (send ventana3 show #t))
      ((equal? (4inLine matriz 2) #t) (send ventana4 show #t))
      (else 0)))

  ;;Frame para ventana ganador jugador
  (define ventana3 (new frame%
                        [label "4Line"]
                        [width 1000]
                        [height 200]))

  (define canva3 (new canvas% [parent ventana3]
                      [paint-callback
                       (lambda (canvas dc)
                         (send dc set-scale 1 1)
                         (send dc set-text-foreground "black")
                         (send dc set-scale 5 5)
                         (send dc set-text-foreground "blue")
                         (send dc draw-text "GANADOR JUGADOR!!!!" 20 4))]))

  (new button% [parent ventana3]
       [label "Cerrar el juego"]
       [callback (lambda (button event)
                   (send msg set-label "Presiona para cerrar el juego!")
                   (send ventana show #f)
                   (send ventana2 show #f)
                   (send ventana3 show #f)
                   (send ventana4 show #f)
                   )])


  ;;Frame para ventana ganador algoritmo
  (define ventana4 (new frame%
                        [label "4Line"]
                        [width 1100]
                        [height 200]))
  
  (define canva4 (new canvas% [parent ventana4]
                      [paint-callback
                       (lambda (canvas dc)
                         (send dc set-scale 1 1)
                         (send dc set-text-foreground "black")
                         (send dc set-scale 5 5)
                         (send dc set-text-foreground "blue")
                         (send dc draw-text "GANADOR ALGORITMO!!!!!" 20 4))]))
  
  (new button% [parent ventana4]
       [label "Cerrar el juego"]
       [callback (lambda (button event)
                   (send msg set-label "Presiona para cerrar el juego!")
                   (send ventana show #f)
                   (send ventana2 show #f)
                   (send ventana3 show #f)
                   (send ventana4 show #f)
                   )])

  ;;Ejecución de la ventaja de juego
  (send ventana2 show #t)
  )
;;Ejecución de la ventana principal
(send ventana show #t)