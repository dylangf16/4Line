#lang racket/gui
(require 2htdp/image)
(require racket/math)
;;(require racket/draw)
;;(require racket/gui/base)

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
                         [label "Valor x"]
                         [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16")]
                         ))

(define msg (new message% [parent ventana]
                          [label "                 "]))



(new button% [parent ventana]
             [label "Empezar"]
             [callback (lambda (button event)
                         (send msg set-label "¡A Jugar!")
                         (send ventana2 show #t)
                         (send ventana show #f)
                         )])
                         



(define (ValorX num)
  (cons num '()))
(define (ValorY num)
  (cons num '()))


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

(define (insertarFicha1 columna)
  (cond
    ((equal? 0 (send choice get-selection))
     (ficha 30 30))))
;;(define (insertarFichaAI x y))

(new button% [parent ventana2]
                     [label "yey"]
                     [callback (lambda (button event)
                                 (insertarFicha1 (send choice get-selection)))])

                    



(send ventana show #t)