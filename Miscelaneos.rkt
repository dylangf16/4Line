#lang scheme 
(provide encontrarColumna verifCeros verifNum eliminaUnValor cambiarValorEspecifico Peso moverMatriz moverMatrizSuperior construirNuevaMatrizTemp FuncionSeleccionAux encontrarFila replace-all)

(define matrix '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 1 0 2 2 2)
                 (1 0 1 2 5 2 1 2)
                 (2 0 2 2 2 1 2 1)
                 (1 2 1 2 1 1 2 2)))


;;matriz, columna donde revisar, 1, 
(define (encontrarFila matriz columna fila)
  (cond
    ((null? matriz) (- fila 1))
    ((>  (encontrarFilaAux (car matriz) columna) 0) (- fila 1))
    (else (encontrarFila (cdr matriz) columna (+ fila 1)))))

(define (encontrarFilaAux lista columna)
  (cond
    ((null? lista) 0)
    ((equal? columna 1) (car lista))
    (else (encontrarFilaAux (cdr lista) (- columna 1)))))

(encontrarFila matrix 5 1)


;;Función que retorna la columna del primer valor que se encuentra
(define (encontrarColumna lista columna)
  (cond
    ((null? lista) columna)
    ((equal? (car lista) 0) (encontrarColumna (cdr lista) (+ columna 1)))
    (else (+ columna 1))))

;;Retorna TRUE si la fila tiene solamente 0
(define (verifCeros lista)
  (cond
    ((null? lista) #t)
    ((equal? (car lista) 0) (verifCeros (cdr lista)))
    (else #f)))

;;Retorna TRUE si la fila tiene solamente numeros > 0
(define (verifNum lista)
  (cond
    ((null? lista) #t)
    ((> (car lista) 0) (verifNum (cdr lista)))
    (else #f)))

;;Retorna una lista con el primer valor encontrado cambiado por un cero
(define (eliminaUnValor lista listaRespuesta)
  (cond
  ((null? lista) listaRespuesta)
  ((equal? (car lista) 0) (eliminaUnValor (cdr lista) (append listaRespuesta (list (car lista)))))
  (else (append listaRespuesta (list 0) (cdr lista)))))

;;Retorna la lista de candidatos renovada
(define (cambiarValorEspecifico fila columna lista listaRespuesta contador maxFilas)
  (cond
  ((null? lista) listaRespuesta)
  ((and (equal? fila maxFilas) (equal? (car lista) 0)) (append listaRespuesta (list (list 0 0)) (cdr lista)))
  ((and (equal? columna contador) (equal? (car lista) 0)) (append listaRespuesta (list (list fila columna)) (cdr lista)))
  (else (cambiarValorEspecifico fila columna (cdr lista) (append listaRespuesta (list (car lista))) (+ contador 1) maxFilas))))


(define (replace-value lst val cont maxFila)
  (cond ((null? lst) '())
        ((list? (car lst)) (cons (replace-value (car lst) val cont maxFila) (replace-value (cdr lst) val cont maxFila)))
        ((equal? (car lst) val) (cons (list maxFila cont) (replace-value (cdr lst) val (+ cont 1) maxFila)))
        (else (cons (car lst) (replace-value (cdr lst) val (+ cont 1) maxFila)))))

;;Reemplaza todos los 0 con (0 0)
(define (replace-all lst cont maxFila)
  (cond ((null? lst) '())
        ((list? (car lst)) (cons (replace-all (car lst) cont maxFila) (replace-all (cdr lst) cont maxFila)))
        (else (replace-value lst (car lst) cont maxFila))))

(replace-all '(0 0 0 0 (7 4) 0 0 0) 1 8)
;;---------------------

;;Funcion que retorna el valor de la posicion dada                                                                       
(define (Peso columna lista contColumna)
  (cond
    ((equal? columna contColumna) (CalculoPeso 0 (car lista)))
    (else (Peso columna (cdr lista) (+ 1 contColumna)))))

;;Función que retorna la matriz con una candidad de filas exactas inferiores
(define (moverMatriz matriz fila)
  (cond
    ((equal? fila 1) matriz)
    (else (moverMatriz (cdr matriz) (- fila 1)))))

;;Función que retorna la matriz con una candidad de filas exactas superiores
(define (moverMatrizSuperior matriz fila matrizNueva)
  (cond
    ((equal? fila 0) matrizNueva)
    (else (moverMatrizSuperior (cdr matriz) (- fila 1) (append matrizNueva (list (car matriz)))))))

;;Da un valor con respecto al valor dado en la funcion Peso
(define (CalculoPeso PesoTotal PesoDado)
  (cond
    ((equal? PesoDado 0) (+ PesoTotal 0))
    ((equal? PesoDado 1) (+ PesoTotal 8))
    ((equal? PesoDado 2) (+ PesoTotal 4))))

;;----------------------

;;Funcion que construye una nueva Matriz temporal, para analizar si la ficha hace un 4 en linea
(define (construirNuevaMatrizTemp fila columna nuevaFila matrizSup matrizInf matrizNueva contador)
  (cond
    ((equal? columna contador) (append matrizNueva matrizSup (list  (append nuevaFila (list 2) (cdr fila))) matrizInf))
    (else (construirNuevaMatrizTemp (cdr fila) columna (append nuevaFila (list (car fila)) ) matrizSup matrizInf matrizNueva (+ contador 1)))))


;;------------------------------

(define (FuncionSeleccionAux listaCandidatos mayor)
  (cond
    ((equal? mayor 1) (car listaCandidatos))
    (else (FuncionSeleccionAux (cdr listaCandidatos) (- mayor 1)))))