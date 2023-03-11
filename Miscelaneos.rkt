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


;;Input: matriz, columna donde revisar, 1,
;;La función se encarga de buscar la última fila sin fecha en la columna dada
;;Output: el número de fila que cumpla la condición
(define (encontrarFila matriz columna fila)
  (cond
    ((null? matriz) (- fila 1))
    ((>  (encontrarFilaAux (car matriz) columna) 0) (- fila 1))
    (else (encontrarFila (cdr matriz) columna (+ fila 1)))))

;;Función auxiliar de (encontrarFila)
(define (encontrarFilaAux lista columna)
  (cond
    ((null? lista) 0)
    ((equal? columna 1) (car lista))
    (else (encontrarFilaAux (cdr lista) (- columna 1)))))

;;Input: fila por analizar, 0
;;Función que retorna la columna del primer valor que se encuentra
;;Output: número de la última columna desocupada
(define (encontrarColumna lista columna)
  (cond
    ((null? lista) columna)
    ((equal? (car lista) 0) (encontrarColumna (cdr lista) (+ columna 1)))
    (else (+ columna 1))))

;;Input: lista por verificar
;;Retorna TRUE si la fila tiene solamente 0
;;Output: #t o #f
(define (verifCeros lista)
  (cond
    ((null? lista) #t)
    ((equal? (car lista) 0) (verifCeros (cdr lista)))
    (else #f)))

;;Input: lista por verificar
;;Retorna TRUE si la fila tiene solamente numeros > 0
;;Output: #t o #f
(define (verifNum lista)
  (cond
    ((null? lista) #t)
    ((> (car lista) 0) (verifNum (cdr lista)))
    (else #f)))

;;Input: lista por analizar, '()
;;Retorna una lista con el primer valor encontrado cambiado por un cero
;;Output: lista con el valor cambiado
(define (eliminaUnValor lista listaRespuesta)
  (cond
    ((null? lista) listaRespuesta)
    ((equal? (car lista) 0) (eliminaUnValor (cdr lista) (append listaRespuesta (list (car lista)))))
    (else (append listaRespuesta (list 0) (cdr lista)))))

;;Input: fila, columna, lista, '(), 1, máximo número de filas
;;Retorna la lista de candidatos renovada
;;Output: lista con los valores cambiados
(define (cambiarValorEspecifico fila columna lista listaRespuesta contador maxFilas)
  (cond
    ((null? lista) listaRespuesta)
    ((and (equal? fila maxFilas) (equal? (car lista) 0)) (append listaRespuesta (list (list 0 0)) (cdr lista)))
    ((and (equal? columna contador) (equal? (car lista) 0)) (append listaRespuesta (list (list fila columna)) (cdr lista)))
    (else (cambiarValorEspecifico fila columna (cdr lista) (append listaRespuesta (list (car lista))) (+ contador 1) maxFilas))))

;;Input: lista, máximo número de filas, 1, '()
;;Reemplaza todos los 0 con (maxFila cont)
;;Output: lista con valores cambiados
(define (replace-all lst maxFila cont listaReturn)
  (cond
    ((null? lst) listaReturn)
    ((equal? (car lst) 0) (replace-all (cdr lst) maxFila (+ cont 1)(append listaReturn (list (list maxFila cont)))))
    (else (replace-all (cdr lst) maxFila (+ cont 1) (append listaReturn (list (car lst)))))))
    

;;Input: número de columna, lista, 0
;;Calcula el peso de posición dada por la columna
;;Output: peso de la posición solicitada
(define (Peso columna lista contColumna)
  (cond
    ((null? lista) 0)
    ((equal? columna contColumna) (CalculoPeso 0 (car lista)))
    (else (Peso columna (cdr lista) (+ 1 contColumna)))))

;;Función auxiliar de (Peso)
(define (CalculoPeso PesoTotal PesoDado)
  (cond
    ((equal? PesoDado 0) (+ PesoTotal 0))
    ((equal? PesoDado 1) (+ PesoTotal 8))
    ((equal? PesoDado 2) (+ PesoTotal 4))))

;;Input: matriz, fila
;;Función que retorna la matriz con una candidad de filas exactas inferiores
;;Outpu: la matriz modificada
(define (moverMatriz matriz fila)
  (cond
    ((equal? fila 1) matriz)
    (else (moverMatriz (cdr matriz) (- fila 1)))))

;;Input: matriz, fila, '()
;;Función que retorna la matriz con una candidad de filas exactas superiores
;;Output: matriz modificada
(define (moverMatrizSuperior matriz fila matrizNueva)
  (cond
    ((equal? fila 0) matrizNueva)
    (else (moverMatrizSuperior (cdr matriz) (- fila 1) (append matrizNueva (list (car matriz)))))))

;;Input: fila, columna, nueva fila, parte superior de la matriz, parte inferior de la matriz, '() , 1
;;Funcion que construye una nueva Matriz temporal, para analizar si la ficha hace un 4 en linea
;;Output: una matriz temporal con valores cambiados
(define (construirNuevaMatrizTemp fila columna nuevaFila matrizSup matrizInf matrizNueva contador)
  (cond
    ((null? fila) (append matrizNueva matrizSup (list  (append nuevaFila (list 2) fila)) matrizInf))
    ((equal? columna contador) (append matrizNueva matrizSup (list  (append nuevaFila (list 2) (cdr fila))) matrizInf))
    (else (construirNuevaMatrizTemp (cdr fila) columna (append nuevaFila (list (car fila)) ) matrizSup matrizInf matrizNueva (+ contador 1)))))

;;Función auxiliar de (FuncionSeleccion)
(define (FuncionSeleccionAux listaCandidatos mayor)
  (cond
    ((equal? mayor 1) (car listaCandidatos))
    (else (FuncionSeleccionAux (cdr listaCandidatos) (- mayor 1)))))