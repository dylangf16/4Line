#lang scheme

;funcion que construye una lista de 0s del tamano dado
(define (buildList list0 m)
  (cond ((equal? m 0)
         list0)
        (else
         (buildList (append list0 '(0)) (- m 1))
         )))

;funcion que construye una matriz de 0s de tamano n x m
(define (buildMatrix matrix n m)
  (cond ((equal? n 0)
         matrix)
        (else
         (buildMatrix (append matrix (list (buildList '() m))) (- n 1) m)
         )))

;funcion que reemplaza un valor en una posicion de la lista
(define (replaceInList list0 pos value count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (append (list value) (cdr list0)))
        (else
         (append (list (car list0)) (replaceInList (cdr list0) pos value (+ count 1)))
         )))

;reemplaza un valor en la posicion n x m en una matriz
(define (replaceInMatrix matrix n m value count)
  (cond ((null? matrix)
         null)
        ((equal? n count)
         (append (list (replaceInList (car matrix) m value 0)) (cdr matrix)))
        (else
         (append (list (car matrix)) (replaceInMatrix (cdr matrix) n m value (+ count 1))))
        ))

;Encuentra si hay 4 elementos seguidos en una lista
(define (4inLine_horizontal_aux list0 team count)
  (cond ((equal? count 4)
         #t)
        ((null? list0)
         #f)
        ((equal? (car list0) team)
         (4inLine_horizontal_aux (cdr list0) team (+ count 1)))
        (else
         (4inLine_horizontal_aux (cdr list0) team 0))))

;Encuentra si hay 4 elementos horizontales seguidos iguales en una lista
(define (4inLine_horizontal matrix team)
  (cond ((null? matrix)
         #f)
        ((4inLine_horizontal_aux (car matrix) team 0)
         #t)
        (else
         (4inLine_horizontal (cdr matrix) team))))

;Obtiene el elemento que se encuentra en la posicion dada en una lista
(define (getFromList list0 pos count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (car list0))
        (else
         (getFromList (cdr list0) pos (+ count 1)))))

;Convierte una columna de una matriz en una lista
(define (getColumn matrix column)
  (cond ((null? matrix)
         null)
        (else
         (append (list (getFromList (car matrix) column 0)) (getColumn (cdr matrix) column)))))

;Encuentra si hay 4 elementos iguales seguidos verticalmente
(define (4inLine_vertical_aux matrix team count)
  (cond ((null? (getFromList (car matrix) count 0))
         #f)
        ((4inLine_horizontal_aux (getColumn matrix count) team 0)
         #t)
        (else
         (4inLine_vertical_aux matrix team (+ count 1)))))
         
                                          


(define matriz '((1 2 3 4 5)
                 (1 3 3 3 3)
                 (1 6 3 9 0)
                 (1 1 3 1 3)
                 (5 5 5 1 0)))

(4inLine_vertical_aux matriz 1 0)

         

(reverse (car matriz))

