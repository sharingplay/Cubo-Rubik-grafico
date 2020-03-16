#lang racket
(require "CuboRubikGrafico.rkt")
(define (len lista)
  ;;Devuelve el tamano de una lista
  ;;El indice empieza en 1
  ;;Tomado del libro de Scheme
  (cond ((null? lista)0)
        (else( + 1 (len(cdr lista))))
        )
  )
(define (indice index lista)
  ;;Devuelve el elemento en una cierta posicion
  ;;Tomado del libro de Scheme
  (cond ((null? lista)#f)
        ((equal? index 1) (car lista))
        (else (indice (- index 1) (cdr lista)))
        )
  )
         
(define (RS X Cubo Movs)
 ;; Funcion principal
  (cond((and (validaEntradas? Cubo) (not (null? Movs)))
       (cond((equal? 'C (indice 1 (car Movs)))
            (cond ((equal? 'A (indice 3 (car Movs)))       
                  (RS X (unirLista(run(unirCaras(movimientos (cubo X Cubo '()) (indice 2 (car Movs)) #t #f X)'()) X) '())(cdr Movs))
                  ))
            (cond ((equal? 'B (indice 3 (car Movs)))
                  (RS X (unirLista(run(unirCaras(movimientos (cubo X Cubo '()) (indice 2 (car Movs)) #f #f X)'()) X) '())(cdr Movs))
                  ))
                   )
            )
       (cond((equal? 'F (indice 1 (car Movs)))
            (cond ((equal? 'D (indice 3 (car Movs)))       
                  (RS X (unirLista(run(unirCaras(movimientos (cubo X Cubo '()) (indice 2 (car Movs)) #t #t X)'()) X) '())(cdr Movs))
                  ))
            (cond ((equal? 'I (indice 3 (car Movs)))
                  (RS X (unirLista(run(unirCaras(movimientos (cubo X Cubo '()) (indice 2 (car Movs)) #f #t X)'()) X) '())(cdr Movs))
                  ))
                   )
            )
       )
  (else Cubo))
  )
(define (validaEntradas? cubo)
  (cond ((null? cubo)#t)
        (else(
              cond((or (equal? (car cubo) "white") (equal? (car cubo) "red") (equal? (car cubo) "orange") (equal? (car cubo) "blue") (equal? (car cubo) "yellow")
                  (equal? (car cubo) "green")) (validaEntradas? (cdr cubo)))
                   (else #f)))))
(define (movimientos cubo pos derArr fila tamano)
  ;;Evalúa el tipo de movimiento y llama a la funcion correspondiente
  (cond ((equal? #t fila)
         (filas cubo pos derArr tamano))
        ((equal? #f fila)
         (columnas cubo pos derArr tamano)
         )
        )
  )
(define (columnas cubo pos arriba tamano)
  (cond ((equal? #t arriba)
         (laterales cubo pos arriba tamano (append
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(car cubo) tamano '())) pos '() 1 (indice pos (transpuesta (dividirFilas (indice 6 cubo) tamano '()))))))
          (list(dividirFilas (indice 2 cubo) tamano '()))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 3 cubo) tamano '())) (- tamano (- pos 1)) '() 1 (reverse(indice (- tamano (- pos 1)) (reverse(transpuesta (dividirFilas (indice 5 cubo) tamano '()))))))))
          (list(dividirFilas (indice 4 cubo) tamano '()))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 5 cubo) tamano '())) pos '() 1 (indice pos (transpuesta (dividirFilas (indice 1 cubo) tamano '()))))))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 6 cubo) tamano '())) pos '() 1 (reverse(indice (- tamano (- pos 1)) (transpuesta (dividirFilas (indice 3 cubo) tamano '())))))))
          ))
         
        )
        ((equal? #f arriba)
         (laterales cubo pos arriba tamano (append
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(car cubo) tamano '())) pos '() 1 (indice pos (transpuesta (dividirFilas (indice 5 cubo) tamano '()))))))
          (list(dividirFilas (indice 2 cubo) tamano '()))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 3 cubo) tamano '())) (- tamano (- pos 1)) '() 1 (reverse(indice pos (transpuesta (dividirFilas (indice 6 cubo) tamano '())))))))
          (list(dividirFilas (indice 4 cubo) tamano '()))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 5 cubo) tamano '())) pos '() 1 (reverse(indice (- tamano (- pos 1)) (transpuesta (dividirFilas (indice 3 cubo) tamano '())))))))
          (list(transpuesta(reemplazar (transpuesta (dividirFilas(indice 6 cubo) tamano '())) pos '() 1 (indice pos (transpuesta (dividirFilas (indice 1 cubo) tamano '()))))))
          )))
         
              
  )
)
(define (laterales cuboIni pos arriba tamano cubo)
  (cond ((and (> pos 1) (< pos tamano))
         (reemplazar (reemplazar cubo 2 '() 1 (dividirFilas (indice 2 cuboIni) tamano '())) 4 '() 1 (dividirFilas (indice 4 cuboIni) tamano '()))
         )
        (else(
              cond ((equal? #t arriba)
                     (cond ((equal? pos 1)
                            (reemplazar (reemplazar cubo 2 '() 1 (dividirFilas (indice 2 cuboIni) tamano '())) 4 '() 1 (reverse(transpuesta (dividirFilas (indice 4 cuboIni) tamano '()))))
                            )
                           (else
                            (reemplazar (reemplazar cubo 2 '() 1 (transpuesta (reverse(indice 2 cubo)))) 4 '() 1 (dividirFilas (indice 4 cuboIni) tamano '())))))
                            
                     ((equal? #f arriba)
                      (cond ((equal? pos 1)
                             (reemplazar (reemplazar cubo 2 '() 1 (dividirFilas (indice 2 cuboIni) tamano '())) 4 '() 1 (transpuesta (reverse(indice 4 cubo)))))
                            (else
                             (reemplazar (reemplazar cubo 4 '() 1 (dividirFilas (indice 4 cuboIni) tamano '())) 2 '() 1 (reverse(transpuesta (dividirFilas (indice 2 cuboIni) tamano '())))))))                   )
             )
        )
              
   )
        
(define (filas cubo pos derecha tamano)
  ;;Movimiento de las filas
  ;;Si derecha es verdadero la direccion va hacia la derecha
  (cond ((equal? #t derecha)
               (append
                 (list(reemplazar (dividirFilas (car cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 4 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (cadr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 1 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (caddr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 2 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (cadddr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 3 cubo) tamano '()))))
                 (tapas cubo pos derecha tamano) 
                 ))
         ((equal? #f derecha)
                (append
                 (list(reemplazar (dividirFilas (car cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 2 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (cadr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 3 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (caddr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 4 cubo) tamano '()))))
                 (list(reemplazar (dividirFilas (cadddr cubo) tamano '()) pos '() 1 (indice pos (dividirFilas (indice 1 cubo) tamano '()))))
                 (tapas cubo pos derecha tamano) 
                 ))     
        )
  )
(define (tapas cubo pos derecha tamano)
  ;;Movimiento de las tapas segun el movimiento
  (cond ((and (> pos 1) (< pos tamano))
         (append
          (list(dividirFilas (indice 5 cubo) tamano '()))
          (list(dividirFilas (indice 6 cubo) tamano '())))
         
         )
        (else(
              cond ((equal? #t derecha)
                     (cond ((equal? pos 1)
                            (append (list(reverse(transpuesta (dividirFilas (indice 5 cubo) tamano '()))))
                                    (list(dividirFilas (indice 6 cubo) tamano '()))))
                           (else
                            (append (list(dividirFilas (indice 5 cubo) tamano '()))
                                     (list(transpuesta (reverse(dividirFilas (indice 6 cubo) tamano '()))))
                                     ))))
                            
                     ((equal? #f derecha)
                      (cond ((equal? pos 1)
                             (append (list(transpuesta (reverse(dividirFilas (indice 5 cubo) tamano '()))))
                                     (list(dividirFilas (indice 6 cubo) tamano '()))))
                            (else
                             (append (list(dividirFilas (indice 5 cubo) tamano '()))
                                    (list(reverse(transpuesta (dividirFilas (indice 6 cubo) tamano '()))))
                                    ))))
                   )
             )
        )
              
   )
(define (unirTodo cubo)
  (print (unirLista (unirCaras cubo '()) '()))
  (unirLista (unirCaras cubo '()) '())
  )
                
(define (unirCaras cubo result)
  ;; Funcion que une las filas de un cubo para formar caras
  (cond ((null? cubo) result)
        (else(
              unirCaras (cdr cubo) (append result (list(unirLista (car cubo) '()))) )
             )
        )
  )

                                
(define (unirLista lista result)
  ;;Funcion que une todos los elementos de una lista
  (cond((null? lista) result)
       (else(
             unirLista (cdr lista) (append result(car lista))))
       )
  )

(define (sacar1 mat)
  ;;Funcion auxiliar de transpuesta
  ;;Saca el primer elemento de una matriz
  (cond ((null? mat)'())
        (else(cons (car (car mat))
                   (sacar1 (cdr mat))))
        )
  )

(define (borrar1 mat)
  ;;Funcion auxiliar de transpuesta
  ;;Borra el primer elemento de una matriz
  (cond ((null? mat) '())
        (else( cons (cdr (car mat))
                    (borrar1 (cdr mat))))
        )
  )
(define (transpuesta mat)
  ;;Funcion que saca transpuesta de una matriz
  ;;Tomada del libro de Scheme
  (cond ((null? (car mat)) '())
        (else (cons (sacar1 mat)(transpuesta(borrar1 mat))))
        )
  )
                 
(define (reemplazar lista pos result cont valor)
  ;; Reemplaza elemento en una cierta posicion de una lista
  ;; La lista empieza en indice 0 si cont inicia en 0.
  (cond((null? lista)result)
       ((equal? pos cont) (reemplazar (cdr lista) pos (append result (list valor)) (+ 1 cont) valor))
       (else(reemplazar (cdr lista) pos (append result (list (car lista))) (+ 1 cont) valor))
       )
  )

                                       
(define (dividirFilas matriz tamano result)
  ;;Funcion que divide una cara en filas
  (cond ((null? matriz) (reverse result))
        (else(dividirFilas (cortarUltimos matriz tamano '() 0) tamano
                   (append (list(cortarPrimeros matriz tamano '() 0)) result)))
        )
  )

(define (cortarPrimeros lista tamano result cont)
  ;;Devuelve los primeros elementos de una lista dado un tamano
  (cond ((equal? cont tamano)(reverse result))
        (else( cortarPrimeros (cdr lista) tamano (cons (car lista) result) (+ 1 cont)))
        )
  )
(define (cortarUltimos lista tamano result cont)
  ;;Devuelve los ultimos elementos de una lista dado un tamano
  (cond ((null? lista) (reverse result))
        ((< cont tamano) (cortarUltimos (cdr lista) tamano result (+ 1 cont)))
        (else(cortarUltimos (cdr lista) tamano (cons (car lista) result) (+ 1 cont)))
       )
  )

(define (cubo tamano lista caras)
  ;;tamano es el tamano de filas y columnas
  ;;lista se refiere al cubo como una lista
  ;;caras es lo que devuelve, que sera el cubo separado por caras
  (cond ((null? lista) (reverse caras))
        (else(cubo tamano (cortarUltimos lista (* tamano tamano) '() 0)
                   (append (list (cortarPrimeros lista (* tamano tamano) '() 0)) caras)))
        )
  )