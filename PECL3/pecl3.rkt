#lang racket

(require racket/include)
(include "enteros.rkt")
(define vacia (lambda (x) x))

(define construir (lambda (x)
    (lambda (y)
        ((par false) ((par x) y)))))

(define vacia? (lambda (l)
    (primero l)))

(define cabeza (lambda (l)
    (primero (segundo l))))

(define cola (lambda (l)
    (segundo (segundo l))))

(define lista-0 vacia)

(define lista-1 ((construir uno) vacia))

(define lista-2 ((construir uno) ((construir dos) vacia)))

(define lista-3 ((construir -uno) ((construir dos) ((construir tres) ((construir -cinco) ((construir ocho) vacia))))))

(define lista-4 ((construir -uno) ((construir -ocho) ((construir -cinco) ((construir cinco) ((construir nueve) vacia))))))

;hay que implementar longitud
(define comprobar-lista (lambda (l)
    (if (= (comprobar (longitud l)) 0)
        '()
        (cons (testenteros (cabeza l)) (comprobar-lista (cola l))))))

(define longitud
    (lambda (l)
        ((Y (lambda (f)
            (lambda(x)
                (((vacia? x)
                    (lambda (no_use)
                        zero
                        )
                    (lambda (no_use)
                        ((sumnat un) (f (cola x)))
                        )
                    )
                zero) ;pasa zero como argumento de no_use
                )
            ))
        l) ;Pasa l como el valor inicial de x
        )
    )

;test de pertenencia____________________________________________________________
;recibe un elemento y una lista, y comprueba si el elemento existe en la lista
(define miembro?
  (lambda (mem)
    (lambda (lista)
        ;definición recursiva
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                   ;fin de la lista, no se ha encontrado
                  (lambda (no_use) false)
                  ;la lista no ha acabado
                  (lambda(no_use)
                    ((((esigualent (cabeza l))m)
                     ;se ha encontrado
                     (lambda (no_use) true)
                     ;llamada recursiva con la cola
                     (lambda (no_use)
                       ((f m) (cola l))
                       )
                     )zero)
                    )
                  )zero))
               )
             )
           )mem)
       lista)
      )
    )
  )
;unit test:
(quote false)
((miembro? dos) lista-0)
(quote true)
((miembro? dos) lista-2)
(quote false)
((miembro? dos) lista-1)


;máximo de una lista____________________________________________________________
;recibe una lista y devuelve su elemento máximo
(define maximo
    (lambda (lista)
      ((Y (lambda (f)
               (lambda (l)
                 (((vacia?  l)
                    ;si la lista es vacía se devuelve 0
                  (lambda (no_use)  zero)
                  (lambda(no_use)
                    (((vacia? (cola l))
                        ;si último elemento, se devuelve
                      (lambda (no_use) (cabeza l))
                      (lambda(no_use)
                        ((((esmenoroigualent (cabeza l)) (cabeza (cola l)))
                          ;si elemento 1 <= elemento2 llamada recursiva con cola
                          (lambda (no_use) (f (cola l)))
                          ;eliminamos el que no es máximo y llamada recursiva
                          (lambda (no_use)
                       (f ((construir (cabeza l)) (cola (cola l))))
                       )
                     )zero)
                    )
                  )zero)
                    )
                  )zero))
             )
           )
       lista)
      )
  )
;unit test:
(testenteros (maximo lista-2))

;minimo de una lista____________________________________________________________
(define minimo
    (lambda (lista)
      ((Y (lambda (f)
               (lambda (l)
                 (((vacia?  l)
                    ;si la lista es vacía se devuelve 0
                  (lambda (no_use)  zero)
                  (lambda(no_use)
                    (((vacia? (cola l))
                      (lambda (no_use) (cabeza l))
                      (lambda(no_use)
                        ((((esmayoroigualent (cabeza l)) (cabeza (cola l)))
                        ;si elemento 1 >= elemento2 llamada recursiva con cola
                          (lambda (no_use) (f (cola l)))
                          ;eliminamos el que no es mínimo y llamada recursiva
                          (lambda (no_use)
                       (f ((construir (cabeza l)) (cola (cola l))))
                       )
                     )zero)
                    )
                  )zero)
                    )
                  )zero))
             )
           )
       lista)
      )
  )

;sumar los elementos de una lista_______________________________________________
;recibe una lista y devuelve la suma de todos sus elementos
(define suma-lista
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
        ;si es vacía se devuelve 0
      (lambda (no_use) zero)
        ;si no, se suma el primer elemento con el resultado (recursivo) del
        ;resto de la lista
      (lambda (no_use) 
        ((sument (cabeza l)) (f (cola l)))))zero)
    )))lista)
    )
  )
;unit test
(quote 0)
(suma-lista lista-0)
(quote 1)
(testenteros (suma-lista lista-1))
(quote 3)
(testenteros (suma-lista lista-2))
;concatenar dos listas__________________________________________________________
;recibe dos listas y devuelve la concatenación de las mismas
(define concatenar
  (lambda (lista1)
    (lambda (lista2)
       (((Y
(lambda (f)
  (lambda (l1)
    (lambda(l2)
      (((vacia? l1)
       (lambda (no_use)
         (((vacia? l2)
            ;si las dos listas están vacias se devuelve vacío
          (lambda (no_use) vacia)
          ;si l1 vacia y l2 llena, se añade lo que queda de l2
          (lambda (no_use)
            ((construir
           (cabeza l2)) ((f l1) (cola l2)))
            )) zero)
         )
       ;si l1 llena se añade la cabeza y llamada recursiva con cola y l2
       (lambda (no_use)
         ((construir (cabeza l1)) ((f (cola l1)) l2))
         )) zero) ;zero -> no_use
      )))) lista1) lista2)
      )
    )
  )
;unit test
(comprobar-lista ((concatenar lista-3) lista-3))



;sumar dos listas (como vectores)_______________________________________________
;recibe 2 listas, y devuelve la lista que resulta de sumar cada elemento de l1 
;con su correspondiente en posición en l2
;si la longitud de las listas varía, devuelve la suma de la menor de las 2
(define sumar_listas
  (lambda (lista1)
    (lambda (lista2)
       (((Y
(lambda (f)
  (lambda (l1)
    (lambda(l2)
      (((vacia? l1)
        ;si l1 es vacía, se corta la recursión devolviendo vacío
       (lambda (no_use) vacia)
       (lambda (no_use)
         (((vacia? l2)
            ;si l2 es vacía se devuelve vacía
          (lambda (no_use) vacia)
          (lambda (no_use)
            ;se construye lista a partir de la suma de las cabezas y la llamada
            ;recursiva
            ((construir
           ((sument (cabeza l1)) (cabeza l2))) ((f (cola l1)) (cola l2)))
            )) zero)
         )) zero) ;zero -> no_use
      )))) lista1) lista2)
      )
    )
  )
;unit test
(list 2)
(comprobar-lista ((sumar_listas lista-1) lista-1))
(list 2 4)
(comprobar-lista ((sumar_listas lista-2) lista-2))
(define lista-n ((construir tres) ((construir cinco) vacia)))
(list 4 7)
(comprobar-lista ((sumar_listas lista-2) lista-n))
;inverso lista__________________________________________________________________
;recibe una lista y devuelve la misma en orden inverso
(define inversa
  (lambda (lista)
    ((Y
      (lambda (f)
        (lambda (l)
          (((vacia? l)
            ;si está vacía, devuelve vacía
            (lambda (no_use) vacia)
            (lambda (no_use)
                ;se concatena la llamada recursiva con la cabeza actual
                ;(en orden inverso al normal)
              ((concatenar (f (cola l))) ((construir (cabeza l))vacia))
              )
            ) zero)
          )
        )
      ) lista)
    )
  )
(list 8 5 3 2 1)
(comprobar-lista (inversa lista-3))
(comprobar-lista (inversa lista-2))
(comprobar-lista (inversa lista-0))

;borrado en lista_______________________________________________________________
;recibe un elemento y una lista, y devuelve otra lista con el elemento eliminado
(define borrar
  (lambda (mem)
    (lambda (lista)
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                  (lambda (no_use) vacia)
                  (lambda(no_use)
                    ((((esigualent (cabeza l))m)
                        ;si la cabeza es lo que se busca, se devuelve la lista
                        ;'saltando' (borrando) la cabeza
                     (lambda (no_use) (cola l))
                     (lambda (no_use)
                       ((construir (cabeza l)) ((f m) (cola l)))
                       )
                     )zero)
                    )
                  )zero))
               )
             )
           )mem)
       lista)
      )
    )
  )
  
;Ordenación mayor a menor en lista______________________________________________
;recibe una lista y devuelve la misma ordenada de mayor a menor
;para ello, va concatenando recursivamente el mayor con la lista de retorno,
;y eliminando el mayor de la lista de entrada
(define mayoramenor
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
      (lambda (no_use) vacia)
      (lambda (no_use) 
        ((construir (maximo l)) (f ((borrar (maximo l))l )) )))zero)
    )))lista)
    )
  )
  
;Unit test
(comprobar-lista (mayoramenor lista-4))

;Ordenación menor a mayor en lista______________________________________________
;recibe una lista y devuelve la misma ordenada de menor a mayor
;para ello, va concatenando recursivamente el menor con la lista de retorno,
;y eliminando el menor de la lista de entrada
(define menoramayor
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
      (lambda (no_use) vacia)
      (lambda (no_use) 
        ((construir (minimo l)) (f ((borrar (minimo l))l )) )))zero)
    )))lista)
    )
  )

;Unit test
(comprobar-lista (menoramayor lista-4))

