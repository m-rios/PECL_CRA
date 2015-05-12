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

;test de pertenencia_________________________________________________________________
(define miembro?
  (lambda (mem)
    (lambda (lista)
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                   ;fin de la lista, no se ha encontrado
                  (lambda (no_use) false)
                  (lambda(no_use)
                    ((((esigualent (cabeza l))m)
                     ;se ha encontrado
                     (lambda (no_use) true)
                     ;devolver miembro? m cola
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
;____________________________________________________________________________________


;> (vacia? lista-1)