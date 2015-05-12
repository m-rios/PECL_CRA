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

(define lista-2 ((construir dos) ((construir uno) vacia)))

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


(define miembro?
  (lambda (mem)
    (lambda (lista)
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                  (lambda (no_use) false)
                  ((lambda(no_use)
                    (((esigualent (cabeza l))m)
                     ;devolver true
                     (lambda (no_use) true)
                     ;devolver miembro cola
                     (lambda (no_use)
                       ((f (cola l))m)
                       )
                     ) zero)
                    )
                  ) zero))
               )
             )
           )mem)
       lista)
      )
    )
  )
((miembro? dos) lista-1)



;> (vacia? lista-1)