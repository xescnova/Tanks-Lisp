;;Francisco José Muñoz Navarro
;;Francesc Nova Prieto

;;FUNCIONALIDAD : Pinta un segmento del fondo de un color u otro dependiendo de los siguientes parametros:
;; x pixeles hay que pintar en el eje x
;; y altitud del eje y si hay que cambiar de color entre azul y verde
;; z indica en que posicion estamos actualmente sobre el eje y0
;; w  indica el la posicion en el eje x estamos

(defun Pintarfondo (x y z w)

(move w z)
( cond ((> z y ) (color 0 0 255 255 255 255))
       ((< z y ) (color 0 143 57 255 255 255))
       )
(drawrel x 0)
( cond ((< z 0) )
       (t (Pintarfondo x y (- z 1) w)))

)

;;FUNCIONALIDAD : Pinta un tanque sobre el fondo. Los parametros son identicos a los de la funcion Pintarfondo

(defun Pintartanque (x y z w)

(move w z)
(color 128 64 0 255 255 255)
(drawrel x 0)
       ( cond ((< z y) )
       (t (Pintartanque x y (- z 1) w)))

)

;;FUNCIONALIDAD: Pinta el cañon de un tanque
;; x e y nos indican la posicion en el eje x y en el eje y respectivamente
;; angulo nos indica el angulo con el pintar el cañon
;; limpiar nos indica el color con el que pintamos el cañon del tanque si en 0 lo pintamos de marron como el tanque y si es 1 lo pintamos de azul como el fondo,
;;esto tiene utilidad para borrar el cañon y volverlo a pintar segun otro angulo

(defun PintarCano (x y angulo limpiar) 

(move x y )
(cond ((= limpiar 0) (color 128 64 0 255 255 255) )
      ((= limpiar 1) (color 0 0 255 255 255 255) ) 
)
(drawrel  (realpart (round(* (cos (AnguloRadian angulo))15)))  (realpart (round(* (sin (AnguloRadian angulo) ) 15))) )

)

;;FUNCIONALIDAD : Convierte un angulo de grados a radianes
(defun AnguloRadian (x)
(* x 0.0174533)
)


;;FUNCIONALIDAD : Dibuja una flecha si la fuerza del viento no es 0
;; Si hay viento llama a las funciones dibujar flecha según la fuerza del viento
(defun flechaViento()
(move 20 300)
(color 0 0 0 255 255 255) 
(cond ((= 0 (get 'escenari 'fuerzaViento))  )
       ((> 0 (get 'escenari 'fuerzaViento))(pintarFlechaIzq) )
       ((< 0 (get 'escenari 'fuerzaViento))(pintarFlechaDer))
))


;;FUNCIONALIDAD : Dibuja ">" en la flecha de viento
(defun pintarFlechaDer()
        (drawrel 40 0)
        (move 60 300)
         (drawrel -10 10)
         (move 60 300)
         (drawrel -10 -10)
        ( pintarFDer 20 300 (- (get 'escenari 'fuerzaViento) 1 ))
)
;;FUNCIONALIDAD : Dibuja "<" en la flecha de viento
(defun pintarFlechaIzq()
        (drawrel 40 0)
        (move 20 300)
         (drawrel 10 10)
         (move 20 300)
         (drawrel 10 -10)
       (pintarFIzq 60 300 (+ (get 'escenari 'fuerzaViento) 1))
)
;;FUNCIONALIDAD : Dibuja la cantidad de fuerza del viento hacia la derecha
(defun pintarFDer(x y f)
       (move x y)
       (drawrel 0 -10)
       (cond ((= 0 f) )
       ((> f 0) (pintarFDer (+ x 5) y (- f 1))
       )
       )
)
;;FUNCIONALIDAD :Dibuja la cantidad de fuerza del viento hacia la izquierda
(defun pintarFIzq(x y f)
       (move x y)
       (drawrel 0 -10)
       (cond ((= 0 f) )
       ((< f 0) (pintarFIzq(- x 5) y (+ 1 f))
       )
       )

)

;;FUNCIONALIDAD: Funcion principal de dibujo. Nos dibuja un escenario aleatorio para los tanques ademas de poner los tanques aleatoriamente.

(defun pinta ()
(cls)

;;Primer segmento aleatorio del escenario 

(putprop 'escenari ( + (random 300) 100) 'x1)
(putprop 'escenari (random 300) 'y2)
(Pintarfondo (get 'escenari 'x1) (get 'escenari 'y2) 400 0)

;;Segundo segmento del escenario

(putprop 'escenari ( + (random 150) 20) 'x2)
(putprop 'escenari ( + (random (- 300 (get 'escenari 'y2)))  (get 'escenari 'y2)) 'y1)
(Pintarfondo (get 'escenari 'x2) (get 'escenari 'y1) 400 (get 'escenari 'x1))

;;Tercer segmento del escenario

(putprop 'escenari (- 640 (+ (get 'escenari 'x1) (get 'escenari 'x2)) ) 'x3)
(putprop 'escenari (random (get 'escenari 'y1)) 'y3)
(Pintarfondo (get 'escenari 'x3) (get 'escenari 'y3) 400 (+ (get 'escenari 'x1) (get 'escenari 'x2)))

;;Pintamos el primer tanque 

(putprop 'cano (random (- (get 'escenari 'x1) 20))'x )
(putprop 'cano (get 'escenari 'y2) 'y )
(Pintartanque 20 (get 'cano 'y) (+ (get 'cano 'y) 10) (get 'cano 'x))
(putprop 'cano 45 'angulo)
(PintarCano (+ (get 'cano 'x) 10) (+(get 'cano 'y) 10) (get 'cano 'angulo) 0 )
;;Pintamos el segundo tanque 

(putprop 'cano2 (+(random (-(- 640 (+ (get 'escenari 'x1) (get 'escenari 'x2))) 20 ) ) (+ (get 'escenari 'x1) (get 'escenari 'x2))) 'x )
(putprop 'cano2 (get 'escenari 'y3) 'y )
(Pintartanque 20 (get 'cano2 'y) (+ (get 'cano2 'y) 10) (get 'cano2 'x))
(putprop 'cano2 135 'angulo)
(PintarCano (+ (get 'cano2 'x) 10) (+(get 'cano2 'y) 10) (get 'cano2 'angulo) 0 )


;;Calculamos la fuerza del viento y pintamos su flecha
(putprop 'escenari (- (random 10) 5) 'fuerzaViento)
(flechaViento)

(goto-xy 0 0)
(cleol)
(color 0 0 0 255 255 255)  

(repetir (eval (read)))
)

;;FUNCIONALIDAD: Sube el cañon de un tanque (variable tanque) en x grados (variable subir)

(defun puja (tanque subir)

(PintarCano (+ (get tanque 'x) 10) (+(get tanque 'y) 10) (get tanque 'angulo) 1 )
(putprop tanque (+ subir (get tanque 'angulo)) 'angulo)
(PintarCano (+ (get tanque 'x) 10) (+(get tanque 'y) 10) (get tanque 'angulo) 0 )

(goto-xy 0 0)
(cleol)
(color 0 0 0 255 255 255)  
(repetir (eval (read)))
)

;;FUNCIONALIDAD: Baja el cañon de un tanque (variable tanque) en x grados (variable bajar)


(defun baixa (tanque baixa)

(PintarCano (+ (get tanque 'x) 10) (+(get tanque 'y) 10) (get tanque 'angulo) 1 )
(putprop tanque (-(get tanque 'angulo) baixa ) 'angulo)
(PintarCano (+ (get tanque 'x) 10) (+(get tanque 'y) 10) (get tanque 'angulo) 0 )

(goto-xy 0 0)
(cleol)
(color 0 0 0 255 255 255)  
(repetir (eval (read)))
)

;;FUNCIONALIDAD: Funcion que simula el lanzamineto de un proyectil de un tanque 

(defun simula (tanque velocidad) 
(move (+ (get tanque 'x) 10 ) (+ (get tanque 'y) 10 )  )  
(trayectoria tanque velocidad 0)

(goto-xy 0 0)
(cleol)
(color 0 0 0 255 255 255) 
(repetir (eval (read)))
)

;;FUNCIONALIDAD: Funcion que calcula la trayectoria de un proyectil recursivamente dado un tanque, una velocidad y un tiempo, ademas de calcular las colisiones para el fondo y el tanque

(defun trayectoria (tanque velocidad tiempo)
(color 0 0 0 255 255 255) 

;;Calculamos la posicion del proyectil y la dibujamos

(putprop 'proyectil (+ (realpart (round (* tiempo (+ (* (cos (AnguloRadian (get tanque 'angulo))) velocidad ) (* (get 'escenari 'fuerzaViento) tiempo))))) (+(+ (get tanque 'x) 10 ) (realpart (round(* (cos (AnguloRadian (get tanque 'angulo)))15))))) 'x)
(putprop 'proyectil (+ (realpart (round(+    (* (+     (* velocidad (sin (AnguloRadian (get tanque 'angulo))) ) (* -9.8 tiempo)) tiempo) (* -4.9 (* tiempo tiempo)))) )  (+(+ (get tanque 'y) 10 ) (realpart (round(* (sin (AnguloRadian (get tanque 'angulo)))15))))) 'y)
(draw (get 'proyectil 'x) (get 'proyectil 'y))

;;Ahora miramos las colisiones para el proyectil, si choca con untaque dibujaremos una explosion y  si toca con el fondo dejaremos de hacer la recursividad 

(cond ((and (>= (get 'proyectil 'x) (get 'cano 'x) ) (<= (get 'proyectil 'x) (+(get 'cano 'x) 20)) (>= (get 'proyectil 'y) (get 'cano 'y) ) (<= (get 'proyectil 'y) (+(get 'cano 'y) 10 )))(explosion))
      ((and (>= (get 'proyectil 'x) (get 'cano2 'x) ) (<= (get 'proyectil 'x) (+(get 'cano2 'x) 20)) (>= (get 'proyectil 'y) (get 'cano2 'y) ) (<= (get 'proyectil 'y) (+(get 'cano2 'y) 10)))(explosion))
     
      ((and (< (get 'proyectil 'x) (get 'escenari 'x1))(> (get 'proyectil 'y) (get 'escenari 'y2))) (trayectoria tanque velocidad (+ tiempo 0.05)))
      ((and (>= (get 'proyectil 'x) (get 'escenari 'x1))(<= (get 'proyectil 'x) (+(get 'escenari 'x1) (get 'escenari 'x2)))(> (get 'proyectil 'y) (get 'escenari 'y1)))(trayectoria tanque velocidad (+ tiempo 0.05)))
      ((and (> (get 'proyectil 'x) (+ (get 'escenari 'x1) (get 'escenari 'x2)))(< (get 'proyectil 'x) 640)(> (get 'proyectil 'y) (get 'escenari 'y3)))(trayectoria tanque velocidad (+ tiempo 0.05)))
)

)

;;FUNCIONALIDAD: llama a una funcion que dibuja una explosión segun donde haya impactado el proyectil
(defun explosion ()
(dibujarExplosion (get 'proyectil 'x) (get 'proyectil 'y) 15 0)

)

;;FUNCIONALIDAD:dibuja un circulo en la posición x0 y0 de radio radio
(defun dibujarExplosion (x0 y0 radio angulo )

        (move x0 y0)
        (color 255 0 0 255 255 255) 
        (drawrel  (realpart (round(* (cos (AnguloRadian angulo))radio)))  (realpart (round(* (sin (AnguloRadian angulo) ) radio))) )
       (cond ((<= angulo 360)(dibujarExplosion x0 y0 radio (+ 1 angulo)))
       
       )
)


(defun repetir (x) (print x) (repetir (eval (read))))
