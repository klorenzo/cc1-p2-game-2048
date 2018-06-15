(load "constantes.scm")

(define puntosAcumulados 0)
(define historial (list))

(define (crearMatriz filas columnas)
	(define matriz (make-vector filas))
	(do ((fila 0 (+ fila 1)))
		((= fila filas) matriz)
		(vector-set! matriz fila (make-vector columnas ""))
	)
)

(define (desplegarMatriz matriz)

	(define vectorFila)

	(define cantidadDeFilas (vector-length matriz))
	(define cantidadDeColumnas)

	(do ((fila 0 (+ fila 1)))

		((>= fila cantidadDeFilas) ())

		(set! vectorFila (vector-ref matriz fila))
		(set! cantidadDeColumnas (vector-length vectorFila))

		(do ((columna 0 (+ columna 1)))

			((>= columna cantidadDeColumnas) (newline))

			(cond
				((= columna 0)
					(do ((cont 0 (+ cont 1)))
						((>= cont cantidadDeColumnas) (display "+\n"))
						(display "+---------------")
					)
					(display (string "|\t" (vector-ref vectorFila columna) "\t|"))			
				)
				(else
					(display (string "\t" (vector-ref vectorFila columna) "\t|"))
				)
			)

		)

		(if (= fila (- cantidadDeFilas 1))
			(do ((cont 0 (+ cont 1)))
				((>= cont cantidadDeColumnas) (display "+\n"))
				(display "+---------------")
			)
		)

	)

)

(define (obtenerPosicionesDisponibles matriz)

	(define posicionesDisponibles (list))

	(define vectorFila)

	(define cantidadDeFilas (vector-length matriz))
	(define cantidadDeColumnas)

	(define valorDeLaPosicion)

	(do ((fila 0 (+ fila 1)))

		((>= fila cantidadDeFilas) ())

		(set! vectorFila (vector-ref matriz fila))
		(set! cantidadDeColumnas (vector-length vectorFila))

		(do ((columna 0 (+ columna 1)))

			((>= columna cantidadDeColumnas) ())

			(set! valorDeLaPosicion (vector-ref vectorFila columna))

			(if (and (string? valorDeLaPosicion) (string=? (string-trim valorDeLaPosicion) ""))
				(set! posicionesDisponibles (append posicionesDisponibles (list (cons fila columna))))
			)

		)

	)

	posicionesDisponibles

)

(define (obtenerNumerosPermitidos base)
	(define numerosPermitidos (vector base (+ base base)))
	numerosPermitidos
)

(define (generarNuevoValor base matriz)

	(define listaDePosicionesDisponibles (obtenerPosicionesDisponibles matriz))

	(define numero 0)
	(define posicion (cons 0 0))

	(if (>= (length listaDePosicionesDisponibles) 1)
		(begin
			(set! numero (vector-ref (obtenerNumerosPermitidos base) (random 2)))

			(set! posicion (list-ref listaDePosicionesDisponibles (random (length listaDePosicionesDisponibles))))

			(vector-set! (vector-ref matriz (car posicion)) (cdr posicion) numero)
		)
	)

)

(define (generarValoresIniciales base matriz)

	(define listaDePosicionesDisponibles (obtenerPosicionesDisponibles matriz))

	(define primerNumero (vector-ref (obtenerNumerosPermitidos base) (random 2)))
	(define segundoNumero (vector-ref (obtenerNumerosPermitidos base) (random 2)))

	(define primeraPosicion (cons 0 0))
	(define segundaPosicion (cons 0 0))

	(do ((ciclo 0 (+ ciclo 0)))
		((and (not (= (car primeraPosicion) (car segundaPosicion))) (not (= (cdr primeraPosicion) (cdr segundaPosicion)))) ())
		(set! primeraPosicion (list-ref listaDePosicionesDisponibles (random (length listaDePosicionesDisponibles))))
		(set! segundaPosicion (list-ref listaDePosicionesDisponibles (random (length listaDePosicionesDisponibles))))
	)

	(vector-set! (vector-ref matriz (car primeraPosicion)) (cdr primeraPosicion) primerNumero)
	(vector-set! (vector-ref matriz (car segundaPosicion)) (cdr segundaPosicion) segundoNumero)

)

(define (obtenerNumeroMayor matriz)

	(define numeroMayor 0)

	(define vectorFila)

	(define cantidadDeFilas (vector-length matriz))
	(define cantidadDeColumnas)

	(define valorDeLaPosicion)

	(do ((fila 0 (+ fila 1)))

		((>= fila cantidadDeFilas) ())

		(set! vectorFila (vector-ref matriz fila))
		(set! cantidadDeColumnas (vector-length vectorFila))

		(do ((columna 0 (+ columna 1)))

			((>= columna cantidadDeColumnas) ())

			(set! valorDeLaPosicion (vector-ref vectorFila columna))

			(if (and (number? valorDeLaPosicion) (> valorDeLaPosicion numeroMayor))
				(set! numeroMayor valorDeLaPosicion)
			)

		)

	)

	numeroMayor

)

(define (agregarAlHistorial matriz)

	(define cantidadDeFilas (vector-length matriz))
	(define cantidadDeColumnas (vector-length (vector-ref matriz 0)))

	(define elementoDelHistorial (crearMatriz cantidadDeFilas cantidadDeColumnas))

	(do ((fila 0 (+ fila 1)))

		((>= fila cantidadDeFilas) ())

		(do ((columna 0 (+ columna 1)))

			((>= columna cantidadDeColumnas) (newline))

			(vector-set! (vector-ref elementoDelHistorial fila) columna (vector-ref (vector-ref matriz fila) columna))

		)

	)

	(set! historial (append historial (list elementoDelHistorial)))

)

; aplicarMovimiento: Aplica los diferentes movimientos que están permitidos: Arriba, Abajo, Derecha e Izquierda.
; Parámetros: matriz ( Matriz en la que se aplicará el movimiento ) y direccion ( Movimiento que hizo el usuario: w, s, a, d ).

(define (aplicarMovimiento matriz direccion)

	(agregarAlHistorial matriz)

	(define cantidadDeFilas (vector-length matriz))
	(define cantidadDeColumnas (vector-length (vector-ref matriz 0)))

	(define valorDeLaPosicion)
	(define posicionesADesplazar 0)
	(define nuevaPosicion (cons 0 0))
	
	(define posicionAnterior (cons 0 0))
	(define valorDeLaPosicionAnterior "")
	(define posicionSiguiente (cons 0 0))
	(define valorDeLaPosicionSiguiente "")
	(define temp "")
	(define temp2 (cons 0 0))

	(cond
		((string-ci=? direccion ARRIBA)

			(do ((columna 0 (+ columna 1)))

				((= columna cantidadDeColumnas) ())

				(do ((fila 0 (+ fila 1)))

					((= fila cantidadDeFilas) ())

					(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))

					(if (number? valorDeLaPosicion)
						(begin

							(do ((fila2 0 (+ fila2 1)))

								((= fila2 fila) ())

								(if (not (number? (vector-ref (vector-ref matriz fila2) columna)))
									(set! posicionesADesplazar (+ posicionesADesplazar 1))
								)

							)

							(set! nuevaPosicion (cons (- fila posicionesADesplazar) columna))

							(vector-set! (vector-ref matriz fila) columna "")
							(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) valorDeLaPosicion)

							(do ((fila2 (+ (car nuevaPosicion) 1) (+ fila2 1)))

								((or (= fila2 cantidadDeFilas) (number? valorDeLaPosicionSiguiente)) ())

								(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz fila2) columna))
								(set! posicionSiguiente (cons fila2 columna))

							)

							(if (and (number? valorDeLaPosicionSiguiente) (= valorDeLaPosicion valorDeLaPosicionSiguiente))
								(begin
									(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) (+ valorDeLaPosicion valorDeLaPosicionSiguiente))
									(vector-set! (vector-ref matriz (car posicionSiguiente)) (cdr posicionSiguiente) "")
									(set! puntosAcumulados (+ puntosAcumulados (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
								)
							)

							(set! valorDeLaPosicionSiguiente "")

						)
					)

					(set! posicionesADesplazar 0)

				)
			)

		)
		((string-ci=? direccion ABAJO)

			(do ((columna 0 (+ columna 1)))

				((= columna cantidadDeColumnas) ())

				(do ((fila (- cantidadDeFilas 1) (- fila 1)))

					((< fila 0) ())

					(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))

					(if (number? valorDeLaPosicion)
						(begin

							(do ((fila2 fila (+ fila2 1)))

								((= fila2 cantidadDeFilas) ())

								(if (not (number? (vector-ref (vector-ref matriz fila2) columna)))
									(set! posicionesADesplazar (+ posicionesADesplazar 1))
								)

							)

							(set! nuevaPosicion (cons (+ fila posicionesADesplazar) columna))

							(vector-set! (vector-ref matriz fila) columna "")
							(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) valorDeLaPosicion)

							(do ((fila2 (- (car nuevaPosicion) 1) (- fila2 1)))

								((or (= fila2 -1) (number? valorDeLaPosicionAnterior)) ())

								(set! valorDeLaPosicionAnterior (vector-ref (vector-ref matriz fila2) columna))
								(set! posicionAnterior (cons fila2 columna))

							)

							(if (and (number? valorDeLaPosicionAnterior) (= valorDeLaPosicion valorDeLaPosicionAnterior))
								(begin
									(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) (+ valorDeLaPosicion valorDeLaPosicionAnterior))
									(vector-set! (vector-ref matriz (car posicionAnterior)) (cdr posicionAnterior) "")
									(set! puntosAcumulados (+ puntosAcumulados (+ valorDeLaPosicion valorDeLaPosicionAnterior)))
								)
							)

							(set! valorDeLaPosicionAnterior "")

						)
					)

					(set! posicionesADesplazar 0)

				)
			)

		)
		((string-ci=? direccion IZQUIERDA)

			(do ((fila 0 (+ fila 1)))

				((= fila cantidadDeFilas) ())

				(do ((columna 0 (+ columna 1)))

					((= columna cantidadDeColumnas) ())

					(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))

					(if (number? valorDeLaPosicion)
						(begin

							(do ((columna2 columna (- columna2 1)))

								((< columna2 0) ())

								(if (not (number? (vector-ref (vector-ref matriz fila) columna2)))
									(set! posicionesADesplazar (+ posicionesADesplazar 1))
								)

							)

							(set! nuevaPosicion (cons fila (- columna posicionesADesplazar)))

							(vector-set! (vector-ref matriz fila) columna "")
							(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) valorDeLaPosicion)

							(do ((columna2 (+ (cdr nuevaPosicion) 1) (+ columna2 1)))

								((or (= columna2 cantidadDeColumnas) (number? valorDeLaPosicionSiguiente)) ())

								(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz fila) columna2))
								(set! posicionSiguiente (cons fila columna2))

							)

							(if (and (number? valorDeLaPosicionSiguiente) (= valorDeLaPosicion valorDeLaPosicionSiguiente))
								(begin
									(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) (+ valorDeLaPosicion valorDeLaPosicionSiguiente))
									(vector-set! (vector-ref matriz (car posicionSiguiente)) (cdr posicionSiguiente) "")
									(set! puntosAcumulados (+ puntosAcumulados (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
								)
							)

							(set! valorDeLaPosicionSiguiente "")

						)
					)

					(set! posicionesADesplazar 0)

				)
			)

		)
		((string-ci=? direccion DERECHA)

			(do ((fila 0 (+ fila 1)))

				((= fila cantidadDeFilas) ())

				(do ((columna (- cantidadDeColumnas 1) (- columna 1)))

					((< columna 0) ())

					(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))

					(if (number? valorDeLaPosicion)
						(begin

							(do ((columna2 columna (+ columna2 1)))

								((= columna2 cantidadDeColumnas) ())

								(if (not (number? (vector-ref (vector-ref matriz fila) columna2)))
									(set! posicionesADesplazar (+ posicionesADesplazar 1))
								)

							)

							(set! nuevaPosicion (cons fila (+ columna posicionesADesplazar)))

							(vector-set! (vector-ref matriz fila) columna "")
							(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) valorDeLaPosicion)

							(do ((columna2 (- (cdr nuevaPosicion) 1) (- columna2 1)))

								((or (= columna2 -1) (number? valorDeLaPosicionAnterior)) ())

								(set! valorDeLaPosicionAnterior (vector-ref (vector-ref matriz fila) columna2))
								(set! posicionAnterior (cons fila columna2))

							)

							(if (and (number? valorDeLaPosicionAnterior) (= valorDeLaPosicion valorDeLaPosicionAnterior))
								(begin
									(vector-set! (vector-ref matriz (car nuevaPosicion)) (cdr nuevaPosicion) (+ valorDeLaPosicion valorDeLaPosicionAnterior))
									(vector-set! (vector-ref matriz (car posicionAnterior)) (cdr posicionAnterior) "")
									(set! puntosAcumulados (+ puntosAcumulados (+ valorDeLaPosicion valorDeLaPosicionAnterior)))
								)
							)

							(set! valorDeLaPosicionAnterior "")

						)
					)

					(set! posicionesADesplazar 0)

				) 
			)

		)
	)

)

; esPosibleContinuar: Verifica si aún se puede hacer un movimiento, devuelve #t si se puede, de lo contrario devuelve #f.
; Parámetros: matriz ( Representa el tablero que se verificará ).

(define (esPosibleContinuar matriz)

	(define resultado #f)

	(define cantidadDeFilas (- (vector-length matriz) 1))
	(define cantidadDeColumnas (- (vector-length (vector-ref matriz 0)) 1))

	(define valorDeLaPosicion)
	(define valorDeLaPosicionSiguiente)

	(do ((fila 0 (+ fila 1)))

		((> fila cantidadDeFilas) ())

		(do ((columna 0 (+ columna 1)))

			((= columna cantidadDeColumnas) ())

			(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))
			(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz fila) (+ columna 1)))

			(if (and (number? valorDeLaPosicion) (number? valorDeLaPosicionSiguiente))
				(if (= valorDeLaPosicion valorDeLaPosicionSiguiente)
					(begin
						(set! resultado #t)
						resultado
					)
				)
			)

		)

	)

	(do ((columna 0 (+ columna 1)))

		((> columna cantidadDeColumnas) ())

		(do ((fila 0 (+ fila 1)))

			((= fila cantidadDeFilas) ())

			(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))
			(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz (+ fila 1)) columna))

			(if (and (number? valorDeLaPosicion) (number? valorDeLaPosicionSiguiente))
				(if (= valorDeLaPosicion valorDeLaPosicionSiguiente)
					(begin
						(set! resultado #t)
						resultado
					)
				)
			)

		)

	)

	resultado

)

(define (calcularMovimiento matriz)

	(define resultado "")

	(define puntosArriba 0)
	(define puntosAbajo 0)
	(define puntosIzquierda 0)
	(define puntosDerecha 0)

	(define cantidadDeFilas (- (vector-length matriz) 1))
	(define cantidadDeColumnas (- (vector-length (vector-ref matriz 0)) 1))

	(define valorDeLaPosicion)
	(define valorDeLaPosicionSiguiente)

	(do ((fila 0 (+ fila 1)))

		((> fila cantidadDeFilas) ())

		(do ((columna 0 (+ columna 1)))

			((= columna cantidadDeColumnas) ())

			(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))
			(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz fila) (+ columna 1)))

			(if (and (number? valorDeLaPosicion) (number? valorDeLaPosicionSiguiente))
				(begin
					(if (= valorDeLaPosicion valorDeLaPosicionSiguiente)
						(begin
							(set! puntosDerecha (+ puntosDerecha (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
							(set! puntosIzquierda (+ puntosIzquierda (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
						)
					)
				)
			)

		)

	)

	(do ((columna 0 (+ columna 1)))

		((> columna cantidadDeColumnas) ())

		(do ((fila 0 (+ fila 1)))

			((= fila cantidadDeFilas) ())

			(set! valorDeLaPosicion (vector-ref (vector-ref matriz fila) columna))
			(set! valorDeLaPosicionSiguiente (vector-ref (vector-ref matriz (+ fila 1)) columna))

			(if (and (number? valorDeLaPosicion) (number? valorDeLaPosicionSiguiente))
				(begin
					(if (= valorDeLaPosicion valorDeLaPosicionSiguiente)
						(begin
							(set! puntosArriba (+ puntosArriba (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
							(set! puntosAbajo (+ puntosAbajo (+ valorDeLaPosicion valorDeLaPosicionSiguiente)))
						)
					)
				)
			)

		)

	)

	(cond
		((and (> puntosAbajo puntosIzquierda) (> puntosAbajo puntosDerecha))
			(set! resultado ABAJO)
		)
		((and (> puntosDerecha puntosArriba) (> puntosDerecha puntosAbajo))
			(set! resultado DERECHA)
		)
	)

	(if (string=? resultado "")
		(set! resultado (list-ref (list ARRIBA IZQUIERDA) (random 2)))
	)

	resultado

)