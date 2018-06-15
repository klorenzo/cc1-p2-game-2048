(load "modos.scm")

(define (main)

	(clear)

	(newline)

	(display "=====================================================================================")(newline)
	(display "::: Bienvenido al Juego 2048 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::")(newline)
	(display "=====================================================================================")(newline)
	(display "                                                                                     ")(newline)
	(display " Integrantes:                                                                        ")(newline)
	(display " - Kevin Lorenzo                                                                     ")(newline)
	(display "                                                                                     ")(newline)
	(display "=====================================================================================")(newline)

	(newline)

	(define base 0)
	(define filas 0)
	(define columnas 0)

	(do ((ciclo 0 (+ ciclo 0)))
		((and (exact-integer? base) (>= base 2) (<= base 9)) ())
		(display "Por favor, elija la base con la que se jugará ( Número entre 2 y 9 ): ")
		(set! base (string->number (read-line)))
	)

	(display "\nDimensiones del Tablero:")(newline)(newline)

	(do ((ciclo 0 (+ ciclo 0)))
		((and (exact-integer? filas) (>= filas 2)) ())
		(display " - Filas ( Mayor o Igual a 2 ): ")
		(set! filas (string->number (read-line)))
	)

	(do ((ciclo 0 (+ ciclo 0)))
		((and (exact-integer? columnas) (>= columnas 2)) ())
		(display " - Columnas ( Mayor o Igual a 2 ): ")
		(set! columnas (string->number (read-line)))
	)

	(newline)

	(display "----------------------------------------")(newline)
	(display "::: Menú Principal :::::::::::::::::::::")(newline)
	(display "----------------------------------------")(newline)
	(display ": 1).- Modo Jugador                    :")(newline)
	(display ": 2).- Modo Máquina                    :")(newline)
	(display ": 3).- Salir                           :")(newline)
	(display "----------------------------------------")(newline)

	(newline)

	(define matriz (crearMatriz filas columnas))

	(define (menuPrincipal)

		(display "Elija una opción del Menú Principal: ")
		(define opcion (string->number (read-line)))

		(cond
			((and (number? opcion) (= opcion 1))
				(generarValoresIniciales base matriz)
				(mostrarJuego MODO_JUGADOR base matriz 0 "")
			)
			((and (number? opcion) (= opcion 2))
				(generarValoresIniciales base matriz)
				(mostrarJuego MODO_MAQUINA base matriz 0 "")
			)
			((and (number? opcion) (= opcion 3))
				(display "\nSaliendo...")
				(display "\nGracias por jugar nuestro juego.\n")
			)
			(else
				(display "¡OPCIÓN NO VÁLIDA! ")
				(menuPrincipal)
			)
		)

	)(menuPrincipal)

	(newline)
	(newline)

)(main)
