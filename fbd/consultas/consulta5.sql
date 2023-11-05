SELECT cliente_documento, (nombre || ' ' || apellido) AS nombre_completo 
FROM clientes cli 
WHERE EXISTS 
	(
	SELECT * 
	FROM estadias e 
	WHERE e.cliente_documento = cli.cliente_documento
		AND e.hotel_codigo IN 
		(
			SELECT hotel_codigo FROM hoteles ho 
			WHERE ho.pais_codigo IN
				(
					SELECT pais1_codigo
					FROM limitan
					WHERE pais2_codigo = cli.pais_codigo 
					AND pais1_codigo <> cli.pais_codigo 
				)
		)
	)