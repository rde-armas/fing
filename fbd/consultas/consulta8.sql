SELECT cli.cliente_documento, (cli.nombre || ' ' || cli.apellido) AS nombre_completo, 
fecha_nacimiento 
FROM clientes cli
JOIN estadias es 
ON cli.cliente_documento = es.cliente_documento 
WHERE NOT EXISTS 
	(
	SELECT * 
	FROM continentes con
	WHERE con.continente_codigo NOT IN
		(
			SELECT pa.continente_codigo FROM paises pa
			JOIN hoteles h ON pa.pais_codigo = h.pais_codigo 
			WHERE h.hotel_codigo = es.hotel_codigo
		)
	)