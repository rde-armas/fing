SELECT o.ocupacion_codigo, o.descripcion, ROUND(AVG(r.check_out-r.check_in),2) as estadia_promedio 
FROM ocupaciones o 
JOIN clientes cli
	ON o.ocupacion_codigo = cli.ocupacion_codigo
JOIN estadias e
	ON e.cliente_documento = cli.cliente_documento
JOIN  reservas r
	ON r.cliente_documento = cli.cliente_documento
	AND e.hotel_codigo = r.hotel_codigo
    AND e.nro_habitacion = r.nro_habitacion
    AND e.check_in = r.check_in
GROUP BY o.ocupacion_codigo, o.descripcion