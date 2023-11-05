SELECT r.hotel_codigo, r.nro_habitacion, r.cliente_documento, r.fecha_reserva, r.check_in,
r.check_out, ch.precio_noche 
FROM reservas_anteriores_sin_precio r 
LEFT JOIN costos_habitacion ch
	ON ch.hotel_codigo = r.hotel_codigo
	AND ch.nro_habitacion = r.nro_habitacion
WHERE ch.fecha_desde = (
		SELECT MAX(fecha_desde) 
		FROM costos_habitacion co 
		WHERE co.hotel_codigo = r.hotel_codigo 
			AND co.nro_habitacion = r.nro_habitacion 
			AND co.fecha_desde <= r.check_in
		)