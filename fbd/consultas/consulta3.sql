SELECT r.hotel_codigo , r.nro_habitacion, r.cliente_documento,r.fecha_reserva,
		r.check_in, es.check_out
FROM estadias_anteriores es JOIN reservas_anteriores r 
	ON es.hotel_codigo   = r.hotel_codigo
	AND es.nro_habitacion = r.nro_habitacion
	AND es.check_in = r.check_in
	WHERE r.fecha_reserva = (
		SELECT MAX(F.fecha_reserva) 
		FROM reservas_anteriores F 
			WHERE es.hotel_codigo = F.hotel_codigo 
				AND es.nro_habitacion = F.nro_habitacion 
				AND es.check_in = F.check_in		
		)
		AND es.check_out = (
			SELECT MAX(F.check_out) 
			FROM estadias_anteriores F 
				WHERE es.hotel_codigo = F.hotel_codigo 
					AND es.nro_habitacion = F.nro_habitacion 
					AND es.check_in = F.check_in		
			)