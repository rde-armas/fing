CREATE OR REPLACE FUNCTION ingreso_extra(codhotel integer, out tipohab smallint, out monto numeric(8,2))
RETURNS setof record AS
$$
DECLARE
    resultado record;
BEGIN
	FOR resultado IN
		SELECT DISTINCT K.tipo_habitacion_codigo ,SUM((A.check_out-A.check_in)*C.precio_noche) AS monto_por_tipo
		FROM estadias_anteriores A NATURAL JOIN habitaciones K 
		NATURAL JOIN costos_habitacion C 
		WHERE A.hotel_codigo = codhotel
		AND NOT EXISTS (SELECT 1 FROM reservas_anteriores T 
						WHERE T.hotel_codigo = A.hotel_codigo
						AND T.nro_habitacion = A.nro_habitacion
						AND T.check_in = A.check_in)
		AND C.precio_noche = (SELECT precio_noche FROM costos_habitacion co 
			WHERE co.hotel_codigo = A.hotel_codigo 
			AND co.nro_habitacion = A.nro_habitacion 
			AND co.fecha_desde <= A.check_in
			AND NOT EXISTS (SELECT * FROM costos_habitacion u 
							WHERE u.fecha_desde<= A.check_in
							AND u.fecha_desde > co.fecha_desde
							AND u.hotel_codigo = co.hotel_codigo
							AND u.nro_habitacion = co.nro_habitacion)
					   )
		GROUP BY K.tipo_habitacion_codigo
	LOOP
		tipohab := resultado.tipo_habitacion_codigo;
		monto := resultado.monto_por_tipo;
		RETURN NEXT;
	END LOOP;
END;
$$
LANGUAGE plpgsql;
