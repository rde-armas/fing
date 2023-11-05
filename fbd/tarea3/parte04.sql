CREATE OR REPLACE FUNCTION  control_costos_proc()
	RETURNS TRIGGER AS $$
    DECLARE hay_estadia integer;
	BEGIN
		--Si es UPDATE y no se modifica el hotel ni la habitacion, y no se modifica la fecha o
		--se modifica a una menor, no hace falta hacer ningun chequeo
		IF TG_OP = 'UPDATE' AND (OLD.hotel_codigo = NEW.hotel_codigo 
			AND OLD.nro_habitacion = NEW.nro_habitacion
			AND OLD.fecha_desde >= NEW.fecha_desde)
		THEN
			RETURN NEW;
		ELSE
			--Si es DELETE o UPDATE pero se modifica algo de la clave, tengo que hacer todo el 
			--chequeo: verificar si hay estadías asociadas al registro que se intentó modificar
			--o eliminar, para las cuales ademas NO exista otro registro costo_habitacion con fecha 
			--anterior a la que se cambia/elimina, mismo hotel, y misma habitacion
			SELECT DISTINCT COUNT(*) INTO hay_estadia
			FROM estadias_anteriores E
			WHERE E.hotel_codigo = OLD.hotel_codigo
			AND E.nro_habitacion = OLD.nro_habitacion
			AND E.check_in >= OLD.fecha_desde
			AND NOT EXISTS(SELECT 1 FROM costos_habitacion C 
						   WHERE C.hotel_codigo = OLD.hotel_codigo
						   AND C.nro_habitacion = OLD.nro_habitacion
						   AND C.fecha_desde > OLD.fecha_desde
						   AND C.fecha_desde <= E.check_in)
			AND NOT EXISTS(SELECT 1 FROM costos_habitacion D
						   WHERE D.hotel_codigo = E.hotel_codigo
						   AND D.nro_habitacion = E.nro_habitacion
						   AND D.fecha_desde < E.check_in
						   AND D.fecha_desde < OLD.fecha_desde);
			--Si hay alguna estadia, no se puede hacer la operacion
			IF hay_estadia>0 AND TG_OP = 'UPDATE' THEN
				 RAISE NOTICE 'La actualización no es correcta';
				 RETURN NULL;
			END IF;
			IF hay_estadia>0 AND TG_OP = 'DELETE' THEN
				RAISE NOTICE 'La operación de borrado no es correcta';
				RETURN NULL;
			END IF;
		END IF;
	END;	
$$ LANGUAGE plpgsql;

CREATE TRIGGER control_costos
BEFORE DELETE OR UPDATE ON costos_habitacion
FOR EACH ROW
EXECUTE FUNCTION control_costos_proc();