CREATE TABLE IF NOT EXISTS finguitos_usuarios (
    cliente_documento integer,
    hotel_codigo integer,
    check_in date,
    check_out date,
    fecha_inicio date,
    fecha_fin date,
    finguitos int,
    fecha_operacion timestamp,
	estado smallint,
	PRIMARY KEY (cliente_documento, hotel_codigo, check_in)
);

CREATE OR REPLACE FUNCTION actualizar_finguitos() RETURNS TRIGGER AS $$ 
DECLARE
	fecha_inicio_aux date;
	fecha_fin_aux date;
	finguitos_aux int;
	precio_aux numeric(5,2);
	estado_aux smallint;
	old_fecha_operacion timestamp;
BEGIN
	IF TG_OP = 'DELETE' THEN
			--si existe un registro para la estadia que se borró, le actualizo el estado de los finguitos
		old_fecha_operacion := NULL;
		old_fecha_operacion = (SELECT fecha_operacion FROM finguitos_usuarios
			  WHERE cliente_documento = OLD.cliente_documento
			  AND hotel_codigo = OLD.hotel_codigo
			  AND check_in = OLD.check_in);
		IF old_fecha_operacion IS NOT NULL THEN		  
			UPDATE finguitos_usuarios
			SET estado = 3, fecha_operacion = current_timestamp
			WHERE cliente_documento = OLD.cliente_documento
		  	AND hotel_codigo = OLD.hotel_codigo
		  	AND check_in = OLD.check_in;
			--si existen estadias que se registraron despues de la que se borró,
			--para el mismo cliente y hotel, y la que se borró fue la primera
			-- con esas condiciones, se le sumaron 5 finguitos a esas que vinieron despues,
			--que ahora se le deben restar
			IF EXISTS (SELECT 1 FROM finguitos_usuarios
						WHERE cliente_documento = OLD.cliente_documento
						AND hotel_codigo = OLD.hotel_codigo
						AND fecha_operacion > old_fecha_operacion
					  	AND check_in <> OLD.check_in)
			THEN
				UPDATE finguitos_usuarios
				SET finguitos = finguitos - 5,
    			fecha_operacion = current_timestamp,
				old_fecha = old_fecha_operacion
				WHERE cliente_documento = OLD.cliente_documento
  				AND hotel_codigo = OLD.hotel_codigo
				AND estado = 1
  				AND fecha_operacion > old_fecha_operacion
  				AND NOT EXISTS (
					SELECT 1 FROM finguitos_usuarios C
					WHERE C.cliente_documento = finguitos_usuarios.cliente_documento
					  AND C.hotel_codigo = finguitos_usuarios.hotel_codigo
					  AND C.fecha_operacion < finguitos_usuarios.fecha_operacion);
			END IF;
		END IF;
	ELSIF TG_OP = 'INSERT' THEN
		fecha_inicio_aux = NEW.check_in + interval '1 month';
		fecha_fin_aux = NEW.check_out + interval '2 year';
		finguitos_aux = 0;
		--calculo los finguitos
		precio_aux = (SELECT precio_noche FROM costos_habitacion A
					WHERE A.hotel_codigo = NEW.hotel_codigo
					AND A.nro_habitacion = NEW.nro_habitacion
					AND NOT EXISTS (SELECT 1 FROM costos_habitacion C
								   WHERE A.hotel_codigo = C.hotel_codigo
								   AND A.nro_habitacion = C.nro_habitacion
								   AND A.fecha_desde < C.fecha_desde));
		finguitos_aux = trunc(((NEW.check_out - NEW.check_in)*precio_aux)/10);
		IF EXISTS (SELECT 1 FROM estadias_anteriores 
				  WHERE cliente_documento = NEW.cliente_documento
				  AND hotel_codigo = NEW.hotel_codigo
				  AND check_in <> NEW.check_in)
		THEN
			finguitos_aux = finguitos_aux + 5;
		END IF;		
		IF fecha_fin_aux >= current_timestamp THEN
			estado_aux = 1;
		ELSE
			estado_aux = 2;
		END IF;		
		--inserto el registro
		INSERT INTO finguitos_usuarios (cliente_documento, hotel_codigo, check_in, check_out, fecha_inicio, fecha_fin, finguitos, fecha_operacion,estado)
		VALUES (NEW.cliente_documento, NEW.hotel_codigo, NEW.check_in, NEW.check_out,fecha_inicio_aux,fecha_fin_aux ,finguitos_aux,current_timestamp,estado_aux);
		--chequeo si hay finguitos que vencieron para ese cliente
		UPDATE finguitos_usuarios
		SET estado = 2, fecha_operacion = current_timestamp 
		WHERE cliente_documento = NEW.cliente_documento AND estado = 1
		AND fecha_fin < current_timestamp;
	ELSIF TG_OP = 'UPDATE' THEN
		--verifico si existe una tupla en finguitos usuarios que haya que actualizar
		old_fecha_operacion := NULL;
		old_fecha_operacion = (SELECT fecha_operacion FROM finguitos_usuarios 
				  WHERE cliente_documento = NEW.cliente_documento
				  AND hotel_codigo = NEW.hotel_codigo
				  AND check_in = OLD.check_in
				  );
		IF old_fecha_operacion IS NOT NULL
		THEN
			fecha_inicio_aux = NEW.check_in + interval '1 month';
			fecha_fin_aux = NEW.check_out + interval '2 year'; 
			precio_aux = (SELECT precio_noche FROM costos_habitacion A
					WHERE A.hotel_codigo = NEW.hotel_codigo
					AND A.nro_habitacion = NEW.nro_habitacion
					AND NOT EXISTS (SELECT 1 FROM costos_habitacion B
								   WHERE A.hotel_codigo = B.hotel_codigo
								   AND A.nro_habitacion = B.nro_habitacion
								   AND A.fecha_desde < B.fecha_desde));
			finguitos_aux = trunc(((NEW.check_out - NEW.check_in)*precio_aux)/10);
			--verifico si el registro que se esta modificando corresponde a una estadia
			--a la que se le habian sumado los 5 finguitos extra, porque se le deben sumar de nuevo
			IF EXISTS (SELECT 1 FROM finguitos_usuarios S
				WHERE S.cliente_documento = NEW.cliente_documento
				AND S.hotel_codigo = NEW.hotel_codigo
				AND S.check_in <> OLD.check_in
				AND S.fecha_operacion < old_fecha_operacion) 
			THEN
				finguitos_aux = finguitos_aux + 5;
			END IF;
			IF fecha_fin_aux >= current_timestamp THEN
				estado_aux = 1;
			ELSE
				estado_aux = 2;
			END IF;	
			--actualizo el registro
			UPDATE finguitos_usuarios
			SET check_in = NEW.check_in, check_out = NEW.check_out,
			fecha_inicio = fecha_inicio_aux, fecha_fin = fecha_fin_aux, finguitos = finguitos_aux,
			fecha_operacion = current_timestamp, estado = estado_aux 
			WHERE cliente_documento = NEW.cliente_documento AND hotel_codigo = NEW.hotel_codigo
			AND check_in = OLD.check_in;
			--chequeo si hay finguitos que vencieron para ese cliente
			UPDATE finguitos_usuarios
			SET estado = 2, fecha_operacion = current_timestamp 
			WHERE cliente_documento = NEW.cliente_documento AND estado = 1
			AND fecha_fin < current_timestamp;
		END IF;
	END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER finguitos
AFTER INSERT OR UPDATE OR DELETE ON estadias_anteriores
FOR EACH ROW
EXECUTE FUNCTION actualizar_finguitos();