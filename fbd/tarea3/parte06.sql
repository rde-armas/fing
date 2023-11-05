CREATE SEQUENCE IF NOT EXISTS logidseq AS integer 
INCREMENT BY 1
START 1 NO CYCLE 

CREATE TABLE IF NOT EXISTS audit_estadia (
    idop integer PRIMARY KEY,
    accion char(1) NOT NULL CHECK (accion IN ('I', 'U', 'D')),
    fecha date NOT NULL,
    usuario text NOT NULL,
    cliente_documento integer NOT NULL,
    hotel_codigo integer NOT NULL,
    nro_habitacion integer NOT NULL,
    check_in date NOT NULL
);

CREATE OR REPLACE FUNCTION set_idop()
RETURNS TRIGGER AS $$
BEGIN
    NEW.idop = nextval('logidseq');
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- idop antes antes de la inserccion
CREATE TRIGGER set_idop_insert
BEFORE INSERT ON audit_estadia
FOR EACH ROW
EXECUTE FUNCTION set_idop();

CREATE OR REPLACE FUNCTION auditoria_estadias() RETURNS TRIGGER AS $$ 
BEGIN
	IF TG_OP = 'DELETE' THEN
		INSERT INTO audit_estadia (accion, fecha, usuario, cliente_documento, hotel_codigo, nro_habitacion, check_in)
		VALUES ('D', current_date, current_user, OLD.cliente_documento, OLD.hotel_codigo, OLD.nro_habitacion, OLD.check_in);
	ELSIF TG_OP = 'INSERT' THEN
		INSERT INTO audit_estadia (accion, fecha, usuario, cliente_documento, hotel_codigo, nro_habitacion, check_in)
        VALUES ('I', current_date, current_user, NEW.cliente_documento, NEW.hotel_codigo, NEW.nro_habitacion, NEW.check_in);
	ELSIF TG_OP = 'UPDATE' AND (OLD.cliente_documento <> NEW.cliente_documento OR OLD.hotel_codigo <> NEW.hotel_codigo OR
                                OLD.nro_habitacion <> NEW.nro_habitacion OR OLD.check_in <> NEW.check_in
                               ) THEN
		INSERT INTO audit_estadia (accion, fecha, usuario, cliente_documento, hotel_codigo, nro_habitacion, check_in)
        VALUES ('U', current_date, current_user, OLD.cliente_documento, OLD.hotel_codigo, OLD.nro_habitacion, OLD.check_in);
	END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER auditoria_estadias
AFTER INSERT OR UPDATE OR DELETE ON estadias_anteriores
FOR EACH ROW
EXECUTE FUNCTION auditoria_estadias();