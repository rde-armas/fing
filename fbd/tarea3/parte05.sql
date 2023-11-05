CREATE TABLE IF NOT EXISTS registro_uso (
	usuario text NOT NULL,
	tabla name NOT NULL,
	fecha date NOT NULL, 
	cantidad integer NOT NULL,
	PRIMARY KEY (usuario, tabla, fecha)
);

CREATE OR REPLACE FUNCTION registro_operaciones() RETURNS TRIGGER AS $$
BEGIN
	INSERT INTO registro_uso(usuario, tabla, fecha, cantidad)
	VALUES (current_user, TG_TABLE_NAME, current_date, 1)
	ON CONFLICT (usuario, tabla, fecha) DO UPDATE
    SET cantidad = registro_uso.cantidad + 1;
	RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER registro_operaciones
AFTER INSERT OR UPDATE OR DELETE ON estadias_anteriores
FOR EACH STATEMENT
EXECUTE FUNCTION registro_operaciones();

CREATE TRIGGER registro_operaciones
AFTER INSERT OR UPDATE OR DELETE ON reservas_anteriores
FOR EACH STATEMENT
EXECUTE FUNCTION registro_operaciones();

CREATE TRIGGER registro_operaciones
AFTER INSERT OR UPDATE OR DELETE ON clientes
FOR EACH STATEMENT
EXECUTE FUNCTION registro_operaciones();