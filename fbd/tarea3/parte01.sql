CREATE OR REPLACE FUNCTION actividad_cliente(codigo char(1), clientedoc integer, anio integer)
RETURNS integer AS
$$
DECLARE
    resultado integer;
BEGIN
    IF EXISTS (SELECT 1 FROM clientes WHERE cliente_documento = clientedoc) THEN
        CASE codigo
            WHEN 'R'THEN
                SELECT DISTINCT COUNT(*) INTO resultado FROM reservas_anteriores WHERE 
                cliente_documento = clientedoc AND EXTRACT(YEAR FROM check_in) = anio;
			WHEN 'r' THEN
                SELECT DISTINCT COUNT(*) INTO resultado FROM reservas_anteriores WHERE 
                cliente_documento = clientedoc AND EXTRACT(YEAR FROM check_in) = anio;
            WHEN 'E' THEN
                SELECT DISTINCT COUNT(*) INTO resultado FROM estadias_anteriores WHERE 
                cliente_documento = clientedoc AND EXTRACT(YEAR FROM check_in) = anio;
			WHEN 'e' THEN
                SELECT DISTINCT COUNT(*) INTO resultado FROM estadias_anteriores WHERE 
                cliente_documento = clientedoc AND EXTRACT(YEAR FROM check_in) = anio;
            ELSE
                RAISE NOTICE 'Código de operación incorrecto';
				RETURN -1;
        END CASE;
    ELSE
        RAISE NOTICE 'No existe el cliente';
		RETURN -1;
    END IF;
    RETURN resultado;
END;
$$
LANGUAGE plpgsql;

