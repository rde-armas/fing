CREATE TABLE resumen (
    pais_codigo char(2),
    cant_estrellas integer,
    total_extra numeric(8, 2),
    PRIMARY KEY (pais_codigo, cant_estrellas)
);

CREATE OR REPLACE PROCEDURE generar_reporte()
AS $$
	DECLARE
		est integer;
		extra numeric(8, 2);
		reporte record;
    	monto numeric(8, 2);
		hotel_loop integer;
		pais_loop char(2);
	BEGIN 
		FOR pais_loop IN (SELECT pais_codigo FROM PAISES)
		LOOP			
			FOR est IN 1..5
			LOOP
				extra := 0;
				FOR hotel_loop IN (SELECT hotel_codigo FROM hoteles ho 
									WHERE ho.pais_codigo = pais_loop and ho.estrellas = est)
				LOOP					
					FOR reporte IN (SELECT * FROM ingreso_extra(hotel_loop))
					LOOP
						extra := extra + reporte.monto;
					END LOOP;		
				END LOOP;
				INSERT INTO resumen(pais_codigo, cant_estrellas, total_extra)
				VALUES(pais_loop, est, extra);
			END LOOP;
		END LOOP;
	END;
$$
LANGUAGE plpgsql;

