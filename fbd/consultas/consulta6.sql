SELECT ho.hotel_codigo, ho.nombre, ho.estrellas, ci.nombre as ciudad_nombre 
FROM hoteles ho, ciudades ci 
WHERE ho.ciudad_codigo = ci.ciudad_codigo 
    AND EXISTS 
        (
            SELECT * 
            FROM reservas re 
            WHERE re.hotel_codigo = ho.hotel_codigo
        ) 
    AND NOT EXISTS 
    (
        SELECT * 
        FROM estadias es 
        WHERE es.hotel_codigo = ho.hotel_codigo
    )