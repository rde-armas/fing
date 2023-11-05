SELECT es.hotel_codigo, es.nro_habitacion, es.cliente_documento, es.check_in 
FROM estadias_anteriores es JOIN reservas_anteriores res
    ON es.hotel_codigo = res.hotel_codigo
    AND es.nro_habitacion = res.nro_habitacion
    AND es.check_in = res.check_in