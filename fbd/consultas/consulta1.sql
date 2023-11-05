SELECT H.hotel_codigo, H.nombre AS hotel_nombre
FROM hoteles h 
WHERE 
	h.estrellas > 3 
AND h.pais_codigo IN 
	(
		SELECT pa.pais_codigo 
		FROM paises pa 
		WHERE pa.continente_codigo = 'EU'
	) 
AND h.nombre IN 
	(
		SELECT CI.nombre 
		FROM ciudades CI 
		WHERE CI.ciudad_codigo = h.ciudad_codigo
	);