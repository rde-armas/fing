/*
 * transponer_tablero(+Tablero, -TableroTranspuesto): transpone un tablero dado
*/
transponer_tablero(m(f(M11, M12, M13, M14), f(M21, M22, M23, M24), f(M31, M32, M33, M34), f(M41, M42, M43, M44)), m(f(M11, M21, M31, M41), f(M12, M22, M32, M42), f(M13, M23, M33, M43), f(M14, M24, M34, M44))).


/*
 * mover_tablero(+Tablero, -TableroNuevo, -ScoreTotal): mueve los valores de un tablero hacia la izquierda y devuelve el puntaje obtenido
*/
mover_tablero(m(M1, M2, M3, M4 ), m(M1_nuevo, M2_nuevo, M3_nuevo, M4_nuevo), ScoreTotal) :-
    mover_fila(M1, M1_nuevo, Puntaje1),
    mover_fila(M2, M2_nuevo, Puntaje2),
    Suma1 is Puntaje1 + Puntaje2,
    mover_fila(M3, M3_nuevo, Puntaje3),
    Suma2 is Suma1 + Puntaje3,
    mover_fila(M4, M4_nuevo, Puntaje4),
    ScoreTotal is Puntaje4 + Suma2.

/*
 * mover_fila(+Fila, -NuevaFila, -Puntaje): mueve los valores de una fila hacia la izquierda y devuelve el puntaje obtenido
*/
mover_fila(f(-,-,-,-), f(-,-,-,-) , 0).
mover_fila(f(M1,M2,M3,M4), NuevaFila, Puntaje) :-
    (       
      M1 = -,
      (
          M2 \= -;
          M3 \= -;
          M4 \= -
      ),
      mover_fila(f(M2,M3,M4,-), NuevaFila, Puntaje);
      (   
        M1 \= -,
        (
            (
                (
                    M1 = M2,
                    Puntos is 2 * M1,
                    Suma1 is M1 + M2,
                    (
                        (
                            M3 \= -,
                            (
                                (
                                    M3 = M4,
                                    Puntaje is Puntos + (2* M4),
                                    Suma2 is M3 + M4,
                                    Suma3 = -,
                                    Suma4 = -
                                );
                                (
                                    M3 \= M4,
                                    Suma2 = M3,
                                    Suma3 = M4,
                                    Suma4 = -,
                                    Puntaje = Puntos 
                                )
                            )
                        );
                        (    
                            M3 = -,
                            Suma2 = M4,
                            Suma3 = -,
                            Suma4 = -,
                            Puntaje = Puntos
                        )
                    )
                );
                (
                    M2 \= M1,
                    M2 = -,
                    (
                        (
                            M1 = M3,
                            Puntaje is M1 * 2,
                            Suma1 is M1 + M3,
                            Suma2 = M4,
                            Suma3 = -,
                            Suma4 = -
                        );
                        (
                            M3 \= -,
                            (
                                (
                                    M3 = M4,
                                    Puntaje is M3 * 2,
                                    Suma2 is M3 + M4,
                                    Suma1 = M1,
                                    Suma3 = -,
                                    Suma4 = -
                                );
                                (
                                    Suma1 = M1,
                                    Suma2 = M3,
                                    Suma3 = M4,
                                    Suma4 = -,
                                    Puntaje = 0
                                )
                            )
                        );
                        (
                            M3 = -,
                            (
                                (
                                    M1 = M4,
                                    Puntaje is M1 * 2,
                                    Suma1 is M1 + M4,
                                    Suma2 = -,
                                    Suma3 = -,
                                    Suma4 = -
                                );
                                (
                                    Suma1 = M1,
                                    Suma2 = M4,
                                    Suma3 = -,
                                    Suma4 = -,
                                    Puntaje = 0
                                )
                            )
                        )
                    )
                );
                (        
                    M2 \= -,
                    M2 \= M1,
                    Suma1 = M1,
                    (
                        (
                            M2 = M3,
                            Suma2 is M2 + M3,
                            Suma3 = M4,
                            Suma4 = -,
                            Puntaje is M2 * 2
                        );
                        (
                            M2 \= M3,
                            (
                                (
                                    M3 \= -,
                                    Suma2  = M2,
                                    (   
                                       (
                                            M3 = M4,
                                            Suma3 is M3 + M4,
                                            Suma4 = -, 
                                            Puntaje is M3 * 2
                                        );
                                        (
                                            M3 \= M4,
                                            Suma3 = M3,
                                            Suma4 = M4,
                                            Puntaje = 0
                                     	)
                                	)
                                ); 
                                (
                                    M3 = -,
                                    (
                                    	(   
                                        	M4 = M2,
                                            Suma2 is M2 + M4,
                                            Suma3 = -,
                                            Suma4 = -,
                                            Puntaje = M2 * 2
                                        );
                                    	( 
                                        	Suma2 = M2,
                                            Suma3 = M4,
                                            Suma4 = -,
                                            Puntaje = 0
                                        )
                                    )

                                )
                            )
                        )
                    )
                )
            ),
            NuevaFila = f(Suma1, Suma2, Suma3, Suma4)
          )
      )
    ),!.

/*
 * invertir_tablero(+Tablero, -TableroInvertido): invierte un tablero dado
*/
invertir_tablero(m(F1, F2, F3, F4), TableroInvertido) :-
    invertirFila(F1, F1_),
    invertirFila(F2, F2_),
    invertirFila(F3, F3_),
    invertirFila(F4, F4_),
    TableroInvertido = m(F1_, F2_, F3_, F4_).
invertirFila(f(M1, M2, M3, M4), f(M4, M3, M2, M1)). 

/*
 * movimientoT(+Tablero,+Direccion,-TableroNew,-ScoreGen): toma el tablero y mueve las piezas hacia la dirección correspondiente
*/
movimientoT(Tablero, Direccion, TableroNew, ScoreGen) :-
    (
        Direccion = left,
        mover_tablero(Tablero, TableroNew, ScoreGen)
    ;
        Direccion = right,
        invertir_tablero(Tablero, TableroInvertido),
        mover_tablero(TableroInvertido, TableroNuevoInvertido, ScoreGen),
        invertir_tablero(TableroNuevoInvertido, TableroNew)
    ;
        Direccion = up,
        transponer_tablero(Tablero, TableroTranspuesto),
        mover_tablero(TableroTranspuesto, TableroNuevoTranspuesto, ScoreGen),
        transponer_tablero(TableroNuevoTranspuesto, TableroNew) 
    ;
        Direccion = down,
        transponer_tablero(Tablero, TableroTranspuesto),
        invertir_tablero(TableroTranspuesto, TableroTranspuestoInvertido),
        mover_tablero(TableroTranspuestoInvertido, TableroNuevoTranspuestoInvertido, ScoreGen),
        invertir_tablero(TableroNuevoTranspuestoInvertido, TableroNuevoTranspuesto),
        transponer_tablero(TableroNuevoTranspuesto, TableroNew)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * isValue(+Tablero, ?Posicion, ?Valor): devuelve el valor de una posicion de un tablero dado 
 * o la posicion de un valor dado o ambos o true si el valor dado esta en la posicion dada.
*/
isValue(Tablero, Posicion, Valor):-
    m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P)) = Tablero,
    ( 
        Posicion = 1, A = Valor;
        Posicion = 2, B = Valor;
        Posicion = 3, C = Valor;
        Posicion = 4, D = Valor;
        Posicion = 5, E = Valor;
        Posicion = 6, F = Valor;
        Posicion = 7, G = Valor;
        Posicion = 8, H = Valor;
        Posicion = 9, I = Valor;
        Posicion = 10, J = Valor;
        Posicion = 11, K = Valor;
        Posicion = 12, L = Valor;
        Posicion = 13, M = Valor;
        Posicion = 14, N = Valor;
        Posicion = 15, O = Valor;
        Posicion = 16, P = Valor
    ).

/*
 * addValue(+Tablero, +Posicion, +Valor, -TableroNuevo): agrega un valor a un tablero en una posicion dada
*/
addValue(Tablero, Posicion, Valor, TableroNuevo):-
    m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P)) = Tablero,
    (
        Posicion = 1, TableroNuevo = m(f(Valor,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 2, TableroNuevo = m(f(A,Valor,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 3, TableroNuevo = m(f(A,B,Valor,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 4, TableroNuevo = m(f(A,B,C,Valor), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 5, TableroNuevo = m(f(A,B,C,D), f(Valor,F,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 6, TableroNuevo = m(f(A,B,C,D), f(E,Valor,G,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 7, TableroNuevo = m(f(A,B,C,D), f(E,F,Valor,H), f(I,J,K,L), f(M,N,O,P));
        Posicion = 8, TableroNuevo = m(f(A,B,C,D), f(E,F,G,Valor), f(I,J,K,L), f(M,N,O,P));
        Posicion = 9, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(Valor,J,K,L), f(M,N,O,P));
        Posicion = 10, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,Valor,K,L), f(M,N,O,P));
        Posicion = 11, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,Valor,L), f(M,N,O,P));
        Posicion = 12, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,Valor), f(M,N,O,P));
        Posicion = 13, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(Valor,N,O,P));
        Posicion = 14, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,Valor,O,P));
        Posicion = 15, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,Valor,P));
        Posicion = 16, TableroNuevo = m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,Valor))
    ).

/*
 * ganador_tablero(+Tablero): devuelve true si el tablero tiene al menos un valor 2048
*/
ganador_tablero(Tablero):-
    isValue(Tablero, _, 2048).

/*
 * perdedor_tablero(+Tablero): devuelve true si el tablero no tiene movimientos posibles
*/
perdedor_tablero(Tablero):-
    \+ movimiento_del_jugador(Tablero, _,_, _).

/*
 * reemplazar_menos(+Tablero, -NewTablero): reemplaza los valores - por 0 en un tablero dado
*/
reemplazar_menos(m(Fila1, Fila2, Fila3, Fila4), m(Fila1New, Fila2New, Fila3New, Fila4New)) :-
    reemplazar_fila(Fila1, Fila1New),
    reemplazar_fila(Fila2, Fila2New),
    reemplazar_fila(Fila3, Fila3New),
    reemplazar_fila(Fila4, Fila4New).

reemplazar_fila(f(A, B, C, D), f(Anew, Bnew, Cnew, Dnew)) :-
    reemplazar_termino(A, Anew),
    reemplazar_termino(B, Bnew),
    reemplazar_termino(C, Cnew),
    reemplazar_termino(D, Dnew).

reemplazar_termino(Termino, NewTermino):- 
    (Termino = -, NewTermino  = 0);
    (NewTermino = Termino).
    

/*
 * resta_adyacentes_abs(+Tablero, -Resultado): devuelve una lista con la resta de los valores adyacentes de un tablero
*/
resta_adyacentes_abs(Tablero, Resultado) :-
    reemplazar_menos(Tablero, m(f(A,B,C,D), f(E,F,G,H), f(I,J,K,L), f(M,N,O,P))),
    Resta1 is abs(A - B),
    Resta2 is abs(A - E),
    Resta3 is abs(B - C),
    Resta4 is abs(B - F),
    Resta5 is abs(C - D),
    Resta6 is abs(C - G),
    Resta7 is abs(D - H),
    Resta8 is abs(E - F),
    Resta9 is abs(E - I),
    Resta10 is abs(F - G),
    Resta11 is abs(F - J),
    Resta12 is abs(G - H),
    Resta13 is abs(G - K),
    Resta14 is abs(H - L),
    Resta15 is abs(I - J),
    Resta16 is abs(I - M),
    Resta17 is abs(J - K),
    Resta18 is abs(J - N),
    Resta19 is abs(K - L),
    Resta20 is abs(K - O),
    Resta21 is abs(L - P),
    Resta22 is abs(M - N),
    Resta23 is abs(N - O),
    Resta24 is abs(O - P),
    Resultado = [Resta1, Resta2, Resta3, Resta4, Resta5, Resta6, Resta7, Resta8, Resta9, Resta10, Resta11, Resta12, Resta13, Resta14, Resta15, Resta16, Resta17, Resta18, Resta19, Resta20, Resta21, Resta22, Resta23, Resta24].


/*
 * sum_all_values(+Tablero, -Cantidad, -Suma): devuelve la cantidad de valores distintos de - que hay en un tablero 
 * y la suma de todos los valores
*/
sum_all_values(Tablero, Cantidad, Suma):-
    findall(Valor,
            (member(Posicion, [1,2,3,4, 5,6,7,8, 9,10,11,12, 13,14,15,16]),
            isValue(Tablero, Posicion, Valor),
            Valor \= -
            ),
            Valores),
    length(Valores, Cantidad),
    sum_list(Valores, Suma).

/*
 * funcion_evaluacion(+Tablero, -Score): devuelve el score de un tablero dado
*/
funcion_evaluacion(Tablero, Score):-
    (
        ganador_tablero(Tablero),
        Score = 500000,!
    );
    (
        perdedor_tablero(Tablero),
        Score = -1000000,!
    );
    sum_all_values(Tablero, Cantidad, Suma),
    resta_adyacentes_abs(Tablero, Resultado),
    sum_list(Resultado, Diff),
    Temp2  is Suma * 4,
    Temp3 is Temp2 - Diff,
    Temp4 is Temp3 * 2,
    Score is Temp4 / Cantidad.

/*
 * movimiento_del_juego(+Tablero, -NewTablero, -Direccion, -Score): devuelve el tablero resultante de agregar
 * un valor 2 o 4 en las posiciones vacias del tablero dado
*/
movimiento_del_juego(Tablero, NewTablero, Prob):-
    isValue(Tablero, Posicion, -),
    member(Valor, [2,4]),
    (Valor = 2, Prob = 0.8; Valor = 4, Prob = 0.2),
    addValue(Tablero, Posicion, Valor, NewTablero).
/*
 * movimiento_del_jugador(+Tablero, -NewTablero, -Direccion, -Score): devuelve el tablero resultante de movimientos validos del jugador
*/
movimiento_del_jugador(Tablero, NewTablero, Direccion, Score):-
	member(Direccion, [up,left,right,down]),
    movimientoT(Tablero, Direccion, NewTablero, Score),
    Tablero\=NewTablero.

/*
 * movimiento_del_jugador_dummy(+Tablero, -NewTablero, -Direccion, -Score): devuelve el tablero resultante de movimientos validos del 
 * en orden [up,down,left,right]
*/
movimiento_del_jugador_dummy(Tablero, NewTablero, Direccion, Score):-
	member(Direccion, [up,down,left,right]),
    movimientoT(Tablero, Direccion, NewTablero, Score),
    Tablero\=NewTablero.


/*
 * minimax_algorithm(+Tablero, +Depth, +Player, -Score, +ScoreMove): devuelve el score de un tablero dado,
 * a partir de un nivel de profundidad y un jugador (true = jugador, false = juego)
*/
minimax_algorithm(Tablero, 0, _, Score, _):-
    funcion_evaluacion(Tablero, Score),!.
    
minimax_algorithm(Tablero, Depth, true, Score, _):-
    % max player
    Depth \=0,
    findall(NewScore,
        (movimiento_del_jugador(Tablero, NewTablero, _, ScoreMov),
        DepthNew is Depth - 1,
        minimax_algorithm(NewTablero, DepthNew, false, NewScore, ScoreMov)),
        Scores),
   (Scores\=[],max_list(Scores, Score) ; Scores = [], Score= -100000 ). 

minimax_algorithm(Tablero, Depth, false, Score, ScoreMove):-
    % min player
    Depth \=0,
    findall(NewScore,
        (movimiento_del_juego(Tablero, NewTablero, Prob), 
        DepthNew is Depth - 1,
        minimax_algorithm(NewTablero, DepthNew, true, Valor, _),
        NewScore is  Valor * Prob),
        Scores),
    sum_list(Scores, Temp),
    length(Scores, Largo),
    Score is ScoreMove + (Temp / (Largo/2)). 

/*
 * max_score(+Pares,-Max): dada una lista de terminos par(Dir, Score), devuelve el par con el score maximo
*/
max_score([M], M).
max_score([par(Dir1, Score1)|T], Max) :-
    max_score(T, par(Dir2, Score2)),
    (Score1 >= Score2 -> Max = par(Dir1, Score1); Max = par(Dir2, Score2)).
	
/*
 * mejor_movimiento(+Tablero,+NivelMiniMax,+Estrategia,-Jugada): devuelve la mejor jugada posible, a partir de un tablero y una estrategia
*/
mejor_movimiento(Tablero,Nivel_minimax,minimax,Jugada):-
    findall(Score, (movimiento_del_jugador(Tablero, NewTablero, Mov, ScoreMov), % Generate possible new boards
        Depth is Nivel_minimax - 1,
        minimax_algorithm(NewTablero, Depth, false, NewScore, ScoreMov),
        Score = par(Mov,NewScore)),
        Scores),
    length(Scores, Largo),
    Largo > 0,
    max_score(Scores, Par),
	par(Jugada,_) = Par.

mejor_movimiento(Tablero,_,random,Jugada):-
    findall(Mov, movimiento_del_jugador(Tablero, _, Mov, _), Moves),
    length(Moves, Largo),
    Largo > 0,
    random(0, Largo, Index),
    nth0(Index, Moves, Jugada).


mejor_movimiento(Tablero,_,dummy,Jugada):-
    findall(SMove,(movimiento_del_jugador_dummy(Tablero, _, Mov, Score), SMove = par(Mov, Score)),SMoves),
    length(SMoves, Largo),
    (Largo > 0 -> max_score(SMoves, Par),par(Jugada,_) = Par; Jugada = up).

