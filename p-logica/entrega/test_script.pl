% Test predicates to generate test cases and save scores in a CSV file
:- consult('p2048log.pl'). % Load the 2048 game logic

random_seeds(0, []).
random_seeds(N, [Rand|Rest]) :-
    N > 0,
    N1 is N - 1,
    random(1, 1000, Rand), % Generates a random integer between 1 and 100
    random_seeds(N1, Rest).

/*
 * Inicializa los csv, genera las seeds a partir de una seed y corre los test para todas las estrategias.
*/
test_and_save_scores :-
    Initial_Seed = 12, % Seed generadora de las demas seeds
    set_random(seed(Initial_Seed)),
    random_seeds(10, Seeds),
    N_Iterations = 100,
    %% SI COMENTAN ESTO HACEN QUE TODO LOS TEST SE AGREGUEN AL FINAL DE LO QUE SEA QUE TENGA EL ARCHIVO AHORA
    %% ES DECIR, NO SE BORRA LA INFO QUE YA ESTA
    write('Iniciando...\n'),
    open('all_test_results.csv', write, Stream), 
    writeln(Stream, 'strategy,seed,depth,final_score,cant_mov,won,lost_with_1024,max_val,time,final_grid'),
    close(Stream),
    open('summary_per_seed_strategy_level.csv', write, Stream2),
    writeln(Stream2, 'strategy,seed,iterations,depth,score_avg,cant_mov_avg,win_rate,lost_with_1024_rate,time_avg'),
    close(Stream2),
    open('summary_per_strategy.csv', write, Stream3),
    writeln(Stream3, 'strategy,cant_seeds,iterations_per_seed,depth,score_avg,cant_mov_avg,win_rate_avg,win_rate_variance,lost_with_1024_rate_avg,time_avg'),
    close(Stream3),
    test_strategy(random, N_Iterations, Seeds, -),
    test_strategy(dummy, N_Iterations, Seeds, -),
    test_strategy(minimax, N_Iterations, Seeds, 3),
    test_strategy(minimax, N_Iterations, Seeds, 2),
    test_strategy(minimax, N_Iterations, Seeds, 1). 

/*
 * Calcula la media de una lista de numeros
*/
mean([], 0).
mean(List, Avg) :-
    length(List, Length),
    Length > 0,
    sum_list(List, Sum),
    Avg is Sum / Length.

/*
 * Calcula la diferencia cuadratica entre cada elemento de una lista y un valor dado
*/
quadraticDiff([],_, 0).
quadraticDiff([H|T], M, VO):-
    quadraticDiff(T,M,Y),
    VO is Y + ((H-M)*(H-M)).

/*
 * Calcula la varianza de una lista de numeros
*/
variance(L, 0):- length(L, N), N < 2.
variance(L, V1) :-
    length(L, N),
	N > 1,
    mean(L, M),
    quadraticDiff(L, M, VO),
    V1 is VO/(N - 1).

/*
 * Corre simulacion test_multiple_seeds para una estrategia, N_Iterations y Seeds dados, y luego calcula 
 * estadisticas sobre los resultados por estrategia. Guarda resultados en el archivo CSV summary_per_strategy.csv
*/
test_strategy(Strategy, N_Iterations, Seeds, Nivel_minimax):-
    test_multiple_seeds(Strategy, N_Iterations, Seeds, Nivel_minimax, WinRates, TotalLostWith1024s, ScoreAvgs, MoveAvgs, TimeAvgs),
    open('summary_per_strategy.csv', append, Stream),
    mean(ScoreAvgs, ScoreAvg),
    mean(MoveAvgs, MoveAvg),
    mean(WinRates, WinRateAvg),
    variance(WinRates, WinRateVariance),
    mean(TimeAvgs, TimeAvg),
    length(Seeds, Cant_seeds),
    mean(TotalLostWith1024s, LostWith1024RateAvg),
    atomic_list_concat([Strategy,Cant_seeds,N_Iterations, Nivel_minimax, ScoreAvg, MoveAvg, WinRateAvg, WinRateVariance, LostWith1024RateAvg, TimeAvg], ',', ScoreLine),
    writeln(Stream, ScoreLine),
    close(Stream).

/*
 * Simula N_Iterations partidas de 2048 por cada seed de Seeds para una estrategia dada,
 * guardando resultados en el archivo CSV summary_per_seed_strategy_level.csv
*/
test_multiple_seeds(_, _, [], _, [], [], [], [], []).
test_multiple_seeds(Strategy, N_Iterations, [Seed|Seeds], Nivel_minimax, [WinRate|RWinRates], [LostWith1024Rate| RLostWith1024Rate], [ScoreAvg|RScoreAvgs], [CantMovAvg|RMoveAvgs], [TimeAvg|RTimeAvgs]):-
    atomic_list_concat(['\nSTRATEGY: ',Strategy,', SEED: ',Seed,', LEVEL: ',Nivel_minimax], StepText),
    write(StepText),
    set_random(seed(Seed)), % Set the random seed
    test_single_seed(Strategy, N_Iterations, Nivel_minimax, Seed, TotalWins, TotalLostWith1024, TotalScore, TotalMoves, TotalTime),
    write('\nCompleted\n'),
    open('summary_per_seed_strategy_level.csv', append, Stream),
    ScoreAvg is TotalScore / N_Iterations,
    CantMovAvg is TotalMoves / N_Iterations,
    WinRate is TotalWins / N_Iterations,
    TimeAvg is TotalTime / N_Iterations,
    ((N_Iterations - TotalWins) > 0 -> LostWith1024Rate is TotalLostWith1024 / (N_Iterations - TotalWins); LostWith1024Rate = 0),
    atomic_list_concat([Strategy, Seed, N_Iterations, Nivel_minimax, ScoreAvg, CantMovAvg, WinRate, LostWith1024Rate,TimeAvg], ',', ScoreLine),
    writeln(Stream, ScoreLine),
    close(Stream),
    test_multiple_seeds(Strategy, N_Iterations, Seeds, Nivel_minimax, RWinRates, RLostWith1024Rate, RScoreAvgs, RMoveAvgs, RTimeAvgs).

/*
 * Simula N_Iterations partidas de 2048 dada una seed pre-fijada y estrategia dada, 
 * guardando resultados en el archivo CSV all_test_results.csv
*/
test_single_seed(_, 0, _, _, 0, 0, 0, 0, 0).
test_single_seed(Estrategia, N_Iterations, Nivel_minimax, Seed, TotalWins, TotalLostWith1024, TotalScore, TotalMoves, TotalTime) :-
    N_Iterations > 0,
    open('all_test_results.csv', append, Stream), % Open the CSV file in append mode
    atomic_list_concat(['\nSTRATEGY: ',Estrategia,', Iteration: ',N_Iterations], StepText),
    write(StepText),
    generar_tablero(Tablero), % Generate a random game board (replace with your own logic)
    statistics(walltime, [Start|_]),
    test_iterations(Estrategia, Nivel_minimax, Tablero, 'Playing', Won, LostWith1024, Score, Cant_mov, Final_grid),
    statistics(walltime, [End|_]),
    Time is (End - Start) / 1000.0,
    write_score_to_csv(Estrategia, Seed, Nivel_minimax, Score, Cant_mov, Won, LostWith1024, Time, Final_grid, Stream),
    close(Stream),
    Iter is N_Iterations - 1,
    test_single_seed(Estrategia, Iter, Nivel_minimax, Seed, RTotalWins, RTotalLostWith1024, RTotalScore, RTotalMoves, RTotalTime),
    TotalWins is RTotalWins + Won,
    TotalLostWith1024 is RTotalLostWith1024 + LostWith1024,
    TotalScore is RTotalScore + Score,
    TotalMoves is RTotalMoves + Cant_mov,
    TotalTime is RTotalTime + Time.


/*
 * Simula una unica partida de 2048, para una estrategia dada y retorna resultados
*/
test_iterations(_, _, Tablero, 'Lost', 0, LostWith1024, 0, 0, Tablero):-
    (isValue(Tablero,_,1024) -> LostWith1024 = 1; LostWith1024 = 0), !.
test_iterations(_, _, Tablero, 'Won', 1, 0, 0, 0, Tablero):- !.
test_iterations(Estrategia, Nivel_minimax, Tablero, 'Playing', Won, LostWith1024, Score, CantMov, FinalGrid) :-
    mejor_movimiento(Tablero, Nivel_minimax, Estrategia, Jugada),
    movimientoT(Tablero, Jugada, TableroAfterPlayerMove, ScoreGen),
    generar_bloque_aleatorio(TableroAfterPlayerMove, TableroAfterGameMove),
    (
        perdedor_tablero(TableroAfterGameMove), NewState = 'Lost';
        ganador_tablero(TableroAfterGameMove), NewState = 'Won';
        NewState =  'Playing'
    ),
    test_iterations(Estrategia,Nivel_minimax,TableroAfterGameMove, NewState, Won, LostWith1024, ScoreIter,CantMovIter, FinalGrid),
    Score is ScoreGen + ScoreIter,
    CantMov is 1 + CantMovIter.

/*
 * Escribe el resultado de una unica partida en el archivo CSV dado
*/
write_score_to_csv(Estrategia, Seed, Nivel_minimax, Score, Cant_mov, Won, LostWith1024, Time, Final_grid, Stream) :-
    term_string(Final_grid, Final_grid_str),
    atomic_list_concat(['"',Final_grid_str,'"'], QuotedFinal_grid_str),
    findall(Value, (isValue(Final_grid, _, Value), Value \= -), Values),
    max_list(Values, Max_val),
    atomic_list_concat([Estrategia, Seed, Nivel_minimax, Score, Cant_mov, Won, LostWith1024, Max_val, Time, QuotedFinal_grid_str], ',', ScoreLine),
    writeln(Stream, ScoreLine).


generar_tablero(TableroNew) :-
    Tablero = m(f(-, -, -, -), f(-, -, -, -), f(-, -, -, -), f(-, -, -, -)),
    generar_bloque_aleatorio(Tablero, Tablero1),
    generar_bloque_aleatorio(Tablero1, TableroNew).

generar_bloque_aleatorio(Tablero, TableroNew) :-
    findall(Indice, isValue(Tablero, Indice, -),Indices),
    random_member(Posicion, Indices),
    random_between(1, 10, ValorAleatorio),
    (
        ValorAleatorio =< 8, Valor = 2;
        ValorAleatorio > 8, Valor = 4
    ),
    addValue(Tablero, Posicion, Valor, TableroNew).

print_tablero(Tablero) :-
    m(f(A, B, C, D), f(E, F, G, H), f(I, J, K, L), f(M, N, O, P)) = Tablero,
    write('-----------------------------\n'),
    write('|'), write(A), write('|'), write(B), write('|'), write(C), write('|'), write(D), write('|\n'),
    write('-----------------------------\n'),
    write('|'), write(E), write('|'), write(F), write('|'), write(G), write('|'), write(H), write('|\n'),
    write('-----------------------------\n'),
    write('|'), write(I), write('|'), write(J), write('|'), write(K), write('|'), write(L), write('|\n'),
    write('-----------------------------\n'),
    write('|'), write(M), write('|'), write(N), write('|'), write(O), write('|'), write(P), write('|\n'),
    write('-----------------------------\n').


% Entry point of the script
:- initialization(test_and_save_scores).