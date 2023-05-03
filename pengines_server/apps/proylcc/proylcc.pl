:- module(proylcc, [
        obtenerIndice/2,
        funcionOrdenar/4,
        eliminandoBloquesBShell/3,
        eliminandoBloquesB/5,
        append/3,
        join/4
		


	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
enlazarGrilla(L1, L2,[L1,L2]).
 

obtenerIndice([X|Xs], Num) :- Num is (X*5) + Xs.

funcionOrdenar([X|Xs], Col, P, L) :- %L es la Lista resultante
	obtenerIndice(X, Pos),
	append(P, [Pos], Q),
	funcionOrdenar(Xs, Col, Q, NewL),
	L = NewL.
	
funcionOrdenar([], Col, P, L) :-
	sort(P, NewL),
	L = NewL.
	
%1 Etapa de la funcionalidad del juego).
% eliminandoBloquesBShell/3 using an accumulator
eliminandoBloquesBShell(L, T, Copia) :-
    
 eliminandoBloquesB(L, T, [], Copia, 0).

eliminandoBloquesB([], T, Acc, Copia, _) :-
 
    append(Acc, [], Copia).

eliminandoBloquesB([Cont], T, Acc, Copia, Cont) :-
    append(Acc, [1], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB([], T, NewAcc, Copia, ContAux).

eliminandoBloquesB([Cont|Ls], [H|T], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(Ls, T, NewAcc, Copia, ContAux).

eliminandoBloquesB(L, [H|T], Acc, Copia, Cont) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(L, T, NewAcc, Copia, ContAux).



join(Grid, Col, Path, RGrids):-
    funcionOrdenar(Path,Col,Aux,L),
    eliminandoBloquesBShell(L,Grid,GRetorno),
    enlazarGrilla(Grid,GRetorno,RGrids).
    
