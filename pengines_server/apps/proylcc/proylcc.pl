:- module(proylcc, [
		join/4
		
		%obtenerIndice/4,
		
		%funcionOrdenar/5


	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, Path, RGrids):-
	%eliminandoBloques(Path,Grid, RGrids).

%Funcion Obtener
%Caso base :
obtenerIndice([X |Xs],Num) :-  Num is (X*5)+Xs.
%Caso Base: La lista de listas de posiciones esta vacia.
funcionOrdenar([],Col,P,L) :- sort(P,L).
% 1) Caso recursivo :
funcionOrdenar([X|Xs],Col,P,L) :- 
	obtenerIndice(X,Pos) ,
	append(P,[Pos],Q) ,
	funcionOrdenar(Xs,Col,Q,L).
	
%1 Etapa de la funcionalidad del juego).
% eliminandoBloquesBShell/3 using an accumulator
eliminandoBloquesBShell(L, T, Copia) :-
    eliminandoBloquesB(L, T, [], Copia, 0).

eliminandoBloquesB([], T, Acc, Copia, _) :-
    write('caso base 1'), 
    append(Acc, [], Copia).

eliminandoBloquesB([Cont], T, Acc, Copia, Cont) :-
    write('caso base 2'),
    write(Cont),
    append(Acc, [1], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB([], T, NewAcc, Copia, ContAux).

eliminandoBloquesB([Cont|Ls], [H|T], Acc, Copia, Cont) :-
    write('Recursivo 1'),
    write(Cont),
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(Ls, T, NewAcc, Copia, ContAux).

eliminandoBloquesB(L, [H|T], Acc, Copia, Cont) :-
    write('Recursivo 2'),
    write(Cont),
    write(L),
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(L, T, NewAcc, Copia, ContAux).
