:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	RGrids = [[0 | Ns], [N2 | Ns]].

%Funcion Obtener
%Caso base :
ObtenerIndice([X |Xs],Num) :- Num is 	(X*5)+Xs
%Caso Base: La lista de listas de posiciones esta vacia.
FuncionOrdenar([[]],Col,[R |Rs],[])
% 1) Caso recursivo :
FuncionOrdenar([X|Xs],Col,[],_listaVacia) :-
	FuncionOrdenar([X|Xs],Col,[Pos]) Pos is  


%1 Etapa de la funcionalidad del juego)
%EliminandoBloques(Grid ,Columnas , Camino , Retorno)
%Caso base 1 :
EliminandoBloques([E] ,Col,_Path,_Retorno)
%Caso Recursivo :
EliminandoBloques([E |Es],Col,[[P|Ps]],[R |Rs]) :- 
	EliminandoBloques([Es] ,Col,[[Ps]],[R |Rs]) ,