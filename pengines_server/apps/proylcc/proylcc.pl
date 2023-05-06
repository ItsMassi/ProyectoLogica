:- module(proylcc, [
        obtenerIndice/2,
        funcionOrdenar/4,
        eliminandoBloquesBShell/3,
        eliminandoBloquesB/5,
        append/3,
        generarColumnaShell/3,
        generarColumna/5,
        generarListasDeListas/2,
        gravedad/3,
        agregarShell/2,
        agregarLista/7,
        join/4
		


	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
enlazarGrilla(L1, L2,Resultado) :-
    append(L1,L2,Resultado).
 

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
eliminandoBloquesB([] , [],Acc , Copia, _) :- append(Acc , [],Copia).

eliminandoBloquesB([], [T|Ts], Acc, Copia, _) :-
    append(Acc , [T], NewAcc),
    eliminandoBloquesB([],Ts, NewAcc,Copia,_).
    

eliminandoBloquesB([Cont], [_|Ts], Acc, Copia, Cont) :-
    append(Acc, [1], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB([], Ts, NewAcc, Copia, ContAux).

eliminandoBloquesB([Cont|Ls], [H|T], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(Ls, T, NewAcc, Copia, ContAux).

eliminandoBloquesB(L, [H|T], Acc, Copia, Cont) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(L, T, NewAcc, Copia, ContAux).

%Segunda etapa del Juego
generarColumnaShell(L,ColNum,Resultado):-
    length(L,Largo),
    generarColumna(L,ColNum,Largo,[],Resultado).

generarColumna([], ColNum, Largo, Acc, Resultado):- %caso base
    gravedad(Acc,X,Resultado). %gruardamos el resultado de la lista de columna

generarColumna(L, ColNum, Largo, Acc, Resultado):- 
    %Acc es una lista Acumulativa (auxiliar) que va a guardar las instancias 
    %intermedias de la lista final
    %
    %la idea es usar ColNum como indice
    nth0(ColNum,L,Valor),%sacamos el valor del exponente ColNum
    append(Acc,[Valor],Q),%lo agregamos al final de la lista Acc
    NuevoColNum is ColNum + 5, %calculamos el nuevo indice
    (NuevoColNum >= Largo -> % si el indice es no esta en la lista
    generarColumna([],_,_,Q,Resultado);%Termina
    generarColumna(L,NuevoColNum,Largo,Q,Resultado)).%caso contrario continua a agregar otro valor.

%generarListasDeListas: Retorna una lista que contiene la lista de elementos por Columnas de la Grid

generarListasDeListas(L1,Retorno ) :-
    generarColumnaShell(L1,0,R1),
	generarColumnaShell(L1,1,R2),
	append([R1],[R2],Resultado1),
	generarColumnaShell(L1,2,R3),
	append(Resultado1,[R3],Resultado2),
	generarColumnaShell(L1,3,R4),
	append(Resultado2,[R4],Resultado3),
	generarColumnaShell(L1,4,R5),
	append(Resultado3,[R5],Resultado4),
    Retorno=Resultado4.

%gravedad genera una lista donde los ceros fueron movidos adelante (arriba en la grilla)
%recibe: Una Columna (Lista), [], Y la variable de retorno
gravedad([],Acc,ResultadoC):-%caso base
    length(Acc,LargoAcc),
    NumCero is 8 - LargoAcc,%numero de Ceros
    length(CeroList, NumCero), findall(0, between(1, NumCero, _), CeroList),%generamos una lista de 0s
    append(CeroList,Acc,Q),%ponemos los ceros delante
    ResultadoC = Q.

gravedad([C|Cs],Acc,ResultadoC):-%caso recursivo
    ( C =\= 0 ->  append(Acc,[C],Q),gravedad(Cs,Q,ResultadoC);
    gravedad(Cs,Acc,ResultadoC)). %si C es diferente de 0 agregalo al acumulador.


%Funcion AgregarShell : Recibe una Lista de Listas de columnas y retorna como resultado la grid Resultante (Este metodo se utiliza despues de haber usado la funcion generarListasDeListas)
    agregarShell(S, GR) :-
        nth0(0,S,L1),
        nth0(1,S,L2),
        nth0(2,S,L3),
        nth0(3,S,L4),
        nth0(4,S,L5),
       agregarLista(L1,L2,L3,L4,L5,[],GR).
   
   agregarLista([],[],[],[],[],Acc,GR):- 
       GR=Acc.
   agregarLista([L1|L1S],[L2|L2S],[L3|L3S],[L4|L4S],[L5|L5S],Acc, GR) :-
       append(Acc,[L1],Q),
       append(Q,[L2],Q2),
       append(Q2,[L3],Q3),
       append(Q3,[L4],Q4),
       append(Q4,[L5],Q5),
       agregarLista(L1S,L2S,L3S,L4S,L5S,Q5,GR).

    %Metodo menorPotencia
    %metodo auxiliar que te devuelve: Resultado = 2^N
potenciaDos(0,1).
potenciaDos(N,Resultado) :-
    N > 0,
    N1 is N-1,
    potenciaDos(N1,R1),
    Resultado is 2*R1.

%Este metodo auxiliar te ayuda a buscar la menor potencia dentro de una
%lista L, empieza por debajo en 2^1 y luego aumenta hasta encontrar la menor
menorPotencia(Cont,Resultado,[]):-%caso base
    potenciaDos(Cont,Resultado).%guardamos la menor potencia de dos

menorPotencia(Cont,Resultado,L):- %caso recursivo
    potenciaDos(Cont,Check),%buscamos la menor potencia de dos
    Cont2 is Cont+1,
    %if ( la potencia esta dentro de L, mandalo a caso base (termina),
    %caso contrario continua
    ( member(Check,L) ->  menorPotencia(Cont,Resultado,[]);
                          menorPotencia(Cont2,Resultado,L)).

%Metodo reemplazarPorRandom : Recorro la grilla hasta encontrar un 0 , cuando encuentro un 0
% reemplazo el 0 por un random , sino  es un 0 dejo el numero como esta

reemplazarPorRandom([],Acc,Retorno) :-
    Retorno=Acc.

reemplazarPorRandom([G|Gs],Acc,Retorno):-
      potenciaDos(1,Num),
     ( G =\= 0 -> append(Acc,[G],Q) ,reemplazarPorRandom(Gs,Q,Retorno);
     append(Acc,[Num],R),
     reemplazarPorRandom(Gs,R,Retorno)).




    



join(Grid, Col, Path, RGrids):-
    funcionOrdenar(Path,Col,Aux,L),
    eliminandoBloquesBShell(L,Grid,GRetorno),
    enlazarGrilla([Grid],[GRetorno],RG),
    generarListasDeListas(GRetorno,Gresultante),
    agregarShell(Gresultante,Resultado),
    enlazarGrilla(RG,[Resultado],RGrids).

