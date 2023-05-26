:- module(proylcc, [
    obtenerIndice/2,
    funcionOrdenar/4,
    sumar/4,
    enlazarGrilla/3,
    eliminandoBloquesShell/3,
    eliminandoBloquesB/5,
    eliminandoBloquesA/6,
    append/3,
    generarColumnaShell/3,
    generarColumna/5,
    generarListasDeListas/2,
    gravedad/3,
    agregarShell/2,
    agregarLista/7,
    potenciaDos/2,
    menorPotencia/3,
    generarRandom/1,
    reemplazarPorRandom/3,
    dfs/5,
    movimientoValido/3,
    explorarVecinos/4,
    obtenerVecinos/2,
    dfsDesdeHasta/4,
    booster/4,
    boosterShell/2,
    botonBooster/4,
    log2/2,
    potenciaDeDosAprox/2,
    generarBloqueV/4,
    esPotenciaDeDos/1,
    sumarCamino/2,
    append/3,
    sumar/4,
    generarGrillaFinalBooster/5,
    join/4
    


]).


/**
* join(+Grid, +NumOfColumns, +Path, -RGrids) 
* RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
* en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
*/ 
 

sumarCamino([], 0). % Caso base: la suma de una lista vacía es 0.
sumarCamino([X|Xs],Suma) :-
      sumarCamino(Xs,SumaRestante), % Sumar los elementos restantes de la lista recursivamente.
      Suma is X+SumaRestante.

esPotenciaDeDos(1).
esPotenciaDeDos(N) :-
    N > 1,
    N mod 2 =:= 0,
    Siguiente is N / 2,
   esPotenciaDeDos(Siguiente).


enlazarGrilla(L1, L2,Resultado) :-
append(L1,L2,Resultado).


obtenerIndice([X|[Xs|_]], Num) :- Num is (X*5) + Xs.
%obtenerIndice([],_).

funcionOrdenar([X|Xs], Col, P, L) :- %L es la Lista resultante
obtenerIndice(X, Pos),
append(P, [Pos], Q),
funcionOrdenar(Xs, Col, Q, L).

%AGREGADO
funcionOrdenar([[]|Xs], Col, P, L) :- %L es la Lista resultante
funcionOrdenar(Xs, Col, P, L).


funcionOrdenar([X|[]], _, P, L) :-
obtenerIndice(X, Pos),
sort(P, Sorted),
append([Pos],Sorted,NewL),
L = NewL.
%Funcion que te devuelve la potencia de 2 proximada a un numero.
log2(X, Log2X) :- Log2X is log(X) / log(2).

potenciaDeDosAprox(X, Y) :- log2(X, Log2X), Y is 2 ** ceil(Log2X).
%suma los valores de los indices del path, dentro de la grilla
generarBloqueV([],_,Suma,Valor):-
    sumarCamino(Suma,Res),
    (not(esPotenciaDeDos(Res))->  potenciaDeDosAprox(Res,V) , Valor is V; Valor is Res).
generarBloqueV([C|Cs],Grid,Suma,Valor) :-
    nth0(C,Grid,V),
    append([V],Suma,L),
    generarBloqueV(Cs,Grid,L,Valor).
        

% eliminandoBloquesBShell/3 using an accumulator
%el eliminando bloques, el ultimos valor es un boolean para saber si ya se cambio el ultimo
eliminandoBloquesShell(L, T, Copia) :-
 %sumar(L,T,1,Res),
 generarBloqueV(L,T,_,Res),
 eliminandoBloquesA(L, T, [], Copia, 0, Res).

eliminandoBloquesB([] , [],Acc , Copia, _) :- append(Acc , [],Copia).

eliminandoBloquesB([], [T|Ts], Acc, Copia, _) :-
    append(Acc , [T], NewAcc),
    eliminandoBloquesB([],Ts, NewAcc,Copia,_).
    

eliminandoBloquesB([Cont], [_|Ts], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB([], Ts, NewAcc, Copia, ContAux).

eliminandoBloquesB([Cont|Ls], [_|T], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(Ls, T, NewAcc, Copia, ContAux).

eliminandoBloquesB(L, [H|T], Acc, Copia, Cont) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesB(L, T, NewAcc, Copia, ContAux).

%-------------------------------------
%caso: busca el primer elemento de los indices y le asigna el valor completo de la suma del path

eliminandoBloquesA([Cont|Ls], [_|T], Acc, Copia, Cont,Res) :-
    append(Acc, [Res], Aux),
    append(Aux,T,NewAcc),
    eliminandoBloquesB(Ls, NewAcc, [], Copia, 0).

eliminandoBloquesA(L, [H|T], Acc, Copia, Cont,Res) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminandoBloquesA(L, T, NewAcc, Copia, ContAux,Res).


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



%Metodo para generar numeros random
generarRandom(X):-
random(1,6,X).

%Metodo reemplazarPorRandom : Recorro la grilla hasta encontrar un 0 , cuando encuentro un 0
% reemplazo el 0 por un random , sino  es un 0 dejo el numero como esta

reemplazarPorRandom([],Acc,Retorno) :-
Retorno=Acc.

reemplazarPorRandom([G|Gs],Acc,Retorno):-
generarRandom(X),
  potenciaDos(X,Num),
 ( G =\= 0 -> append(Acc,[G],Q) ,reemplazarPorRandom(Gs,Q,Retorno);
 append(Acc,[Num],R),
 reemplazarPorRandom(Gs,R,Retorno)).

% Caso base: lista vacía de vecinos
% Si la lista de vecinos esta vacia, significa que ya se visitaron todos los nodos alcanzables
dfs(_, [], _, Visitados, Visitados).

% Caso recursivo: explorar el primer vecino y continuar con el resto
% En caso contrario, se explora el primer vecino, se añade a los visitados y se busca los vecinos de este vecino.
% Luego se hace una llamada recursiva a dfs con la lista de vecinos encontrados del vecino actual,
% junto con los demas vecinos pendientes.
dfs(Grid, [Vecino|Vecinos],MinPot, Visitados, VisitadosFinales) :-
    (member(Vecino, Visitados) ->
        dfs(Grid, Vecinos, MinPot, Visitados, VisitadosFinales)
    ;
        append(Visitados, [Vecino],NuevosVisitados),
        explorarVecinos(Grid, Vecino, MinPot,VecinosVecino),
        dfs(Grid, VecinosVecino, MinPot, NuevosVisitados, VisitadosDespuesVecinos),
        dfs(Grid, Vecinos, MinPot,VisitadosDespuesVecinos, VisitadosFinales)
    ).

% Predicado auxiliar para comprobar si un movimiento es válido
movimientoValido(Grid, MinPot, Indice) :-
    Indice >= 0,
    length(Grid, L),
    Indice < L,
    nth0(Indice, Grid, MinPot).

% Predicado auxiliar para explorar vecinos
%  Busca los vecinos del indice dado, verifica si son movimientos validos 
%  y los devuelve en una lista
explorarVecinos(Grid, Indice, MinPot,Vecinos) :-
    obtenerVecinos(Indice,V),
% Esta lista contiene los índices de los nodos que están por encima, por debajo, 
% a la izquierda y a la derecha del nodo actual, respectivamente.
findall(N, (
    member(N, V),
    movimientoValido(Grid, MinPot,N)
), Vecinos).

%recibe un indice Z y devuelve una lista de indices
obtenerVecinos(Z, P) :-
% Make sure Z is valid (i.e., divisible by 5)
% Solve for X and Y
X is Z // 5,
Y is Z - (X * 5),
Arriba is X-1,
Abajo is X+1,
Derecha is Y+1,
Izquierda is Y-1,
(X=:=0 -> PosArriba = [] ; PosArriba = [Arriba,Y]),
(X=:=8 -> PosAbajo = [] ; PosAbajo = [Abajo,Y]),
(Y=:=4 -> PosDerecha = [] ; PosDerecha = [X,Derecha]),
(Y=:=0 -> PosIzquierda = [] ; PosIzquierda = [X,Izquierda]),
((X=:=0;Y=:=0) -> PosArrIz = [] ; PosArrIz = [Arriba,Izquierda]),
((X=:=0;Y=:=4) -> PosArrDr = [] ; PosArrDr = [Arriba,Derecha]),
((X=:=4;Y=:=0) -> PosAbjIz = [] ; PosAbjIz = [Abajo,Izquierda]),
((X=:=4;Y=:=4) -> PosAbjDr = [] ; PosAbjDr = [Abajo,Derecha]),
findall(N, (
    member(N, [PosArriba,PosAbajo,PosDerecha,PosIzquierda,PosArrIz,PosArrDr,PosAbjIz,PosAbjDr]),
    dif(N,[]))
, Vecinos),
funcionOrdenar(Vecinos,5,[],P).

% Predicado principal para ejecutar DFS desde el índice A
% buscando los vecinos validos y visitando aquellos que no se hayan visitado.
% Retorna una lista con los indices visitados
dfsDesdeHasta(Grid, A, MinPot, VisitadosFinales) :-
    explorarVecinos(Grid, A, MinPot,Vecinos),
    dfs(Grid, Vecinos, MinPot, [A], VisitadosFinales).

%caso base de la ejecucion de booster hasta 40 tamaño de la lista
booster(_,40,Acumulador,VisitadosFinales):-
    VisitadosFinales = Acumulador.

% Funcion recursiva que ejecuta dfsDesdeHasta en cada indice desde
% el 0 hasta el 40
%acumulador funciona como acumulador de lista de listas
booster(Grid,Indice,Acumulador,VisitadosFinales):-
    %menorPotencia(1,MinPot,Grid),
    nth0(Indice,Grid,ValorIndice),
    dfsDesdeHasta(Grid,Indice,ValorIndice,Indices),
    %si el valor del indice es MinPut agrega este valor actual
    %caso contrario no egreges el indice actual
    (nth0(Indice,Grid,ValorIndice) -> append([],Indices,ListaIndices);%corregir esto a no hacer nada
                                    [_|Ts] = Indices, append([],Ts,ListaIndices)),
    list_to_set(ListaIndices,IndicesSinRepetir),%funcion de libreria estandar
    sort(IndicesSinRepetir,IndicesOrdenados),
    %write('Valor acumulador: '),write(Acumulador),nl,
    %write('Valor Indice: '),write(Indice),nl,
    %write('Valor IndicesOrdenados: '),write(IndicesOrdenados),nl,
    (member(IndicesOrdenados,Acumulador) ->  append(Acumulador,[],ListaDeListas);
    										append(Acumulador,[IndicesOrdenados],ListaDeListas)),
    
    %write('Valor ListaDeListas: '),write(ListaDeListas),nl,
    %write('----------------------------'),nl,
    IndiceB is Indice + 1,
    booster(Grid,IndiceB,ListaDeListas, VisitadosFinales).

boosterShell(Grid,GridResultado):-
    booster(Grid,0,[],ListaGrupos),
    eliminarGruposInvalidos(ListaGrupos,_Acc,GruposValidos),
    generarGrillaFinalBooster(GruposValidos,Grid,Aux,Suma,GridResultado).
    %sort(Indices, IndicesBooster).

%elimina grupos de longitud n <= 1
%[L|Ls] es la lista con los grupos de indices: [[1,2,3],[4],...]
eliminarGruposInvalidos([],Acc,IndicesBooster):-
    IndicesBooster = Acc.
eliminarGruposInvalidos([L|Ls], Acc,IndicesBooster):-
    length(L,Longitud),
    (Longitud =< 1 ->  append(Acc,[],Grupos); append(Acc,[L],Grupos)),
    eliminarGruposInvalidos(Ls,Grupos,IndicesBooster). 

%[L|Ls] es la lista de listas de indices, tiene el siguiente formato [[1,2,3],[4,5],...]
%Grid funcionara como grilla intermedia en donde se van eliminando los bloques de los grupos dentro de [L|Ls]
generarGrillaFinalBooster([L|Ls], Grid, AccSuma, Suma,Resultado):-
    eliminandoBloquesShell(L,Grid,Res),
    generarBloqueV(L,Grid,Aux,ValorBloque),
    append(AccSuma,[ValorBloque],ListaValores),
    generarGrillaFinalBooster(Ls,Res,ListaValores,Suma,Resultado).

generarGrillaFinalBooster([], Grid, AccSuma, Suma, Resultado):-
    sumarValoresLista(AccSuma,0,Suma),
    Resultado = Grid.

sumarValoresLista([],Acc,Suma):-
    Suma = Acc.
sumarValoresLista([L|Ls],Acc,Suma):-
    Aux is L+Acc,
    sumarValoresLista(Ls,Aux,Suma).

botonBooster(Grids,Col,Path,RGrid) :-

boosterShell(Grids,GRet),
%eliminandoBloquesShell(ListaIndices,Grids,GRet),
enlazarGrilla([Grids],[GRet],RGr),
generarListasDeListas(GRet,Gresultado),
agregarShell(Gresultado,Retorna),
enlazarGrilla(RGr,[Retorna],RGr1),
reemplazarPorRandom(Retorna,Acumulador,Resultante),
enlazarGrilla(RGr1,[Resultante],RGrid).

join(Grid, Col, Path, RGrids):-
    funcionOrdenar(Path,Col,Aux,L),
    eliminandoBloquesShell(L,Grid,GRetorno),
    enlazarGrilla([Grid],[GRetorno],RG),
    generarListasDeListas(GRetorno,Gresultante),
    agregarShell(Gresultante,Resultado),
    enlazarGrilla(RG,[Resultado],RG2),
    reemplazarPorRandom(Resultado,Acc,Re),
    enlazarGrilla(RG2,[Re],RGrids).



    
 

