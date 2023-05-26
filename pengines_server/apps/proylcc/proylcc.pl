:- module(proylcc, [
    obtenerIndice/2,
    traducirPathAIndices/4,
    sumar/4,
    enlazarGrillas/3,
    eliminarBloquesShell/3,
    eliminarBloque/5,
    insertarResultadoPath/6,
    append/3,
    generarColumnaShell/3,
    generarColumna/5,
    generarListasDeColumnas/2,
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
    boosterShell/3,
    botonBooster/4,
    log2/2,
    potenciaDeDosAprox/2,
    generarBloque/4,
    esPotenciaDeDos/1,
    sumarCamino/2,
    sumar/4,
    generarGrillaFinalBooster/5,
    sumarValoresLista/3,
    join/4
    


]).


/**
* join(+Grid, +NumOfColumns, +Path, -RGrids) 
* RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
* en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
*/ 
 





 
%----------------------PREDICADOS AUXILIARES-------------------------


sumarCamino([], 0). % Caso base: la suma de una lista vacía es 0.

sumarCamino([X|Xs],Suma) :-
    sumarCamino(Xs,SumaRestante), % Sumar los elementos restantes de la lista recursivamente.
    Suma is X+SumaRestante.

%Este predicado Retorna true si N es potencia de dos
esPotenciaDeDos(1).

esPotenciaDeDos(N) :-
    N > 1,
    N mod 2 =:= 0,
    Siguiente is N / 2,
   esPotenciaDeDos(Siguiente).

%Metodo que enlaza dos grillas, este metodo se usa para generar la "animacion de gravedad"
enlazarGrillas(L1, L2,Resultado) :-
    append(L1,L2,Resultado).

%dado una coordenada (X,Xs) te devuelve el indice al que pertenece dentro de la grid
obtenerIndice([X|[Xs|_]], Num) :- Num is (X*5) + Xs.

%Funcion que te devuelve la potencia de 2 proximada a un numero.
log2(X, Log2X) :- Log2X is log(X) / log(2).

%devuelve la potencia de dos mas cercana al numero X
potenciaDeDosAprox(X, Potencia) :- log2(X, Log2X), Potencia is 2 ** ceil(Log2X).

%suma los valores de los indices del path, dentro de la grilla
generarBloque([],_,Suma,Valor):-
    sumarCamino(Suma,Res),
    (not(esPotenciaDeDos(Res))->  potenciaDeDosAprox(Res,V) , Valor is V; Valor is Res).

generarBloque([C|Cs],Grid,Suma,Valor) :-
    nth0(C,Grid,V),
    append([V],Suma,L),
    generarBloque(Cs,Grid,L,Valor).
        
%este metodo retorna un la columna ColNum de la Grilla Grid
generarColumnaShell(L,ColNum,Resultado):-
    length(L,Largo),
    generarColumna(L,ColNum,Largo,[],Resultado).

generarColumna([], _, _, Acc, Resultado):- %caso base
    gravedad(Acc,_,Resultado). %gruardamos el resultado de la lista de columna

generarColumna(L, ColNum, Largo, Acc, Resultado):- 
    %Acc es una lista Acumulativa (auxiliar) que va a guardar las instancias 
    %intermedias de la lista final
    %la idea es usar ColNum como indice
    nth0(ColNum,L,Valor),%sacamos el valor del exponente ColNum
    append(Acc,[Valor],Q),%lo agregamos al final de la lista Acc
    NuevoColNum is ColNum + 5, %calculamos el nuevo indice
    (NuevoColNum >= Largo -> % si el indice es no esta en la lista
    generarColumna([],_,_,Q,Resultado);%Termina
    generarColumna(L,NuevoColNum,Largo,Q,Resultado)).%caso contrario continua a agregar otro valor.

%generarListasDeColumnas: Retorna una lista que contiene la lista de elementos por Columnas de la Grid
generarListasDeColumnas(Grid,Retorno ) :-
    %las variables Resultado son resultados intermedios
    generarColumnaShell(Grid,0,Columna1),
    generarColumnaShell(Grid,1,Columna2),
    append([Columna1],[Columna2],Resultado1),
    generarColumnaShell(Grid,2,Columna3),
    append(Resultado1,[Columna3],Resultado2),
    generarColumnaShell(Grid,3,Columna4),
    append(Resultado2,[Columna4],Resultado3),
    generarColumnaShell(Grid,4,Columna5),
    append(Resultado3,[Columna5],Resultado4),
    Retorno=Resultado4.

%Funcion AgregarShell : Recibe una Lista de Listas de columnas y retorna como resultado la grid Resultante 
%(Este metodo se utiliza despues de haber usado la funcion generarListasDeColumnas)
agregarShell(Grid, GrillaResultado) :-
    nth0(0,Grid,Columna1),
    nth0(1,Grid,Columna2),
    nth0(2,Grid,Columna3),
    nth0(3,Grid,Columna4),
    nth0(4,Grid,Columna5),
   agregarLista(Columna1,Columna2,Columna3,Columna4,Columna5,[],GrillaResultado).

%Este metodo vuelve a juntar todas columnas en una grilla GR = GrillaResultado
agregarLista([],[],[],[],[],Acc,GrillaResultado):- 
    GrillaResultado=Acc.
agregarLista([Columna1|Columna1S],[Columna2|Columna2S],[Columna3|Columna3S],[Columna4|Columna4S],[Columna5|Columna5S],Acc, GrillaResultado) :-
   append(Acc,[Columna1],Q),
   append(Q,[Columna2],Q2),
   append(Q2,[Columna3],Q3),
   append(Q3,[Columna4],Q4),
   append(Q4,[Columna5],Q5),
   agregarLista(Columna1S,Columna2S,Columna3S,Columna4S,Columna5S,Q5,GrillaResultado).


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
    %if ( la potencia esta dentro de L) then mandalo a caso base (termina),
    %else caso contrario continua
    ( member(Check,L) ->  menorPotencia(Cont,Resultado,[]); menorPotencia(Cont2,Resultado,L)).

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
    %if(el valor actual dentro de la grid es != 0) then no hacemos nada y continua con el siguiente, else lo reemplaza por un numero random y continua con el siguiente
    ( G =\= 0 -> append(Acc,[G],Q) ,reemplazarPorRandom(Gs,Q,Retorno);
    append(Acc,[Num],R),
    reemplazarPorRandom(Gs,R,Retorno)).

% Predicado auxiliar para comprobar si un movimiento es válido
movimientoValido(Grid, MinPot, Indice) :-
    Indice >= 0,
    length(Grid, L),
    Indice < L,
    nth0(Indice, Grid, MinPot).

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
    eliminarBloquesShell(L,Grid,Res),
    generarBloque(L,Grid,_,ValorBloque),
    append(AccSuma,[ValorBloque],ListaValores),
    generarGrillaFinalBooster(Ls,Res,ListaValores,Suma,Resultado).

generarGrillaFinalBooster([], Grid, AccSuma, Suma, Resultado):-
    sumarValoresLista(AccSuma,0,Suma),
    Resultado = Grid.

%Metodo que suma los valores de una lista
sumarValoresLista([],Acc,Suma):-
    Suma = Acc.
sumarValoresLista([L|Ls],Acc,Suma):-
    Aux is L+Acc,
    sumarValoresLista(Ls,Aux,Suma).










%--------------------PREDICADOS PRICIPALES------------------------

traducirPathAIndices([X|Xs], Col, P, L) :- %L es la Lista resultante
    obtenerIndice(X, Pos),
    append(P, [Pos], Q),
    traducirPathAIndices(Xs, Col, Q, L).


    traducirPathAIndices([[]|Xs], Col, P, L) :- %L es la Lista resultante
    traducirPathAIndices(Xs, Col, P, L).


    traducirPathAIndices([X|[]], _, P, L) :-
    obtenerIndice(X, Pos),
    sort(P, Sorted),
    append([Pos],Sorted,NewL),
    L = NewL.

%este metodo reemplaza los valores de los indices de una grilla por 0 y ubica el resultado de la suma de los
%valores de la grilla en esos indices
eliminarBloquesShell(Indices, Grid, Copia) :-
    generarBloque(Indices,Grid,_,Res),
    insertarResultadoPath(Indices, Grid, [], Copia, 0, Res).

%intercambia el valor dentro de la grilla por 0
%recibe: una parte Lista de indices,una parte de la grid, un acumulador, variable donde se guardara 
%la grilla resultado que sera una copia con cambios y un contador
eliminarBloque([] , [],Acc , Copia, _) :- append(Acc , [],Copia).%caso base

%caso 1: el contador es distinto del indice buscado -> no cambiamos nada
eliminarBloque(L, [H|T], Acc, Copia, Cont) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    eliminarBloque(L, T, NewAcc, Copia, ContAux).

%caso 2: el contador es igual indice buscado -> lo reemplaza por 0
eliminarBloque([Cont|Ls], [_|T], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminarBloque(Ls, T, NewAcc, Copia, ContAux).

%caso 3: solo queda un indice y es el mismo que el contador -> lo cambiamos por 0
eliminarBloque([Cont], [_|Ts], Acc, Copia, Cont) :-
    append(Acc, [0], NewAcc),
    ContAux is Cont + 1,
    eliminarBloque([], Ts, NewAcc, Copia, ContAux).

%caso 5: no hay mas indices para cambiar -> dejamos el resto como estaba hasta que se vacie la grid -> caso base
eliminarBloque([], [T|Ts], Acc, Copia, _) :-
    append(Acc , [T], NewAcc),
    eliminarBloque([],Ts, NewAcc,Copia,_).
    


%busca el primer elemento de los indices y le asigna el valor completo de la suma del path
%recibe: Lista de indices, Grid, Acumlador, variable donde se guardara la grilla resultado
%que sera una copia con cambios, un contador y el resultado de la suma de los valores de los indices
insertarResultadoPath([Cont|Ls], [_|T], Acc, Copia, Cont,Res) :-
    append(Acc, [Res], Aux),
    append(Aux,T,NewAcc),
    eliminarBloque(Ls, NewAcc, [], Copia, 0).

insertarResultadoPath(L, [H|T], Acc, Copia, Cont,Res) :-
    append(Acc, [H], NewAcc),
    ContAux is Cont + 1,
    insertarResultadoPath(L, T, NewAcc, Copia, ContAux,Res).


%Segunda etapa del Juego

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



% Predicado para explorar vecinos
%  Busca los vecinos del indice dado, verifica si son movimientos validos 
%  y los devuelve en una lista
explorarVecinos(Grid, Indice, MinPot,Vecinos) :-
    obtenerVecinos(Indice,VecinosObtenidos),
    % Esta lista contiene los índices de los nodos que están por encima, por debajo, 
    % a la izquierda y a la derecha del nodo actual, respectivamente.
    findall(N, (
        member(N, VecinosObtenidos),
        movimientoValido(Grid, MinPot,N)),
        Vecinos).

%recibe un indicey devuelve una lista de indices
obtenerVecinos(Indice, P) :-
    % Nos aseguramos que Indice Sea valido (i.e., divisible by 5)
    % obtiene los valores de X e Y para trabajar como matriz
    X is Indice // 5,
    Y is Indice - (X * 5),
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
        traducirPathAIndices(Vecinos,5,[],P).

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
    nth0(Indice,Grid,ValorIndice),
    dfsDesdeHasta(Grid,Indice,ValorIndice,Indices),
    %si el valor del indice es MinPut agrega este valor actual
    %caso contrario no egreges el indice actual
    (nth0(Indice,Grid,ValorIndice) -> append([],Indices,ListaIndices);%corregir esto a no hacer nada
                                    [_|Ts] = Indices, append([],Ts,ListaIndices)),
    list_to_set(ListaIndices,IndicesSinRepetir),%funcion de libreria estandar
    sort(IndicesSinRepetir,IndicesOrdenados),
    %si Los indices ordenados ya existen no los agreges al acumulador
    (member(IndicesOrdenados,Acumulador) ->  append(Acumulador,[],ListaDeListas);
    										append(Acumulador,[IndicesOrdenados],ListaDeListas)),
    IndiceB is Indice + 1,
    booster(Grid,IndiceB,ListaDeListas, VisitadosFinales).

%funcion principal del booster
boosterShell(Grid,GridResultado,Suma):-
    booster(Grid,0,[],ListaGrupos),
    eliminarGruposInvalidos(ListaGrupos,_Acc,GruposValidos),
    generarGrillaFinalBooster(GruposValidos,Grid,_,Suma,GridResultado).

botonBooster(Grids,_,_,RGrid) :-
    boosterShell(Grids,GRet,Suma),
    enlazarGrillas([Grids],[GRet],RGr),
    generarListasDeColumnas(GRet,Gresultado),
    agregarShell(Gresultado,Retorna),
    enlazarGrillas(RGr,[Retorna],RGr1),
    reemplazarPorRandom(Retorna,_,Resultante),
    enlazarGrillas(RGr1,[Resultante],RGrid).

join(Grid, Col, Path, RGrids):-
    traducirPathAIndices(Path,Col,_,L),
    eliminarBloquesShell(L,Grid,GRetorno),
    enlazarGrillas([Grid],[GRetorno],RG),
    generarListasDeColumnas(GRetorno,Gresultante),
    agregarShell(Gresultante,Resultado),
    enlazarGrillas(RG,[Resultado],RG2),
    reemplazarPorRandom(Resultado,_,Re),
    enlazarGrillas(RG2,[Re],RGrids).



    
 

