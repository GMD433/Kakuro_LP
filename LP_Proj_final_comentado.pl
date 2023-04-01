%Nome: Gustavo Manuel Cabral de Mascarenhas Diogo Numero: 99233 %

:- [codigo_comum].

%juntar(Lista,Lista,Lista): Junta os elementos de duas listas. E diferente do metapredicado append, uma vez que, une os primeiros dois elementos da lista dois a dois.%

juntar([],[],[]).

juntar([P|R],[P1|R1],[P,P1|R2]) :-
    	juntar(R,R1,R2).

%cria_ilha(Integer,Lista,Lista): Cria uma lista de ilhas.%

cria_ilhas(_,[],[]).

cria_ilhas(N_L,[P,_|R],R1):-
    	P == 0,
    	cria_ilhas(N_L,R,R1).

cria_ilhas(N_L,[P,P1|R],[ilha(P,(N_L,P1))|R1]):-
    	P > 0,
    	cria_ilhas(N_L,R,R1).

%ilhas_linhas(Lista,Lista): Devolve uma lista com todos as ilhas de uma determinada linha usando uma puzzle associado a um indice.%

ilhas_linhas([],[]).

ilhas_linhas([P,P1|R],[P2|R2]):-
    	extrai_ilhas_linha(P,P1,P2),
    	ilhas_linhas(R,R2).

%faz_ilha(integer,integer,integer,ilha): Construtor que devolve uma ilha com um numero,linha e coluna.%

faz_ilha(N,L,C,ilha(N,(L,C))).

%linha(ilha,integer),coluna(ilha,integer): Seletores que devolvem a linha e coluna de uma ilha, respetivamente.%

linha(ilha(_,(L,_)),L).
coluna(ilha(_,(_,C)),C).

%compara_linha(ilha,Lista,Lista): Compara a linha de uma ilha a uma a lista de ilhas e adiciona apenas as que possuem linha em comum.%

compara_linha(_,[],[]).

compara_linha(Ilha,[P|R],[P|R1]) :-
    linha(Ilha,L),
    linha(P,L1),
    L == L1,
    compara_linha(Ilha,R,R1).

compara_linha(Ilha,[P|R],R1) :-
    linha(Ilha,L),
    linha(P,L1),
    L \= L1,
    compara_linha(Ilha,R,R1).

%compara_coluna(ilha,Lista,Lista): Compara a coluna de uma ilha a uma a lista de ilhas e adiciona apenas as que possuem coluna em comum.%

compara_coluna(_,[],[]).

compara_coluna(Ilha,[P|R],[P|R1]) :-
    coluna(Ilha,C),
    coluna(P,C1),
    C == C1,
    compara_coluna(Ilha,R,R1).

compara_coluna(Ilha,[P|R],R1) :-
    coluna(Ilha,C),
    coluna(P,C1),
    C \= C1,
    compara_coluna(Ilha,R,R1).

%esq(ilha,lista,lista),dir(ilha,lista,lista): Devolvem as ilhas a esquerda e a direita de uma ilha numa lista, respetivamente.%

esq(_,[],[]).

esq(I,[P|_],[]) :-
    I == P,!.

esq(I,[P,P1|_],[P]) :-
    I == P1,!.

esq(I,[_,P1|R],R1) :-
    I \== P1,
    esq(I,[P1|R],R1),!.

dir(_,[],[]).

dir(I,[P,P1|_],[P1]) :-
    I == P,!.

dir(I,[P,P1|R],R1) :-
    I \== P,
    dir(I,[P1|R],R1),!.

dir(I,[P],[]) :-
    I == P,!.

%aumenta(integer,integer): Incrementa por um o valor dado.%

aumenta(X, X1) :-
    X1 is X+1.

%vizinhas_ilhas(integer,lista,lista): Devolve todos as vizinhas de uma lista de ilhas.%

vizinhas_ilhas(X,Ilhas,[P|R]) :-
    length(Ilhas,L),
    X < L,
    nth1(X,Ilhas,E),
    vizinhas(Ilhas,E,P),
    aumenta(X,X1),
    vizinhas_ilhas(X1,Ilhas,R).

vizinhas_ilhas(X,Ilhas,[P]) :-
    length(Ilhas,L),
    X == L,
    nth1(X,Ilhas,E),
    vizinhas(Ilhas,E,P).

%lista_estados(integer,lista,lista,lista): Cria uma lista de todas as entradas de todas as ilhas dadas.Fundamentalmente, cria um estado usando recursao.%

lista_estados(X,Ilhas,Vizinhas,[P|R]) :-
    length(Ilhas,L),
    X < L,
    nth1(X,Ilhas,E),
    nth1(X,Vizinhas,E1),
    aumenta(X,X1),
    P = [E,E1,[]],
    lista_estados(X1,Ilhas,Vizinhas,R).

lista_estados(X,Ilhas,Vizinhas,[P]) :-
    length(Ilhas,L),
    X < L,
    nth1(X,Ilhas,E),
    nth1(X,Vizinhas,E1),
    P = [E,E1,[]].

%posicao(integer,integer,posicao): Construtor que cria uma posicao.%

posicao(L,C,(L,C)).

%condicao(posicao,posicao,integer,integer,integer,integer,integer): Condicao que permite verificar se as linhas e colunas de uma ilha e de uma vizinha sao iguas as posicoes dadas.%

condicao(Pos1,Pos2,L,C,LV,CV) :-
    (Pos1,Pos2) ==((L,C),(LV,CV)),!.
    
condicao(Pos1,Pos2,L,C,LV,CV) :-
    (Pos2,Pos1) ==((L,C),(LV,CV)),!.

%posicao(ilha,lista,lista,entrada): Construtor que cria uma entrada.%

entrada(I,Vz,Pt,[I,Vz,Pt]).

%caminhos_livres(posicao,posicao,lista,ilha,lista,lista): Obtem todos os caminhos livres de um estado recursivamente.%

caminhos_livres(_,_,_,_,[],[]).

caminhos_livres(Pos1,Pos2,Posicoes,I,[P|R],[P|R1]) :-
    caminho_livre(Pos1,Pos2,Posicoes,I,P),
    caminhos_livres(Pos1,Pos2,Posicoes,I,R,R1),!.

caminhos_livres(Pos1,Pos2,Posicoes,I,[P|R],R1) :-
    \+caminho_livre(Pos1,Pos2,Posicoes,I,P),
    caminhos_livres(Pos1,Pos2,Posicoes,I,R,R1),!.

%actualiza_vizinhas_entradas(posicao,posicao,lista,lista,lista): Atualiza a lista de vizinhas de todas as entradas de um estado.%

actualiza_vizinhas_entradas(_,_,_,[],[]).

actualiza_vizinhas_entradas(Pos1,Pos2,Posicoes,[P|R],[P1|R1]) :-
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,P,P1),
	actualiza_vizinhas_entradas(Pos1,Pos2,Posicoes,R,R1),!.

%ilhas_pontes(ilha,integer): Obtem o numero de pontes de uma ilha terminada.%

ilha_pontes(ilha(N_pontes,_),N_pontes).

%ilhas_term_aux(lista,lista): Predicado auxiliar que usa a recursao para obter todas as ilhas terminadas de um estado.%

ilhas_term_aux([],[]).

ilhas_term_aux([P|R],[I|R1]) :-
    nth1(1,P,I),
    nth1(3,P,Pts),
    ilha_pontes(I,N_pontes),
    length(Pts,Pts_L),
    N_pontes \== 'X',
    Pts_L == N_pontes,
    ilhas_term_aux(R,R1).

ilhas_term_aux([P|R],R1) :-
    nth1(1,P,I),
    ilha_pontes(I,N_pontes),
    N_pontes == 'X',
    ilhas_term_aux(R,R1).

ilhas_term_aux([P|R],R1) :-
    nth1(1,P,I),
    nth1(3,P,Pts),
    ilha_pontes(I,N_pontes),
    length(Pts,Pts_L),
    N_pontes \== 'X',
    Pts_L \== N_pontes,
    ilhas_term_aux(R,R1).

%tira_ilhas_term_entradas(lista,lista,lista): Aplica a tira_ilhas_terminadas_entrada recursivamente.%

tira_ilhas_term_entradas([],_,[]).

tira_ilhas_term_entradas([P|R],Ilhas_term,[Nova_Entrada|R1]) :-
    tira_ilhas_terminadas_entrada(Ilhas_term,P,Nova_Entrada),
    tira_ilhas_term_entradas(R,Ilhas_term,R1).

%marca_ilhas_term_entradas(lista,lista,lista): Apilca a marca_ilha_term_entrada recursivamente.%

marca_ilhas_term_entradas([],_,[]).

marca_ilhas_term_entradas([P|R],Ilhas_term,[Nova_Entrada|R1]) :-
    marca_ilhas_terminadas_entrada(Ilhas_term,P,Nova_Entrada),
    marca_ilhas_term_entradas(R,Ilhas_term,R1).

%adiciona_pontes_entrada(ilha,ilha,entrada,entrada): Adiciona as pontes possiveis a uma entrada.% 

adiciona_pontes_entrada(Ilha1,Ilha2,Entrada,Nova_Entrada) :-
    linha(Ilha1,L1),
    coluna(Ilha1,C1),
    linha(Ilha2,L2),
    coluna(Ilha2,C2),
    cria_ponte((L1,C1),(L2,C2),N_ponte),
    entrada(I,Vz,Pts,Entrada),
    append([Pts,[N_ponte]],N_Pts),
    entrada(I,Vz,N_Pts,Nova_Entrada).

%adiciona_pontes(ilha,ilha,lista,lista): Aplica a adiciona_pontes_entrada recursivamente.%

adiciona_pontes(_,_,[],[]).

adiciona_pontes(Ilha1,Ilha2,[P|R],[P1|R1]) :-
    entrada(I,Vz,_,P),
    (I == Ilha1 ->
    include(=(Ilha2),Vz,Vz_comum),
    length(Vz_comum,Len),
    (Len == 1 ->  
    adiciona_pontes_entrada(Ilha1,Ilha2,P,P1),
    adiciona_pontes(Ilha1,Ilha2,R,R1))
    ;I == Ilha2 -> 
    include(=(Ilha1),Vz,Vz_comum),
    length(Vz_comum,Len),
    (Len == 1 ->  
    adiciona_pontes_entrada(Ilha1,Ilha2,P,P1),
    adiciona_pontes(Ilha1,Ilha2,R,R1))
    ;P = P1,
	adiciona_pontes(Ilha1,Ilha2,R,R1)),!.
 
%multiplica_pontes_entrada(integer,entrada,entrada): Multiplica o numero de pontes consoante o numero de pontes dado.%

multiplica_pontes_entrada(Num_pontes,Entrada,N_entrada) :-
    entrada(I,Vz,Pts,Entrada),
    length(Pts,Len),
    (Len == 0 ->  
    N_entrada  = Entrada
    ;last(Pts,Pt),
    Num_pontes1 is Num_pontes-1,
    length(L,Num_pontes1),
    maplist(=(Pt),L),
   	append(Pts,L,N_Pts),
    entrada(I,Vz,N_Pts,N_entrada)).

%multiplica_pontes(integer,lista,lista): Aplica a multiplica_pontes_entrada recursivamente.%

multiplica_pontes(_,[],[]).

multiplica_pontes(Num_pontes,[P|R],[P1|R1]) :-
    multiplica_pontes_entrada(Num_pontes,P,P1),
    multiplica_pontes(Num_pontes,R,R1),!.

%extrai_ilhas_linha(integer,lista,lista): Devolve todas as ilhas de uma linha.%

extrai_ilhas_linha(N_L,Linha,Ilhas) :-
    	findall(X,nth1(X,Linha,_),L),
    	juntar(Linha,L,LP),
    	cria_ilhas(N_L,LP,Ilhas).

%ilhas(lista,lista): Devolve as ilhas de uma puzzle.%

ilhas(Puz,Ilhas) :-
	findall(X,nth1(X,Puz,_),L),
   	juntar(L,Puz,LP),
    ilhas_linhas(LP,Ilhas_listas),
	flatten(Ilhas_listas,Ilhas),!.

%vizinhas(lista,ilha,lista): Devolve todas as vizinhas de uma ilha sendo que ambas pertencem a uma lista de ilhas.%

vizinhas(Ilhas,Ilha,Vizinhas) :-
    compara_linha(Ilha,Ilhas,LC),
    compara_coluna(Ilha,Ilhas,CC),
    esq(Ilha,LC,LCB),
    dir(Ilha,LC,LCA),
    esq(Ilha,CC,CCB),
    dir(Ilha,CC,CCA),
    Vizinhas_Ll = [CCB,LCB,LCA,CCA],
    flatten(Vizinhas_Ll,Vizinhas).

%estado(lista,lista): Devolve um estado de todas as ilhas presentes numa lista de ilhas.%

estado(Ilhas,Estado) :-
    vizinhas_ilhas(1,Ilhas,Vizinhas),
    lista_estados(1,Ilhas,Vizinhas,Estado).

%posicoes_entre(posicao,posicao,lista): Devolve uma lista com todas as posicoes entre duas posicoes.%

posicoes_entre(Pos1,Pos2,Posicoes) :-
    posicao(L1,C1,Pos1),
    posicao(L2,C2,Pos2),
    L1 == L2,
    C1 > C2,
    findall((L1,C),between(C2,C1,C),Pos3),
	exclude(=(Pos1),Pos3,Pos4),
    exclude(=(Pos2),Pos4,Posicoes),!.

posicoes_entre(Pos1,Pos2,Posicoes) :-
    posicao(L1,C1,Pos1),
    posicao(L2,C2,Pos2),
    L1 == L2,
    C2 > C1,
    findall((L1,C),between(C1,C2,C),Pos3),
	exclude(=(Pos1),Pos3,Pos4),
    exclude(=(Pos2),Pos4,Posicoes),!.

posicoes_entre(Pos1,Pos2,Posicoes) :-
    posicao(L1,C1,Pos1),
    posicao(L2,C2,Pos2),
    C1 == C2,
    L1 > L2,
    findall((L,C1),between(L2,L1,L),Pos3),
	exclude(=(Pos1),Pos3,Pos4),
    exclude(=(Pos2),Pos4,Posicoes),!.

posicoes_entre(Pos1,Pos2,Posicoes) :-
    posicao(L1,C1,Pos1),
    posicao(L2,C2,Pos2),
    C1 == C2,
    L2 > L1,
    findall((L,C1),between(L1,L2,L),Pos3),
	exclude(=(Pos1),Pos3,Pos4),
    exclude(=(Pos2),Pos4,Posicoes),!.

%cria_ponte(posicao,posicao,ponte): Devolve uma ponte.%

cria_ponte(Pos1,Pos2,Ponte) :-
    posicao(L1,C1,Pos1),
    posicao(L2,C2,Pos2),
    (L1 > L2 ->  
    Ponte = ponte(Pos2,Pos1)
    ;L2 > L1 ->  
    Ponte = ponte(Pos1,Pos2)
    ;C1 > C2 ->  
    Ponte = ponte(Pos2,Pos1)
    ;C2 > C1 ->  
    Ponte = ponte(Pos1,Pos2)
    ).

%caminho_livre(posicao,posicao,lista,ilha,lista): Devolve o valor booleano dependendo se a ponte criada por duas posicoes faz com que duas ilhas deixem de ser vizinhas.%

caminho_livre(Pos1,Pos2,Posicoes,I,Vz) :-
    cria_ponte(Pos1,Pos2,_),
    linha(I,L),
	coluna(I,C),
    linha(Vz,LV),
	coluna(Vz,CV),
    (condicao(Pos1,Pos2,L,C,LV,CV) 
    -> true
    ;posicoes_entre((L,C),(LV,CV),Pos3),
    intersection(Pos3,Posicoes,Pos4),
	length(Pos4,Len),
    Len \==1
     ).

%actualiza_vizinhas_entrada(posicao,posicao,lista,entrada,entrada): Devolve uma entrada com as vizinhas que continuem a ser vizinhas de uma ilha mesmo apos da criacao de uma ponte.%

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,Entrada,Nova_Entrada) :-
    entrada(I,Vz,Pt,Entrada),
    caminhos_livres(Pos1,Pos2,Posicoes,I,Vz,Nova_Vz),
	entrada(I,Nova_Vz,Pt,Nova_Entrada),!.

%actualiza_vizinhas_apos_pontes(lista,posicao,posicao,lista): Devolve um estado com as vizinhas das entradas atualizadas apos a criacao de uma ponte.%

actualiza_vizinhas_apos_pontes(Estado,Pos1,Pos2,Novo_estado) :-
    posicoes_entre(Pos1,Pos2,Posicoes),
    actualiza_vizinhas_entradas(Pos1,Pos2,Posicoes,Estado,Novo_estado).

%ilhas_terminadas(lista,lista): Devolve todas as ilhas terminadas de um estado.%

ilhas_terminadas(Estado,Ilhas_term) :-
    ilhas_term_aux(Estado,Ilhas_term),!.

%tira_ilha_terminadas_entrada(lista,entrada,entrada): Devolve uma entrada com as ilhas vizinhas que ja foram terminadas retiradas.%

tira_ilhas_terminadas_entrada(Ilhas_term,Entrada,Nova_Entrada) :-
    entrada(I,Vzs,Pt,Entrada),
    subtract(Vzs,Ilhas_term,Nova_Vzs),
    entrada(I,Nova_Vzs,Pt,Nova_Entrada).

%tira_ilha_terminadas(lista,lista,lista): Devolve um estado  com as ilhas vizinhas que ja foram terminadas retiradas para cada entrada.%

tira_ilhas_terminadas(Estado,Ilhas_term,Novo_Estado) :-
    tira_ilhas_term_entradas(Estado,Ilhas_term,Novo_Estado).

%marca_ilha_terminadas_entrada(lista,entrada,entrada): Devolve uma entrada com o numero de pontes substituido por X se esta possuir uma ilha terminada.%

marca_ilhas_terminadas_entrada(Ilhas_term,Entrada,Nova_Entrada) :-
    entrada(I,Vzs,Pt,Entrada),
    member(I,Ilhas_term),
    linha(I,L),
    coluna(I,C),
    faz_ilha('X',L,C,NI),
    entrada(NI,Vzs,Pt,Nova_Entrada),!.

marca_ilhas_terminadas_entrada(Ilhas_term,Entrada,Nova_Entrada) :-
    entrada(I,_,_,Entrada),
    \+member(I,Ilhas_term),
    Entrada = Nova_Entrada.

%marca_ilhas_terminadas(lista,lista,lista): Devolve um estado com as entradas com o numero de pontes substituido por X se estas possuirem ilhas terminadas.%

marca_ilhas_terminadas(Estado,Ilhas_term,Novo_Estado) :-
    marca_ilhas_term_entradas(Estado,Ilhas_term,Novo_Estado).

%trata_ilhas_terminadas(lista,lista): Devolve a aplicacao dos predicados  tira_ilhas_terminadas/3 e  marca_ilhas_terminadas/3.%

trata_ilhas_terminadas(Estado,Novo_Estado) :-
    ilhas_terminadas(Estado,Ilhas_term),
    tira_ilhas_terminadas(Estado,Ilhas_term,N_Estado),
    marca_ilhas_terminadas(N_Estado,Ilhas_term,Novo_Estado).

%junta_pontes(lista,integer,ilha,ilha,lista): Devolve um estado atualizado com as pontes colocadas nas entradas, as vizinhas atualizadas e as ilhas terminadas.%

junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
    adiciona_pontes(Ilha1,Ilha2,Estado,Estado1),
    multiplica_pontes(Num_pontes,Estado1,Estado2),
    linha(Ilha1,L1),
    coluna(Ilha1,C1),
    linha(Ilha2,L2),
    coluna(Ilha2,C2),
    actualiza_vizinhas_apos_pontes(Estado2,(L1,C1),(L2,C2),Estado3),
    trata_ilhas_terminadas(Estado3,Novo_estado).