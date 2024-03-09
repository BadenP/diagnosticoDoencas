% Consulta a base de conhecimento
:- consult('base.pl'). 
:- use_module(library(readutil)). 

 % Declaração dinâmica de predicados para pacientes, sintomas e diagnostico
:- dynamic paciente/3.
:- dynamic sintomaPaciente/2. 
:- dynamic diagnostico/3. 

% fatorRisco(+Doenca, ?F, +Nome) is det
% Predicado para calcular o fator de risco de uma doença.
% Mulheres possuem 1.5 vezes mais chances de desenvolver doenças neurológicas do que homens
% A cada 10 anos após os 65 anos, a probabilidade de desenvolver doenças neurológicas aumenta em 5%
fatorRisco(Doenca, F, Nome) :-
    paciente(Nome, Idade, Genero),
    probabilidade(Doenca,P),
    (Genero == 'masculino', X is P; X is P * 1.5),
    (Idade > 65, F is X + (Idade - 65) / 10 * 0.05; F is X).

% listaSintomas(+Doenca, ?L) is det
% Predicado para listar os sintomas de uma doença.
listaSintomas(Doenca, L) :-
    findall(Sintoma, sintoma(Doenca, Sintoma), L).

% contaSintoma(+Resposta, ?X, +Nome, +Sintoma) is det
% Predicado para contar o número de sintomas apresentados por um paciente, se o sintoma é contado, ele é salvo dinâmicamente.
contaSintoma(1, 1, Nome, Sintoma) :-
    assert(sintomaPaciente(Nome,Sintoma)).
contaSintoma(2, 0, _, _).

% quantidadeSintomas(+Doenca, +L, ?N, +Nome) is semidet
% Predicado para contar os N sintomas de uma Doenca que são confirmados pelo paciente identificado pelo Nome.
% A lista L é a lista de todos os sintomas da Doenca e N é a quantidade de sintomas confirmados pelo paciente.
quantidadeSintomas(_, [], 0, _).
quantidadeSintomas(Doenca, [Sintoma|Resto], N, Nome) :-
    writeln(""),
    writeln("Responda 1. ou 2. de acordo com as respostas para cada sintoma apresentado: "),
    write("Eu apresento "),
    write(Sintoma),
    writeln("."),
    writeln("1 - Sim, apresento esse sintoma."),
    writeln("2 - Não apresento esse sintoma."),
    read(Resposta),
    contaSintoma(Resposta, X, Nome, Sintoma),
    quantidadeSintomas(Doenca, Resto, N1, Nome),
    N is N1 + X.

% perguntas(+Doenca, ?P, +Nome) is det
% Predicado para fazer perguntas ao paciente sobre sintomas para gerar um diagnóstico.
% A lista de sintomas L da Doenca é passada para contar a quantidade N de sintomas do paciente, e a probabilidade P é calculada através 
% do fator de risco e a quantidade N de sintomas.
perguntas(Doenca, P, Nome) :-
    listaSintomas(Doenca, L),
    quantidadeSintomas(Doenca, L, N, Nome),
    length(L, TotalSintomas),
    F is 1 / TotalSintomas,
    PS is N * F,
    fatorRisco(Doenca,FR,Nome),
    P is PS * FR.

% listar(+Lista) is semidet
% Predicado para listar os elementos de uma lista.
listar([]).
listar([X|Y]) :-
    write("- "),
    writeln(X),
    listar(Y).

% exibirSintomas(+Nome) is det
% Predicado para exibir os sintomas apresentados por um paciente.
exibirSintomas(Nome) :-
    writeln(""),
    writeln("Sintomas apresentados pelo paciente: "),
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), ListaSintomas),
    listar(ListaSintomas).

% respostaDiagnostico(+Resposta, +Nome) is det
% Predicado para lidar com a resposta do paciente após o diagnóstico.
respostaDiagnostico(1, _).
respostaDiagnostico(2, _) :- halt.
respostaDiagnostico(3, Nome) :-
    writeln(""),
    writeln("----------------------------------------------------------------------"),
    writeln("Sintomas apresentados pelo paciente: "),
    forall(sintomaPaciente(Nome, Sintoma),(
        sintoma(Doenca, Sintoma),
        writeln(""),
        write("Doença: "),
        writeln(Doenca),
        write("Sintoma apresentado: "),
        writeln(Sintoma))).

% exibirDiagnostico(+Lista, +Nome) is det
% Predicado para exibir o diagnóstico gerado para um paciente.
exibirDiagnostico([], _).
exibirDiagnostico([DP|Resto], Nome) :-
    [Doenca|[Probabilidade|_]] = DP,
    R is Probabilidade * 100,
    write(Doenca),
    write(": "),
    write(R),
    writeln("%"),
    assert(diagnostico(Nome,Doenca,Probabilidade)),
    exibirDiagnostico(Resto,Nome),
    exibirSintomas(Nome),
    writeln("---------------------------------------------------------------------------------------------------------------------------"),
    writeln("Lembre-se que este é um diagnóstico baseado somente em sintomas e algumas probabilidades. É importante consultar um médico."),
    writeln(""),
    writeln("Deseja realizar um novo diagnóstico?"),
    writeln(""),
    writeln('1 - Sim'),
    writeln('2 - Não'),
    writeln('3 - Ver detalhes do diagnóstico'),
    writeln(""),
    read(Resposta),
    gravarDadosArquivo(Nome),
    gravarPacientes,
    respostaDiagnostico(Resposta, Nome),
    main(). % Fecha o arquivo

% lerPacientes is det
% Predicado para listar os cadastros de pacientes.
lerPacientes :-
    consult('pacientes.pl'),
    paciente(Nome, Idade, Genero),
    format('Nome: ~w, Idade: ~w, Gênero: ~w~n', [Nome, Idade, Genero]),
    fail.

% excluirPaciente(+Nome) is det
% Predicado para excluir um cadastro de paciente.
excluirPaciente(Nome) :-
    consult('pacientes.pl'),
    retract(paciente(Nome, _, _)),
    retractall(sintomaPaciente(Nome, _)),
    retractall(diagnostico(Nome, _, _)),
    atomic_concat(Nome, '.txt', NomeComExtensao),
    delete_file(NomeComExtensao),
    gravarPacientes,
    write('Cadastro de paciente excluído.'), nl.

% atualizarPaciente(+Nome, +NovaIdade, +NovoGenero) is det
% Predicado para atualizar os dados de um paciente.
atualizarPaciente(Nome, NovaIdade, NovoGenero) :-
    excluirPaciente(Nome),
    assert(paciente(Nome, NovaIdade, NovoGenero)),
    gravarPacientes,
    write('Cadastro de paciente atualizado.'), nl.

% atualizarDiagnosticoPaciente(+Nome) is det
% Predicado para atualizar o diagnóstico de um paciente.
atualizarDiagnosticoPaciente(Nome) :-
    excluirPaciente(Nome),
    write('Para atualizar o cadastro, realize um novo diagnóstico:'), 
    nl,
    main.

% gravarPacientes is det
% Predicado para gravar os pacientes em um arquivo.
gravarPacientes :-
    tell('pacientes.pl'),
    listing(paciente),
    listing(sintomaPaciente),
    listing(diagnostico),
    told.

% gravarDadosArquivo(+Nome) is det
% Predicado para gravar os dados de um paciente em um arquivo.
gravarDadosArquivo(Nome) :-
    atomic_concat(Nome, '.txt', NomeComExtensao),
    open(NomeComExtensao, append, File), % Abre o arquivo em modo de adição
    nl,
    writeln(File, "#########################################################################"),
    writeln(File, "Dados do paciente: "),
    writeln(File, "#########################################################################"),
    paciente(Nome, Idade, Genero),
    writeln(File, Nome),
    writeln(File, Idade), 
    writeln(File, Genero), 
    writeln(File, "----------------------------------------------------------------------"),
    writeln(File, "Sintomas apresentados pelo paciente e suas possíveis doenças: "),
    writeln(File, "----------------------------------------------------------------------"),
    forall(sintomaPaciente(Nome, Sintoma),(
        sintoma(Doenca, Sintoma),
        writeln(File, ""),
        write(File, "Possível doença: "),
        writeln(File, Doenca),
        write(File, "Sintoma apresentado: "),
        writeln(File, Sintoma))),
    writeln(File, ""),
    writeln(File, "----------------------------------------------------------------------"),
    writeln(File, 'Diagnóstico:'),
    writeln(File, "----------------------------------------------------------------------"),
    forall(diagnostico(Nome, Doenca, Probabilidade),(
        writeln(File, ""),
        write(File, "Doença: "),
        writeln(File, Doenca),
        write(File, "Probabilidade: "),
        P is Probabilidade * 100,
        write(File, P),
        writeln(File, "%"))),
    writeln(File, ""),
    writeln(File, "----------------------------------------------------------------------"),
    close(File).

% perguntas(+Nome, -L) is det
% Predicado para fazer perguntas ao paciente para gerar um diagnóstico.
perguntas(Nome, L) :-
    perguntas('Alzheimer', P1, Nome),
    perguntas('Ataxia', P2, Nome), 
    perguntas('Paralisia supranuclear progressiva', P3, Nome),
    L = [['Alzheimer', P1], ['Ataxia', P2], ['Paralisia supranuclear progressiva', P3]].

/*
perguntas(Nome, L) :-
    perguntas('Alzheimer', P1, Nome),
    perguntas('Ataxia', P2, Nome), 
    perguntas('Paralisia supranuclear progressiva', P3, Nome),
    perguntas('Enxaqueca', P4, Nome),       
    perguntas('Atrofia de Múltiplos Sistemas', P5, Nome),
    perguntas('Fibromialgia', P6, Nome),
    perguntas('Esclerose múltipla', P7, Nome),
    perguntas('Parkinson', P8, Nome),
    perguntas('Esclerose lateral amiotrófica', P9, Nome),
    perguntas('Epilepsia', P10, Nome),
    perguntas('AVC',P11, Nome),
    L = [['Alzheimer', P1], ['Ataxia', P2], ['Paralisia supranuclear progressiva', P3],
    ['Enxaqueca', P4], ['Atrofia de Múltiplos Sistemas', P5], ['Fibromialgia', P6], ['Esclerose múltipla', P7],
    ['Parkinson', P8], ['Esclerose lateral amiotrófica', P9], ['Epilepsia', P10], ['Doenca',P11]].
*/

% genero(+Resposta, ?Genero) is det
% Predicado para identificar o gênero do paciente.
genero(1, 'masculino').
genero(2, 'feminino').

% main is det
% Predicado principal para iniciar o sistema de diagnóstico.
main :-
    writeln("-----------------------------------------------------------------------------------------------------------------------------"),
    writeln("Seja bem-vindo ao sistema de diagnóstico de doenças neurológicas. Por favor, responda as perguntas para cadastro do paciente:"),
    writeln(""),
    writeln("Digite o nome do paciente:"),
    read(Nome),
    writeln(""),
    writeln("Digite a idade do paciente:"),
    read(Idade),
    writeln(""),
    writeln("Insira o gênero do paciente:"),
    writeln("1 - Masculino"),
    writeln("2 - Feminino"),
    read(Resposta),
    genero(Resposta, Genero),
    assert(paciente(Nome, Idade, Genero)),
    perguntas(Nome, ListaProbabilidades),
    ordenar(ListaProbabilidades, ListaOrdenada),
    writeln(""),
    writeln("----------------------------------------------------------------------------------------------------------------------"),
    writeln("Diagnóstico gerado com base nos sintomas fornecidos: "),
    writeln(""),
    filtrar(ListaOrdenada, ListaOrdenadaFiltrada),
    exibirDiagnostico(ListaOrdenadaFiltrada, Nome). % Fecha o arquivo

% decrescente(+Elemento, +Lista, ?ListaOrdenada) is det
% Predicado para inserir um elemento em uma lista ordenada em ordem decrescente pelo segundo elemento de cada sublista
decrescente(Elemento, [], [Elemento]) :- !.
decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome1, Valor1], [Nome2, Valor2] | Resto]) :-
    Valor1 >= Valor2, !.
decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome2, Valor2] | CaudaOrdenada]) :-
    Valor1 < Valor2,
    decrescente([Nome1, Valor1], Resto, CaudaOrdenada), !.

% ordenar(+Lista, ?ListaOrdenada) is det
% Predicado para ordenar uma lista em ordem decrescente pelo segundo elemento de cada sublista
ordenar([], []) :- !.
ordenar([Elemento], [Elemento]) :- !.
ordenar([Cabeça|Cauda], ListaOrdenada) :-
    ordenar(Cauda, CaudaOrdenada),
    decrescente(Cabeça, CaudaOrdenada, ListaOrdenada).

% filtrar(+Lista, ?ListaFiltrada) is det
% Predicado para filtrar os diagnósticos com probabilidade maior do que zero.
filtrar([], []).
filtrar([X|Xs], [X|Ys]) :-
    [_|[P|_]] = X,
    P > 0,
    filtrar(Xs, Ys).

filtrar([X|Xs], Ys) :-
    [_|[P|_]] = X,
    P =< 0,
    filtrar(Xs, Ys).
