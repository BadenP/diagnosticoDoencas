% Consulta a base de conhecimento
:- consult('base.pl'). 
:- use_module(library(readutil)). 

 % Declaração dinâmica de predicados para pacientes, sintomas e diagnostico
:- dynamic paciente/3.
:- dynamic sintomaPaciente/2. 
:- dynamic diagnostico/3. 

% fatorRiscoGenero(?Genero, ?F) is semidet
% Predicado para calcular o fator de risco de acordo com o gênero do paciente.
fatorRiscoGenero('masculino', 1).
fatorRiscoGenero('feminino', 1.5).

% fatorRiscoIdade(+Idade, +X, ?FD) is det
% Predicado para calcular o fator de risco de acordo com a idade do paciente.
fatorRiscoIdade(Idade, X, X) :-
    Idade =< 65, !.
fatorRiscoIdade(Idade, X, FD) :-
    Idade > 65, 
    FD is X + (Idade - 65) / 10 * 0.1, !.

% fatorRisco(+Doenca, ?F, +Nome) is semidet
% Predicado para calcular o fator de risco de uma doença.
% Mulheres possuem 1.5 vezes mais chances de desenvolver doenças neurológicas do que homens
% A cada 10 anos após os 65 anos, a probabilidade de desenvolver doenças neurológicas aumenta em 10%
fatorRisco(Doenca, F, Nome) :-
    paciente(Nome, I, Genero),
    probabilidade(Doenca,P),
    atom_number(I, Idade),
    fatorRiscoGenero(Genero, FG),
    X is P * FG,
    fatorRiscoIdade(Idade, X, F).

% listaSintomas(+Doenca, ?L) is semidet
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
    nl,
    writeln("Responda 1. ou 2. de acordo com as respostas para cada sintoma apresentado: "),
    write("Eu apresento "),
    write(Sintoma),
    writeln("."),
    writeln("1 - Sim, apresento esse sintoma."),
    writeln("2 - Não apresento esse sintoma."),
    read_line_to_string(user_input, RespostaString),
    atom_number(RespostaString, Resposta),
    contaSintoma(Resposta, X, Nome, Sintoma),
    quantidadeSintomas(Doenca, Resto, N1, Nome),
    N is N1 + X.

% perguntas(+Doenca, ?P, +Nome) is semidet
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

% listar(+Lista) is det
% Predicado para listar os elementos de uma lista.

listar([]).
listar([X|Y]) :-
    write("- "),
    writeln(X),
    listar(Y).

% listarDetalhes(+Lista, +Nome) is semidet

listarDetalhes([],_).
listarDetalhes([X|Y],Nome) :-
    sintomaPaciente(Nome, X),
    write("- "),
    write(X),
    writeln(" (apresentado pelo paciente)"),
    listarDetalhes(Y,Nome).
listarDetalhes([X|Y],Nome) :-
    \+ sintomaPaciente(Nome, X),
    write("- "),
    write(X),
    writeln(" (não apresentado pelo paciente)"),
    listarDetalhes(Y,Nome).

% listarDetalhesDiagnostico(+Lista) is det
% Predicado para listar os detalhes do diagnóstico gerado para um paciente.
listarDetalhesDiagnostico([]).
listarDetalhesDiagnostico([X|Y]) :-
    nl,
    sintoma(Doenca,X),
    write("Sintoma apresentado: "),
    writeln(X),
    write("Doença relacionada: "),
    writeln(Doenca),
    listarDetalhesDiagnostico(Y).

% exibirSintomas(+Nome) is det
% Predicado para exibir os sintomas apresentados por um paciente.
exibirSintomas(Nome) :-
    nl,
    writeln("Sintomas apresentados pelo paciente: "),
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), ListaSintomas),
    listar(ListaSintomas).


% respostaDoenca(+Resposta, ?Doenca) is semidet
% identificar a doença escolhida pelo paciente.
espostaDoenca(1, 'Alzheimer').
respostaDoenca(2, 'Ataxia').
respostaDoenca(3, 'Paralisia supranuclear progressiva').
respostaDoenca(4, 'Enxaqueca').
respostaDoenca(5, 'Atrofia de Múltiplos Sistemas').
respostaDoenca(6, 'Fibromialgia').
respostaDoenca(7, 'Esclerose múltipla').
respostaDoenca(8, 'Parkinson').
respostaDoenca(9, 'Esclerose lateral amiotrófica').
respostaDoenca(10, 'Epilepsia').
respostaDoenca(11, 'AVC').

% detalhesDoenca(+Resposta) is det
detalhesDoenca(R,Nome) :-
    nl,
    respostaDoenca(R, Doenca),
    write("Sintomas da doença "),
    write(Doenca),
    writeln(":"),
    listaSintomas(Doenca, L),
    listarDetalhes(L,Nome).

% visualizarDoencas(+Nome) is semidet
% Predicado para visualizar as doenças e escolher uma para ver mais detalhes.
visualizarDoencas(Nome) :-
    nl,
    writeln("Para ver mais detalhes dos sintomas das outras doenças, digite o número da doença de interesse abaixo:"),
    writeln("1 - Alzheimer"),
    writeln("2 - Ataxia"),
    writeln("3 - Paralisia supranuclear progressiva"),
    writeln("4 - Enxaqueca"),
    writeln("5 - Atrofia de Múltiplos Sistemas"),
    writeln("6 - Fibromialgia"),
    writeln("7 - Esclerose múltipla"),
    writeln("8 - Parkinson"),
    writeln("9 - Esclerose lateral amiotrófica"),
    writeln("10 - Epilepsia"),
    writeln("11 - AVC"),
    read_line_to_string(user_input, RespostaString),
    atom_number(RespostaString, Resposta),
    detalhesDoenca(Resposta,Nome).

% mensagemFinal(+Nome) is semidet
% Predicado para exibir a mensagem final do diagnóstico.
mensagemFinal(Nome) :-
    nl,
    writeln("Deseja realizar um novo diagnóstico?"),
    nl,
    writeln('1 - Sim'),
    writeln('2 - Não'),
    writeln('3 - Ver detalhes do meu diagnóstico'),
    writeln('4 - Ver mais detalhes sobre outras doenças'),
    nl,
    read_line_to_string(user_input, RespostaString),
    atom_number(RespostaString, Resposta),
    respostaDiagnostico(Resposta, Nome).

% respostaDiagnostico(+Resposta, +Nome) is semidet
% Predicado para lidar com a resposta do paciente após o diagnóstico.

respostaDiagnostico(4,Nome) :-
    visualizarDoencas(Nome),
    mensagemFinal(Nome).

respostaDiagnostico(1, _) :- consultarDiagnostico.
respostaDiagnostico(2, _) :- halt.
respostaDiagnostico(3, Nome) :-
    nl,
    writeln("----------------------------------------------------------------------"),
    writeln("Sintomas apresentados pelo paciente: "),
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), ListaSintomas),
    listarDetalhesDiagnostico(ListaSintomas),
    visualizarDoencas(Nome),
    mensagemFinal(Nome).

% exibirDiagnostico(+Lista, +Nome) is semidet
% Predicado para exibir o diagnóstico gerado para um paciente.
exibirDiagnostico([], _).
exibirDiagnostico([DP|Resto], Nome) :-
    [Doenca|[Probabilidade|_]] = DP,
    R is Probabilidade * 100,
    write(Doenca),
    write(": "),
    write(R),
    writeln("%"),
    assert(diagnostico(Nome,Doenca,R)),
    exibirDiagnostico(Resto,Nome),
    exibirSintomas(Nome),
    writeln("---------------------------------------------------------------------------------------------------------------------------"),
    writeln("Lembre-se que este é um diagnóstico baseado somente em sintomas e algumas probabilidades. É importante consultar um médico."),
    nl,
    writeln("Deseja realizar um novo diagnóstico?"),
    nl,
    writeln('1 - Sim'),
    writeln('2 - Não'),
    writeln('3 - Ver detalhes do diagnóstico'),
    nl,
    read_line_to_string(user_input, RespostaString),
    atom_number(RespostaString, Resposta),
    gravarPacientes,
    gravarDadosArquivo(Nome),
    respostaDiagnostico(Resposta, Nome). 

% lerPacientes is nondet
% Predicado para listar os cadastros de pacientes.
lerPacientes :-
    consult('pacientes.pl'),
    paciente(Nome, Idade, Genero),
    format('~w, ~w anos, gênero ~w~n', [Nome, Idade, Genero]).

% excluirPaciente(+Nome) is semidet
% Predicado para excluir um cadastro de paciente.
excluirPaciente(Nome) :-
    consult('pacientes.pl'),
    retract(paciente(Nome, _, _)),
    retractall(sintomaPaciente(Nome, _)),
    retractall(diagnostico(Nome, _, _)),
    gravarPacientes,
    excluirArquivoPaciente(Nome),
    write('Cadastro de paciente excluído.'), nl.

% excluirArquivoPaciente(+Nome) is semidet
% Predicado para excluir o arquivo de um paciente.
excluirArquivoPaciente(Nome):-
    atomic_concat(Nome, '.txt', NomeComExtensao),
    delete_file(NomeComExtensao).

% atualizarPaciente(+Nome, +NovaIdade, +NovoGenero) is semidet
% Predicado para atualizar os dados de um paciente.
atualizarPaciente(Nome, NovaIdade, NovoGenero) :-
    consult('pacientes.pl'),
    retract(paciente(Nome, _, _)),
    assert(paciente(Nome, NovaIdade, NovoGenero)),
    gravarPacientes,
    write('Cadastro de paciente atualizado. Para gerar um novo arquivo do paciente atualize o diagnóstico do paciente.'), nl.

% atualizarDiagnosticoPaciente(+Nome) is semidet
% Predicado para atualizar o diagnóstico de um paciente.
atualizarDiagnosticoPaciente(Nome) :-
    excluirPaciente(Nome),
    write('Para atualizar o cadastro, realize um novo diagnóstico:'), 
    nl,
    consultarDiagnostico.

% gravarPacientes is det
% Predicado para gravar os pacientes em um arquivo.
gravarPacientes :-
    tell('pacientes.pl'),
    listing(paciente),
    listing(sintomaPaciente),
    listing(diagnostico),
    told.

% gravarSintomas(+File, +Lista) is semidet
% Predicado para gravar os sintomas de um paciente em um arquivo.
gravarSintomas(_, []).
gravarSintomas(File, [X|Y]) :-
    writeln(File, ""),
    write(File, "Sintoma apresentado: "),
    writeln(File, X),
    sintoma(Doenca, X),
    write(File, "Doença relacionada: "),
    writeln(File, Doenca),
    gravarSintomas(File, Y).

% gravarDiagnostico(+File, +Lista, +Nome) is semidet
% Predicado para gravar o diagnóstico de um paciente em um arquivo.
gravarDiagnostico(_, [], _).
gravarDiagnostico(File, [X|Y], Nome) :-
    diagnostico(Nome, D, X),
    writeln(File, ""),
    write(File, "Doença: "),
    writeln(File, D),
    write(File, "Probabilidade: "),
    write(File, X),
    writeln(File, "%"),
    gravarDiagnostico(File, Y, Nome).

% gravarDadosArquivo(+Nome) is semidet
% Predicado para gravar os dados de um paciente em um arquivo.
gravarDadosArquivo(Nome) :-
    atomic_concat(Nome, '.txt', NomeComExtensao),
    open(NomeComExtensao, append, File), % Abre o arquivo em modo de adição
    nl,
    writeln(File, "-------------------------------------------------------------------------"),
    writeln(File, "Dados do paciente: "),
    writeln(File, "-------------------------------------------------------------------------"),
    paciente(Nome, Idade, Genero),
    writeln(File, Nome),
    writeln(File, Idade), 
    writeln(File, Genero), 
    writeln(File, "----------------------------------------------------------------------"),
    writeln(File, "Sintomas apresentados pelo paciente e suas possíveis doenças: "),
    writeln(File, "----------------------------------------------------------------------"),
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), ListaSintomas),
    gravarSintomas(File, ListaSintomas),
    writeln(File, ""),
    writeln(File, "----------------------------------------------------------------------"),
    writeln(File, 'Diagnóstico:'),
    writeln(File, "----------------------------------------------------------------------"),
    findall(Probabilidade, diagnostico(Nome, _, Probabilidade), ListaDiagnostico),
    gravarDiagnostico(File, ListaDiagnostico, Nome),
    writeln(File, ""),
    writeln(File, "----------------------------------------------------------------------"),
    close(File).

% perguntas(+Nome, -L) is semidet
% Predicado para fazer perguntas ao paciente para gerar um diagnóstico.

/*
Este trecho das perguntas utilizei para fazer testes mais curtos, com número menor de doenças
perguntas(Nome, L) :-
    perguntas('Alzheimer', P1, Nome),
    perguntas('Ataxia', P2, Nome), 
    perguntas('Paralisia supranuclear progressiva', P3, Nome),
    L = [['Alzheimer', P1], ['Ataxia', P2], ['Paralisia supranuclear progressiva', P3]].
*/

perguntas(Nome, L) :-
    perguntas('Alzheimer', P1, Nome),
    perguntas('Ataxia', P2, Nome), 
    perguntas('Paralisia supranuclear progressiva', P3, Nome),
    perguntas('Enxaqueca', P4, Nome),       
    perguntas('Atrofia de múltiplos sistemas', P5, Nome),
    perguntas('Fibromialgia', P6, Nome),
    perguntas('Doença de Huntington', P7, Nome),
    perguntas('Esclerose múltipla', P8, Nome),
    perguntas('Parkinson', P9, Nome),
    perguntas('Epilepsia', P10, Nome),
    perguntas('AVC',P11, Nome),
    L = [['Alzheimer', P1], ['Ataxia', P2], ['Paralisia supranuclear progressiva', P3], ['Enxaqueca', P4],
    ['Atrofia de Múltiplos Sistemas', P5], ['Fibromialgia', P6], ['Doença de Huntington', P7],
    ['Esclerose múltipla', P8],['Parkinson', P9], ['Epilepsia', P10], ['AVC',P11]].


% genero(+Resposta, ?Genero) is semidet
% Predicado para identificar o gênero do paciente.
genero(1, 'masculino').
genero(2, 'feminino').

% consultarDiagnostico is semidet
% Predicado principal para iniciar o sistema de diagnóstico.
consultarDiagnostico :-
    nl,
    writeln("-----------------------------------------------------------------------------------------------------------------------------"),
    writeln("Seja bem-vindo ao sistema de diagnóstico de doenças neurológicas. Por favor, responda as perguntas para cadastro do paciente:"),
    nl,
    writeln("Digite o nome do paciente:"),
    read_line_to_string(user_input,Nome),
    nl,
    writeln("Digite a idade do paciente:"),
    read_line_to_string(user_input,Idade),
    nl,
    writeln("Insira o gênero do paciente:"),
    writeln("1 - Masculino"),
    writeln("2 - Feminino"),
    read_line_to_string(user_input, RespostaString),
    atom_number(RespostaString, Resposta),
    genero(Resposta, Genero),
    assert(paciente(Nome, Idade, Genero)),
    perguntas(Nome, ListaProbabilidades),
    ordenar(ListaProbabilidades, ListaOrdenada),
    nl,
    writeln("----------------------------------------------------------------------------------------------------------------------"),
    writeln("Diagnóstico gerado com base nos sintomas fornecidos: "),
    nl,
    filtrar(ListaOrdenada, ListaOrdenadaFiltrada),
    exibirDiagnostico(ListaOrdenadaFiltrada, Nome). 

% decrescente(+Elemento, +Lista, ?ListaOrdenada) is semidet
% Predicado para inserir um elemento em uma lista ordenada em ordem decrescente pelo segundo elemento de cada sublista
decrescente(Elemento, [], [Elemento]) :- !.
decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome1, Valor1], [Nome2, Valor2] | Resto]) :-
    Valor1 >= Valor2, !.
decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome2, Valor2] | XOrdenada]) :-
    Valor1 < Valor2,
    decrescente([Nome1, Valor1], Resto, XOrdenada), !.

% ordenar(+Lista, ?ListaOrdenada) is semidet
% Predicado para ordenar uma lista em ordem decrescente pelo segundo elemento de cada sublista
ordenar([], []) :- !.
ordenar([Elemento], [Elemento]) :- !.
ordenar([X|Xs], ListaOrdenada) :-
    ordenar(X, XOrdenada),
    decrescente(X, XOrdenada, ListaOrdenada).

% filtrar(+Lista, ?ListaFiltrada) is semidet
% Predicado para filtrar os diagnósticos com probabilidade maior do que zero.
filtrar([], []).
filtrar([X|Xs], [X|Ys]) :-
    [_|[P|_]] = X,
    P > 0,
    !,
    filtrar(Xs, Ys).
filtrar([_|Xs], Ys) :-
    filtrar(Xs, Ys).

% Predicado para exibir o menu principal do programa.
programa :-
    writeln("Programa de diagnóstico de doenças neurológicas:"),
    writeln("- Para realizar diagnósticos, digite consultarDiagnostico."),
    writeln("- Para listar os pacientes cadastrados, digite lerPacientes."),
    writeln("- Para excluir um paciente, digite excluirPaciente(Nome)."),
    writeln("- Para atualizar um paciente, digite atualizarPaciente(Nome, NovaIdade, NovoGenero)."),
    writeln("- Para atualizar o diagnóstico de um paciente, digite atualizarDiagnosticoPaciente(Nome).").

:- programa.

