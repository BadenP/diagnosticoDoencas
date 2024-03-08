
:- consult('base.pl').
:- use_module(library(readutil)).
:- dynamic paciente/3.
:- dynamic sintomaPaciente/2.
:- dynamic diagnostico/3.

fatorRisco(Doenca, F, Nome) :-
    paciente(Nome,Idade,Genero),
    probabilidade(Doenca,P),
    (Genero == 'masculino', X is P; X is P * 1.5),
    (Idade > 65, F is X + (Idade - 65) / 10 * 0.05; F is X).

listaSintomas(Doenca,L) :-
    findall(Sintoma, sintoma(Doenca, Sintoma), L).

contaSintoma(1, 1, Nome, Sintoma) :-
    assert(sintomaPaciente(Nome,Sintoma)).

contaSintoma(2, 0, _, _).

% Predicado para contar sintomas e salvar seus nomes em uma lista
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

perguntas(Doenca, P, Nome) :-
    listaSintomas(Doenca, L),
    quantidadeSintomas(Doenca, L, N, Nome),
    length(L, TotalSintomas),
    F is 1 / TotalSintomas,
    PS is N * F,
    fatorRisco(Doenca,FR,Nome),
    P is PS * FR.

listar([]).
listar([X|Y]) :-
    write("- "),
    writeln(X),
    listar(Y).

exibirSintomas(Nome) :-
    writeln(""),
    writeln("Sintomas apresentados pelo paciente: "),
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), ListaSintomas),
    listar(ListaSintomas).

respostaDiagnostico(1,_).
respostaDiagnostico(2,_) :- halt.
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

exibirDiagnostico([],_).
exibirDiagnostico([DP|Resto],Nome) :-
    [Doenca|[Probabilidade|_]] = DP,
    write(Doenca),
    write(": "),
    R is Probabilidade * 100,
    R > 0,
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
    respostaDiagnostico(Resposta,Nome),
    main().

gravarDadosArquivo(Nome) :-
    open('pacientes.txt', append, File), % Abre o arquivo em modo de adição
    writeln(File, ""),
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

genero(1, 'masculino').
genero(2, 'feminino').

main() :-
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
    writeln("A doença mais provável é: "),
    writeln(""),
    exibirDiagnostico(ListaOrdenada,Nome). % Fecha o arquivo
    
% Predicado para inserir um elemento em uma lista ordenada em ordem decrescente pelo segundo elemento de cada sublista
decrescente(Elemento, [], [Elemento]) :- !.

decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome1, Valor1], [Nome2, Valor2] | Resto]) :-
    Valor1 >= Valor2, !.

decrescente([Nome1, Valor1], [[Nome2, Valor2] | Resto], [[Nome2, Valor2] | CaudaOrdenada]) :-
    Valor1 < Valor2,
    decrescente([Nome1, Valor1], Resto, CaudaOrdenada), !.

% Predicado para ordenar uma lista em ordem decrescente pelo segundo elemento de cada sublista
ordenar([], []) :- !.
ordenar([Elemento], [Elemento]) :- !.
ordenar([Cabeça|Cauda], ListaOrdenada) :-
    ordenar(Cauda, CaudaOrdenada),
    decrescente(Cabeça, CaudaOrdenada, ListaOrdenada).

% Predicado para atualizar os dados de um paciente no arquivo
atualizarPacienteArquivo(Nome, Idade, Genero) :-
    carregarPacientes, % Carrega todos os pacientes do arquivo para a memória
    retract(paciente(Nome, _, _)), % Remove os dados antigos do paciente da memória
    assert(paciente(Nome, Idade, Genero)), % Insere os novos dados do paciente na memória
    reescreverPacientes. % Reescreve todos os pacientes no arquivo com os dados atualizados

excluirPacienteArquivo(Nome, Idade, Genero) :-
    carregarPacientes, % Carrega todos os pacientes do arquivo para a memória
    retract(paciente(Nome, _, _)), % Remove os dados antigos do paciente da memória
    reescreverPacientes. % Reescreve todos os pacientes no arquivo com os dados atualizados

% Predicado para reescrever todos os pacientes no arquivo com os dados atualizados
reescreverPacientes :-
    open('pacientes.txt', write, File), % Abre o arquivo para escrita
    forall(paciente(Nome, Idade, Genero), (
        writeln(File, Nome),
        writeln(File, Idade),
        writeln(File, Genero),
        writeln(File, "----------------------------------------------------------------------")
    )),
    close(File). % Fecha o arquivo

% Predicado para carregar todos os pacientes do arquivo para a memória
carregarPacientes :-
    open('pacientes.txt', read, File), % Abre o arquivo para leitura
    repeat,
    read(File, Nome),
    read(File, Idade),
    read(File, Genero),
    assert(paciente(Nome, Idade, Genero)), % Insere os dados do paciente na memória
    read(File, _), % Descarta a linha de separação
    at_end_of_stream(File), !, % Verifica se chegou ao final do arquivo
    close(File). % Fecha o arquivo

:- main.
