:- use_module(library(plunit)).

% Base de conhecimento

% Sintomas da doença de Alzheimer
sintoma('Alzheimer', 'lapsos de memória').
sintoma('Alzheimer', 'dificuldade em reconhecer pessoas').
sintoma('Alzheimer', 'dificuldade em recordar palavras').
sintoma('Alzheimer', 'repetição frequente de perguntas').
sintoma('Alzheimer', 'dificuldade em tomar decisões').
sintoma('Alzheimer', 'confusão e desorientação').
sintoma('Alzheimer', 'delírios').
sintoma('Alzheimer', 'paranoia e desconfiança').
sintoma('Alzheimer', 'distúrbios do sono').
sintoma('Alzheimer', 'problemas de memória a curto e longo prazo').

% Sintomas da Ataxia
sintoma('Ataxia', 'quedas com frequência').
sintoma('Ataxia', 'incapacidade de andar em linha reta').
sintoma('Ataxia', 'desajeitamento').
sintoma('Ataxia', 'fala arrastada').
sintoma('Ataxia', 'dificuldades para engolir').
sintoma('Ataxia', 'tremores ou agitação').
sintoma('Ataxia', 'fadiga').
sintoma('Ataxia', 'problemas de visão').

% Sintomas de Paralisia Supranuclear Progressiva
sintoma('Paralisia supranuclear progressiva', 'quedas repetidas').
sintoma('Paralisia supranuclear progressiva', 'rigidez muscular').
sintoma('Paralisia supranuclear progressiva', 'problemas oculares').
sintoma('Paralisia supranuclear progressiva', 'tremores').
sintoma('Paralisia supranuclear progressiva', 'lentidão de pensamento').
sintoma('Paralisia supranuclear progressiva', 'fala mais lenta e arrastada').
sintoma('Paralisia supranuclear progressiva', 'problemas de mobilidade').
sintoma('Paralisia supranuclear progressiva', 'dificuldade no controle muscular da boca língua e garganta').
sintoma('Paralisia supranuclear progressiva', 'problemas intestinais e urinários').

% Sintomas de uma Enxaqueca
sintoma('Enxaqueca', 'dor intensa na cabeça').
sintoma('Enxaqueca', 'dor pulsante').
sintoma('Enxaqueca', 'náuseas').
sintoma('Enxaqueca', 'vômitos').
sintoma('Enxaqueca', 'sensibilidade a luz e som').

% Sintomas de Atrofia de multiplos sistemas
sintoma('Atrofia de Múltiplos Sistemas', 'problemas de coordenação').
sintoma('Atrofia de Múltiplos Sistemas', 'tremor, lentidão e rigidez').
sintoma('Atrofia de Múltiplos Sistemas', 'risos ou choro incontroláveis').
sintoma('Atrofia de Múltiplos Sistemas', 'respiração ruidosa').
sintoma('Atrofia de Múltiplos Sistemas', 'suspiros involuntários').
sintoma('Atrofia de Múltiplos Sistemas', 'voz fraca').
sintoma('Atrofia de Múltiplos Sistemas', 'visão turva').
sintoma('Atrofia de Múltiplos Sistemas', 'mãos e pés frios').

fatorRisco('Alzheimer', 0.5).
fatorRisco('Ataxia', 0.3).
fatorRisco('Paralisia supranuclear progressiva', 0.2).
fatorRisco('Enxaqueca', 0.1).
fatorRisco('Atrofia de Múltiplos Sistemas', 0.4).

probabilidade('Alzheimer', 0.05).
probabilidade('Ataxia', 0.03).
probabilidade('Paralisia supranuclear progressiva', 0.02).
probabilidade('Enxaqueca', 0.5).
probabilidade('Atrofia de Múltiplos Sistemas', 0.4).

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
    probabilidade(Doenca,PD),
    fatorRisco(Doenca,FR),
    P is PS * PD * FR.

exibirSintomas(Nome) :-
    writeln(""),
    writeln("Sintomas apresentados pelo paciente: "),
    forall(sintomaPaciente(Nome, Sintoma), writeln(Sintoma)).

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
    write(R),
    writeln("%"),
    assert(diagnostico(Nome,Doenca,Probabilidade)),
    exibirDiagnostico(Resto,Nome),
    exibirSintomas(Nome),
    writeln("---------------------------------------------------------------------------------------------------------------------------"),
    writeln("Lembre-se que este é um diagnóstico baseado somente em sintomas e algumas probabilidades. É importante consultar um médico."),
    writeln(""),
    writeln("Deseja realizar um novo diagnóstico?"),
    writeln('1 - Sim'),
    writeln('2 - Não'),
    writeln('3 - Ver detalhes do diagnóstico'),
    writeln(""),
    read(Resposta),
    gravarDadosArquivo(Nome),
    respostaDiagnostico(Resposta,Nome),
    main().

gravarDadosArquivo(Nome) :-
    open('pacientes.txt', write, File), % Abre o arquivo em modo de adição
    paciente(Nome, Idade, Genero),
    writeln(File, Nome),
    writeln(File, Idade), 
    writeln(File, Genero), 
    writeln(File, "----------------------------------------------------------------------"),
    writeln("Sintomas apresentados pelo paciente e suas possíveis doenças: "),
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
    forall(diagnostico(Nome, Doenca, Probabilidade),(
        writeln(File, ""),
        write(File, "Doença: "),
        writeln(File, Doenca),
        write(File, "Probabilidade: "),
        P is Probabilidade * 100,
        write(File, P),
        writeln(File, "%"))),
    writeln(File, ""),
    writeln(File, 'Todos os sintomas apresentados pelo paciente: '),
    forall(sintomaPaciente(Nome, Sintoma), writeln(File, Sintoma)),
    close(File).

perguntas(Nome, L) :-
    perguntas('Alzheimer', P1, Nome),
    perguntas('Ataxia', P2, Nome), 
    perguntas('Paralisia supranuclear progressiva', P3, Nome),
    perguntas('Enxaqueca', P4, Nome),       
    perguntas('Atrofia de Múltiplos Sistemas', P5, Nome),
    L = [['Alzheimer', P1], ['Ataxia', P2], ['Paralisia supranuclear progressiva', P3],
    ['Enxaqueca', P4], ['Atrofia de Múltiplos Sistemas', P5]].

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
    writeln("Digite o gênero do paciente (masculino/feminino):"),
    read(Genero),
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


:- main.