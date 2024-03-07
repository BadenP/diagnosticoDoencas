:- dynamic(doencas_registradas/1). % Declaração dinâmica para manter o conjunto de doenças registradas

paciente('Carla', 80, 'F').
paciente('Joao', 20, 'M').
paciente('Maria', 30, 'F').
sintomaPaciente('Carla', 'febre').
sintomaPaciente('Carla', 'tosse').
sintomaPaciente('Carla', 'dor_de_cabeca').
sintomaPaciente('Joao', 'febre').
sintomaPaciente('Joao', 'tosse').
sintoma('gripe', 'febre').
sintoma('gripe', 'tosse').
sintoma('gripe', 'dor_de_cabeca').

sintomasPaciente(Nome, L) :-
    findall(Sintoma, sintomaPaciente(Nome, Sintoma), L).

gravarDadosArquivo(Nome) :-
    open('teste.txt', write, Arquivo), % Abre o arquivo em modo de escrita
    paciente(Nome, Idade, Genero),
    writeln(Arquivo, "Dados cadastrais do paciente"),
    write(Arquivo, "Nome: "),
    writeln(Arquivo, Nome),
    write(Arquivo, "Idade: "),
    writeln(Arquivo, Idade), % Supondo que Idade seja uma variável global preenchida na função main()
    write(Arquivo, "Gênero: "),
    writeln(Arquivo, Genero), % Supondo que Genero seja uma variável global preenchida na função main()
    writeln(Arquivo, "----------------------------------------------------------------------"),
    writeln(Arquivo, 'Diagnóstico:'),
    sintomasPaciente(Nome, ListaSintomas), % Aqui corrigimos para passar a lista diretamente
    writeln(ListaSintomas), % Verifique se a lista está correta
    retractall(doencas_registradas(_)), % Limpa a base de conhecimento das doenças registradas
    gravarSintomas(Arquivo, Nome, ListaSintomas), % Aqui corrigimos para passar a lista diretamente
    close(Arquivo).

gravarSintomas(_, _, []).
gravarSintomas(Arquivo, Nome, [Sintoma|Resto]) :-
    sintoma(Doenca, Sintoma),
    \+ doencas_registradas(Doenca), % Verifica se a doença já foi registrada
    asserta(doencas_registradas(Doenca)), % Registra a doença
    write(Arquivo, "Doença: "),
    writeln(Arquivo, Doenca),
    writeln(Arquivo, "Sintomas apresentados:"),
    findall(SintomasDoenca, sintoma(Doenca, SintomasDoenca), ListaSintomasDoenca),
    listarSintomas(Arquivo, ListaSintomasDoenca, Nome),
    gravarSintomas(Arquivo, Nome, Resto).

listarSintomas(_, [], _).
listarSintomas(Arquivo, [Sintoma|Resto], Nome) :-
    sintomaPaciente(Nome, Sintoma),
    writeln(Arquivo, Sintoma),
    listarSintomas(Arquivo, Resto, Nome).

listarSintomas(Arquivo, [Sintoma|Resto], Nome) :-
    \+ sintomaPaciente(Nome, Sintoma),
    listarSintomas(Arquivo, Resto, Nome).
