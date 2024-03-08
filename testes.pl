:- use_module(library(plunit)).
:- include('/home/paula/Documents/prolog/base.pl').

% Testes para o predicado sintoma/2
:- begin_tests(sintoma).

test(sintoma_alzheimer) :-
    sintoma('Alzheimer', 'lapsos de memória').

test(sintoma_ataxia) :-
    sintoma('Ataxia', 'quedas com frequência').

test(sintoma_paralisia_supranuclear_progressiva) :-
    sintoma('Paralisia supranuclear progressiva', 'quedas repetidas').

test(sintoma_enxaqueca) :-
    sintoma('Enxaqueca', 'dor intensa na cabeça').

test(sintoma_atrofia_multiplos_sistemas) :-
    sintoma('Atrofia de Múltiplos Sistemas', 'problemas de coordenação').

:- end_tests(sintoma).

% Testes para o predicado fatorRisco/2
:- begin_tests(fatorRisco).

test(fator_risco_alzheimer) :-
    fatorRisco('Alzheimer', 0.5).

test(fator_risco_ataxia) :-
    fatorRisco('Ataxia', 0.3).

test(fator_risco_paralisia_supranuclear_progressiva) :-
    fatorRisco('Paralisia supranuclear progressiva', 0.2).

test(fator_risco_enxaqueca) :-
    fatorRisco('Enxaqueca', 0.1).

test(fator_risco_atrofia_multiplos_sistemas) :-
    fatorRisco('Atrofia de Múltiplos Sistemas', 0.4).

:- end_tests(fatorRisco).

% Testes para o predicado probabilidade/2
:- begin_tests(probabilidade).

test(probabilidade_alzheimer) :-
    probabilidade('Alzheimer', 0.05).

test(probabilidade_ataxia) :-
    probabilidade('Ataxia', 0.03).

test(probabilidade_paralisia_supranuclear_progressiva) :-
    probabilidade('Paralisia supranuclear progressiva', 0.02).

test(probabilidade_enxaqueca) :-
    probabilidade('Enxaqueca', 0.5).

test(probabilidade_atrofia_multiplos_sistemas) :-
    probabilidade('Atrofia de Múltiplos Sistemas', 0.4).

:- end_tests(probabilidade).

% Testes para o predicado listaSintomas/2
:- begin_tests(listaSintomas).

test(lista_sintomas_alzheimer) :-
    listaSintomas('Alzheimer', ['lapsos de memória', 'dificuldade em reconhecer pessoas', 'dificuldade em recordar palavras', 'repetição frequente de perguntas', 'dificuldade em tomar decisões', 'confusão e desorientação', 'delírios', 'paranoia e desconfiança', 'distúrbios do sono', 'problemas de memória a curto e longo prazo']).

test(lista_sintomas_ataxia) :-
    listaSintomas('Ataxia', ['quedas com frequência', 'incapacidade de andar em linha reta', 'desajeitamento', 'fala arrastada', 'dificuldades para engolir', 'tremores ou agitação', 'fadiga', 'problemas de visão']).

test(lista_sintomas_paralisia_supranuclear_progressiva) :-
    listaSintomas('Paralisia supranuclear progressiva', ['quedas repetidas', 'rigidez muscular', 'problemas oculares', 'tremores', 'lentidão de pensamento', 'fala mais lenta e arrastada', 'problemas de mobilidade', 'dificuldade no controle muscular da boca língua e garganta', 'problemas intestinais e urinários']).

test(lista_sintomas_enxaqueca) :-
    listaSintomas('Enxaqueca', ['dor intensa na cabeça', 'dor pulsante', 'náuseas', 'vômitos', 'sensibilidade a luz e som']).

test(lista_sintomas_atrofia_multiplos_sistemas) :-
    listaSintomas('Atrofia de Múltiplos Sistemas', ['problemas de coordenação', 'tremor, lentidão e rigidez', 'risos ou choro incontroláveis', 'respiração ruidosa', 'suspiros involuntários', 'voz fraca', 'visão turva', 'mãos e pés frios']).

:- end_tests(listaSintomas).

% Testes para o predicado quantidadeSintomas/4
:- begin_tests(quantidadeSintomas).

test(quantidade_sintomas_alzheimer) :-
    quantidadeSintomas('Alzheimer', ['lapsos de memória', 'dificuldade em reconhecer pessoas'], 2, 'Paula').

test(quantidade_sintomas_ataxia) :-
    quantidadeSintomas('Ataxia', ['quedas com frequência', 'incapacidade de andar em linha reta', 'desajeitamento'], 3, 'Paula').

test(quantidade_sintomas_paralisia_supranuclear_progressiva) :-
    quantidadeSintomas('Paralisia supranuclear progressiva', ['quedas repetidas', 'rigidez muscular', 'problemas oculares'], 3, 'Paula').

test(quantidade_sintomas_enxaqueca) :-
    quantidadeSintomas('Enxaqueca', ['dor intensa na cabeça', 'dor pulsante', 'náuseas'], 3, 'Paula').

test(quantidade_sintomas_atrofia_multiplos_sistemas) :-
    quantidadeSintomas('Atrofia de Múltiplos Sistemas', ['problemas de coordenação', 'tremor, lentidão e rigidez', 'risos ou choro incontroláveis'], 3, 'Paula').

:- end_tests(quantidadeSintomas).

% Testes para o predicado perguntas/3
:- begin_tests(perguntas).

test(perguntas_alzheimer) :-
    perguntas('Alzheimer', 0.05, 'Paula').

test(perguntas_ataxia) :-
    perguntas('Ataxia', 0.03, 'Paula').

test(perguntas_paralisia_supranuclear_progressiva) :-
    perguntas('Paralisia supranuclear progressiva', 0.02, 'Paula').

test(perguntas_enxaqueca) :-
    perguntas('Enxaqueca', 0.5, 'Paula').

test(perguntas_atrofia_multiplos_sistemas) :-
    perguntas('Atrofia de Múltiplos Sistemas', 0.4, 'Paula').

:- end_tests(perguntas).

% Testes para o predicado exibirSintomas/1
:- begin_tests(exibirSintomas).

test(exibir_sintomas) :-
    assert(sintomaPaciente('Paula', 'lapsos de memória')),
    assert(sintomaPaciente('Paula', 'dificuldade em reconhecer pessoas')),
    exibirSintomas('Paula').

:- end_tests(exibirSintomas).

% Testes para o predicado respostaDiagnostico/2
:- begin_tests(respostaDiagnostico).

test(resposta_diagnostico_1) :-
    respostaDiagnostico(1, 'Paula').

test(resposta_diagnostico_2) :-
    respostaDiagnostico(2, 'Paula').

test(resposta_diagnostico_3) :-
    respostaDiagnostico(3, 'Paula').

:- end_tests(respostaDiagnostico).

% Testes para o predicado exibirDiagnostico/2
:- begin_tests(exibirDiagnostico).

test(exibir_diagnostico) :-
    assert(diagnostico('Paula', 'Alzheimer', 0.05)),
    exibirDiagnostico([['Alzheimer', 0.05]], 'Paula').

:- end_tests(exibirDiagnostico).

% Testes para o predicado gravarDadosArquivo/1
:- begin_tests(gravarDadosArquivo).

test(gravar_dados_arquivo) :-
    assert(paciente('Paula', 30, 'feminino')),
    assert(sintomaPaciente('Paula', 'lapsos de memória')),
    assert(diagnostico('Paula', 'Alzheimer', 0.05)),
    gravarDadosArquivo('Paula').

:- end_tests(gravarDadosArquivo).

% Testes para o predicado main/0
:- begin_tests(main).

test(main) :-
    main.

:- end_tests(main).

% Predicado para executar todos os testes
run_tests :-
    run_tests(sintoma),
    run_tests(fatorRisco),
    run_tests(probabilidade),
    run_tests(listaSintomas),
    run_tests(quantidadeSintomas),
    run_tests(perguntas),
    run_tests(exibirSintomas),
    run_tests(respostaDiagnostico),
    run_tests(exibirDiagnostico),
    run_tests(gravarDadosArquivo),
    run_tests(main).:- use_module(library(plunit)).
:- include('/home/paula/Documents/prolog/base.pl').

% Testes para o predicado sintoma/2
:- begin_tests(sintoma).

test(sintoma_alzheimer) :-
    sintoma('Alzheimer', 'lapsos de memória').

test(sintoma_ataxia) :-
    sintoma('Ataxia', 'quedas com frequência').

test(sintoma_paralisia_supranuclear) :-
    sintoma('Paralisia supranuclear progressiva', 'quedas repetidas').

test(sintoma_enxaqueca) :-
    sintoma('Enxaqueca', 'dor intensa na cabeça').

test(sintoma_atrofia_multiplos_sistemas) :-
    sintoma('Atrofia de Múltiplos Sistemas', 'problemas de coordenação').

:- end_tests(sintoma).

% Testes para o predicado fatorRisco/2
:- begin_tests(fatorRisco).

test(fator_risco_alzheimer) :-
    fatorRisco('Alzheimer', 0.5).

test(fator_risco_ataxia) :-
    fatorRisco('Ataxia', 0.3).

test(fator_risco_paralisia_supranuclear) :-
    fatorRisco('Paralisia supranuclear progressiva', 0.2).

test(fator_risco_enxaqueca) :-
    fatorRisco('Enxaqueca', 0.1).

test(fator_risco_atrofia_multiplos_sistemas) :-
    fatorRisco('Atrofia de Múltiplos Sistemas', 0.4).

:- end_tests(fatorRisco).

% Testes para o predicado probabilidade/2
:- begin_tests(probabilidade).

test(probabilidade_alzheimer) :-
    probabilidade('Alzheimer', 0.05).

test(probabilidade_ataxia) :-
    probabilidade('Ataxia', 0.03).

test(probabilidade_paralisia_supranuclear) :-
    probabilidade('Paralisia supranuclear progressiva', 0.02).

test(probabilidade_enxaqueca) :-
    probabilidade('Enxaqueca', 0.5).

test(probabilidade_atrofia_multiplos_sistemas) :-
    probabilidade('Atrofia de Múltiplos Sistemas', 0.4).

:- end_tests(probabilidade).

% Testes para o predicado listaSintomas/2
:- begin_tests(listaSintomas).

test(lista_sintomas_alzheimer) :-
    listaSintomas('Alzheimer', ['lapsos de memória', 'dificuldade em reconhecer pessoas', 'dificuldade em recordar palavras', 'repetição frequente de perguntas', 'dificuldade em tomar decisões', 'confusão e desorientação', 'delírios', 'paranoia e desconfiança', 'distúrbios do sono', 'problemas de memória a curto e longo prazo']).

test(lista_sintomas_ataxia) :-
    listaSintomas('Ataxia', ['quedas com frequência', 'incapacidade de andar em linha reta', 'desajeitamento', 'fala arrastada', 'dificuldades para engolir', 'tremores ou agitação', 'fadiga', 'problemas de visão']).

test(lista_sintomas_paralisia_supranuclear) :-
    listaSintomas('Paralisia supranuclear progressiva', ['quedas repetidas', 'rigidez muscular', 'problemas oculares', 'tremores', 'lentidão de pensamento', 'fala mais lenta e arrastada', 'problemas de mobilidade', 'dificuldade no controle muscular da boca língua e garganta', 'problemas intestinais e urinários']).

test(lista_sintomas_enxaqueca) :-
    listaSintomas('Enxaqueca', ['dor intensa na cabeça', 'dor pulsante', 'náuseas', 'vômitos', 'sensibilidade a luz e som']).

test(lista_sintomas_atrofia_multiplos_sistemas) :-
    listaSintomas('Atrofia de Múltiplos Sistemas', ['problemas de coordenação', 'tremor, lentidão e rigidez', 'risos ou choro incontroláveis', 'respiração ruidosa', 'suspiros involuntários', 'voz fraca', 'visão turva', 'mãos e pés frios']).

:- end_tests(listaSintomas).

% Testes para o predicado quantidadeSintomas/4
:- begin_tests(quantidadeSintomas).

test(quantidade_sintomas_alzheimer) :-
    quantidadeSintomas('Alzheimer', ['lapsos de memória', 'dificuldade em reconhecer pessoas'], 2, 'Paula').

test(quantidade_sintomas_ataxia) :-
    quantidadeSintomas('Ataxia', ['quedas com frequência', 'incapacidade de andar em linha reta'], 2, 'Paula').

test(quantidade_sintomas_paralisia_supranuclear) :-
    quantidadeSintomas('Paralisia supranuclear progressiva', ['quedas repetidas', 'rigidez muscular'], 2, 'Paula').

test(quantidade_sintomas_enxaqueca) :-
    quantidadeSintomas('Enxaqueca', ['dor intensa na cabeça', 'dor pulsante'], 2, 'Paula').

test(quantidade_sintomas_atrofia_multiplos_sistemas) :-
    quantidadeSintomas('Atrofia de Múltiplos Sistemas', ['problemas de coordenação', 'tremor, lentidão e rigidez'], 2, 'Paula').

:- end_tests(quantidadeSintomas).

% Testes para o predicado perguntas/3
:- begin_tests(perguntas).

test(perguntas_alzheimer) :-
    perguntas('Alzheimer', 0.1, 'Paula').

test(perguntas_ataxia) :-
    perguntas('Ataxia', 0.2, 'Paula').

test(perguntas_paralisia_supranuclear) :-
    perguntas('Paralisia supranuclear progressiva', 0.3, 'Paula').

test(perguntas_enxaqueca) :-
    perguntas('Enxaqueca', 0.4, 'Paula').

test(perguntas_atrofia_multiplos_sistemas) :-
    perguntas('Atrofia de Múltiplos Sistemas', 0.5, 'Paula').

:- end_tests(perguntas).

% Testes para o predicado exibirSintomas/1
:- begin_tests(exibirSintomas).

test(exibir_sintomas) :-
    assert(sintomaPaciente('Paula', 'lapsos de memória')),
    assert(sintomaPaciente('Paula', 'dificuldade em reconhecer pessoas')),
    exibirSintomas('Paula').

:- end_tests(exibirSintomas).

% Testes para o predicado respostaDiagnostico/2
:- begin_tests(respostaDiagnostico).

test(resposta_diagnostico_1) :-
    respostaDiagnostico(1, 'Paula').

test(resposta_diagnostico_2) :-
    respostaDiagnostico(2, 'Paula').

test(resposta_diagnostico_3) :-
    respostaDiagnostico(3, 'Paula').

:- end_tests(respostaDiagnostico).

% Testes para o predicado exibirDiagnostico/2
:- begin_tests(exibirDiagnostico).

test(exibir_diagnostico) :-
    assert(diagnostico('Paula', 'Alzheimer', 0.1)),
    assert(diagnostico('Paula', 'Ataxia', 0.2)),
    exibirDiagnostico([['Alzheimer', 0.1], ['Ataxia', 0.2]], 'Paula').

:- end_tests(exibirDiagnostico).

% Testes para o predicado gravarDadosArquivo/1
:- begin_tests(gravarDadosArquivo).

test(gravar_dados_arquivo) :-
    assert(paciente('Paula', 30, 'feminino')),
    assert(sintomaPaciente('Paula', 'lapsos de memória')),
    assert(sintomaPaciente('Paula', 'dificuldade em reconhecer pessoas')),
    assert(diagnostico('Paula', 'Alzheimer', 0.1)),
    gravarDadosArquivo('Paula').

:- end_tests(gravarDadosArquivo).

% Testes para o predicado main/0
:- begin_tests(main).

test(main) :-
    main.

:- end_tests(main).

% Testes para o predicado decrescente/3
:- begin_tests(decrescente).

test(decrescente_maior) :-
    decrescente([a, 5], [[b, 3], [c, 2]], [[a, 5], [b, 3], [c, 2]]).

test(decrescente_menor) :-
    decrescente([a, 1], [[b, 3], [c, 2]], [[b, 3], [a, 1], [c, 2]]).

:- end_tests(decrescente).

% Testes para o predicado ordenar/2
:- begin_tests(ordenar).

test(ordenar_vazio) :-
    ordenar([], []).

test(ordenar_um_elemento) :-
    ordenar([[a, 1]], [[a, 1]]).

test(ordenar_mais_elementos) :-
    ordenar([[a, 1], [b, 3], [c, 2]], [[b, 3], [c, 2], [a, 1]]).

:- end_tests(ordenar).

% Executa todos os testes
:- run_tests.