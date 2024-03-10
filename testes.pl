:- use_module(library(plunit)).
:- consult('base_teste.pl').
:- consult('diagnostico.pl').

:- begin_tests(fatorRiscoGenero).

    test(t1, X == 1) :- fatorRiscoGenero('masculino', X).
    test(t2, X == 1.5) :- fatorRiscoGenero('feminino', X).

:- end_tests(fatorRiscoGenero).


:- begin_tests(fatorRiscoIdade).

    test(t1) :- fatorRiscoIdade(60, 0.12, 0.12).
    test(t2) :- fatorRiscoIdade(70, 0.03, 0.08).

:- end_tests(fatorRiscoIdade).


:- begin_tests(contaSintoma).

    test(t1, X==1) :-
        assert(sintomaPaciente('maria', 'Perda de memória')),
        contaSintoma(1, X, 'maria', 'Perda de memória'),
        retract(sintomaPaciente('maria', 'Perda de memória')).

:- end_tests(contaSintoma).


:- begin_tests(fatorRisco).
    
    test(t1, F==0.03625) :- fatorRisco('Alzheimer', F, "joao").
    test(t2, F==0.15) :- fatorRisco('Enxaqueca', F, "paulo").
    test(t3, F==0.375) :- fatorRisco('Enxaqueca', F, "maria").
    test(t4, F==0.12) :- fatorRisco('Fibromialgia', F, "ana").

:- end_tests(fatorRisco).


:- begin_tests(decrescente).
    test(t1) :- decrescente(1, [], [1]).
    test(t2) :- decrescente([a, 2], [[b, 1]], [[a, 2], [b, 1]]).
    test(t3) :- decrescente([c, 3], [[a, 4], [b, 2]], [[a, 4], [c, 3], [b, 2]]).
:- end_tests(decrescente).


:- begin_tests(ordenar).
    test(t1) :- ordenar([], []).
    test(t2) :- ordenar([[a, 1]], [[a, 1]]).
    test(t2) :- ordenar([[a, 2], [b, 1], [c, 3]], [[c, 3], [a, 2], [b, 1]]).
:- end_tests(ordenar).


:- begin_tests(filtrar).
    test(t1) :- filtrar([], []).
    test(t2) :- filtrar([[a, 0.51], [b, 0.8], [c, 0.02]], [[a, 0.51], [b, 0.8], [c, 0.02]]).
    test(t3) :- filtrar([[a, 0.58], [b, -0.1], [c, 0.21]], [[a, 0.58], [c, 0.21]]).
    test(t4) :- filtrar([[a, -0.001], [b, -0.22], [c, -0.13]], []).
:- end_tests(filtrar).
