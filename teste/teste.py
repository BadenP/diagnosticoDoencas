from pyswip import Prolog

def consultar_doenca_sintomas(doenca):
    prolog = Prolog()
    prolog.consult("base.pl")  # Substitua pelo nome do seu arquivo de base de conhecimento

    sintomas = []
    for resultado in prolog.query("sintoma('{}', S)".format(doenca)):
        sintomas.append(resultado["S"])
    
    if sintomas:
        return sintomas
    else:
        return ["Doença não encontrada na base de conhecimento."]

def main():
    while True:
        doenca = input("Digite o nome da doença (ou 'sair' para encerrar): ").lower()
        if doenca == "sair":
            break
        sintomas = consultar_doenca_sintomas(doenca)
        print("Sintomas da doença '{}': {}".format(doenca, sintomas))

if __name__ == "__main__":
    main()
