import sys
from PyQt5.QtWidgets import QApplication, QWidget, QVBoxLayout, QComboBox, QPushButton, QLabel
from pyswip import Prolog

class MainWindow(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Consultor de Doenças")
        self.setGeometry(100, 100, 400, 200)

        self.prolog = Prolog()
        self.prolog.consult("base.pl")  # Substitua pelo nome do seu arquivo de base de conhecimento

        self.sintomas_selecionados = []

        layout = QVBoxLayout()

        self.label = QLabel("Selecione os sintomas:")
        layout.addWidget(self.label)

        self.combo_sintomas = QComboBox()
        self.combo_sintomas.addItems(["febre", "tosse", "Dor de Garganta", "Nariz Entupido", "Mal-Estar", "Dor de Cabeça", "Dor no Corpo", "Manchas na Pele"])
        layout.addWidget(self.combo_sintomas)

        self.button_adicionar = QPushButton("Adicionar Sintoma")
        self.button_adicionar.clicked.connect(self.adicionar_sintoma)
        layout.addWidget(self.button_adicionar)

        self.button_consultar = QPushButton("Consultar Doença")
        self.button_consultar.clicked.connect(self.consultar_doenca)
        layout.addWidget(self.button_consultar)

        self.result_label = QLabel()
        layout.addWidget(self.result_label)

        self.setLayout(layout)

    def adicionar_sintoma(self):
        sintoma = self.combo_sintomas.currentText()
        if sintoma not in self.sintomas_selecionados:
            self.sintomas_selecionados.append(sintoma)
            self.label.setText("Sintomas selecionados: " + ", ".join(self.sintomas_selecionados))

    def consultar_doenca(self):
        if self.sintomas_selecionados:
            sintomas = [sintoma.replace(" ", "_") for sintoma in self.sintomas_selecionados]
            sintomas_query = ",".join(["sintoma(D, {})".format(s) for s in sintomas])
            doenca_query = "doenca(D, {})".format(",".join(["_" for _ in range(len(self.sintomas_selecionados))]))
            query = "{},{}".format(sintomas_query, doenca_query)
            doencas = list(self.prolog.query(query))
            if doencas:
                doencas_encontradas = ", ".join(set(doenca["D"] for doenca in doencas))
                self.result_label.setText("Possíveis doenças: " + doencas_encontradas)
            else:
                self.result_label.setText("Nenhuma doença encontrada para os sintomas selecionados.")
        else:
            self.result_label.setText("Selecione pelo menos um sintoma.")



if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec_())
