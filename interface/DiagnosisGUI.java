import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.jpl7.*;

import java.util.ArrayList;
import java.util.Map;

public class DiagnosisGUI extends Application {
    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Diagnosis");

        CheckBox coughCheckbox = new CheckBox("Cough");
        CheckBox feverCheckbox = new CheckBox("Fever");
        CheckBox headacheCheckbox = new CheckBox("Headache");
        CheckBox rashCheckbox = new CheckBox("Rash");

        Button button = new Button("Get Diagnosis");
        button.setOnAction(e -> {
            ArrayList<String> symptoms = new ArrayList<>();
            if (coughCheckbox.isSelected()) symptoms.add("cough");
            if (feverCheckbox.isSelected()) symptoms.add("fever");
            if (headacheCheckbox.isSelected()) symptoms.add("headache");
            if (rashCheckbox.isSelected()) symptoms.add("rash");

            // Consult Prolog file
            Query q1 = new Query("consult", new Term[] {new Atom("disease.pl")});
            System.out.println("consult " + (q1.hasSolution() ? "succeeded" : "failed"));

            // Query for each symptom
            for (String symptom : symptoms) {
                Variable X = new Variable("X");
                Query q2 = new Query("disease", new Term[] {new Compound("symptom", new Term[] {new Atom(symptom)}), X});
                Map<String, Term> solution;
                while (q2.hasMoreSolutions()) {
                    solution = q2.nextSolution();
                    System.out.println("You may have " + solution.get("X"));
                }
            }
        });

        VBox vbox = new VBox(coughCheckbox, feverCheckbox, headacheCheckbox, rashCheckbox, button);

        Scene scene = new Scene(vbox, 200, 200);
        primaryStage.setScene(scene);
        primaryStage.show();
    }
}