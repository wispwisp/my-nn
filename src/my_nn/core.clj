(ns my-nn.core
  (:gen-class)
  (:require [clojure.core.matrix :as m]))

(require '[clojure.string :as str])
(require '[mylib.csvutils :as csv])
(require '[mylib.model :as ml])

(def filepath "~/shared/")
(def filename "titanic_train_5.csv")
(def get-full-path (fn [] (str filepath filename)))

(defn convert-class [lhs rhs] (if (= lhs rhs) 1.0 0.0))

;; TODO(wisp): df = df.fillna(0)
(defn to-vector [[_PassengerId Survived Pclass _Name Sex
                  Age SibSp Parch _Ticket Fare _Cabin Embarked]]
  [(Integer. Survived) (Double. Age) (Double. SibSp) (Double. Parch) (Double. Fare)
   (convert-class Sex "male")
   (convert-class Embarked "C") (convert-class Embarked "Q")
   (convert-class Pclass "1") (convert-class Pclass "2")])

(defn process-a-csv-line [line]
  (->> line
       (map str/trim-newline)
       (map str/trim)
       (into [])))

(defn process-a-csv-lines [lines]
  (->> lines
       (map #(csv/split-a-csv-line % \,))
       (map process-a-csv-line)))

(defn -main
  "Entrypoint to a programm"
  [& _args]
  (let [fullpath (get-full-path)
        content (slurp filename)
        lines (str/split content #"\n")
        header (take 1 lines)
        without-header (drop 1 lines)
        processed (process-a-csv-lines without-header)
        vectorized (map to-vector processed)
        labels (m/array (reduce (fn [i v] (conj i (v 0))) [] vectorized))
        ;; TODO: clases from lab
        train (m/array (reduce (fn [i v] (conj i (rest v))) [] vectorized))]
    (println fullpath header)
    (ml/train-a-model (m/transpose train) (m/transpose labels))))
