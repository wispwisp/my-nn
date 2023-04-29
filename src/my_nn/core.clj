(ns my-nn.core
  (:gen-class)
  (:require [clojure.core.matrix :as m]
            [mylib.csvutils :as mycsv]
            [mylib.model :as ml]))

(m/set-current-implementation :vectorz)

(defn -main
  "Entrypoint to a programm"
  [& _args]
  (let [labels (mycsv/load-matrix-from-csv "mnist_train_labels.csv")
        train (mycsv/load-matrix-from-csv "mnist_train.csv")
        epochs 1000
        network (ml/train-a-model (m/transpose train) (m/transpose labels) epochs)
        test_labels (mycsv/load-matrix-from-csv "mnist_test_labels.csv")
        test (mycsv/load-matrix-from-csv "mnist_test.csv")
        accuracity (ml/evaluate-a-model network (m/transpose test) (m/transpose test_labels))]
    (println "Test Accuracity:" accuracity)))

;; apply, partial, comp, into, conj, first/last, doall/dorun
;; assoc
