(ns mylib.csvutils
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]))

(defn split-a-csv-line [line separator]
  (loop [[character & rest] line
         current ""
         result []
         inside false]
    (if (empty? rest)
      (conj result (str current character))
      (if (= character \")
        (if inside
          (recur rest current result false) ;; close "
          (recur rest current result true)) ;; open "
        (if inside
          (recur rest (str current character) result inside) ;; inside quotes ignore comma
          (if (or (= character separator) (= character \newline))
            (recur rest "" (conj result current) inside)
            (recur rest (str current character) result inside)))))))

(defn convert-into-double [x] (Double. x))

(defn convert_line_into_double [line]
  (m/array (map convert-into-double line)))

(defn load-matrix-from-csv
  [filepath]
  (let [content (slurp filepath)
        lines (str/split content #"\n")
        processed (map #(str/split % #",") lines)
        converted (map convert_line_into_double processed)]
    converted))
