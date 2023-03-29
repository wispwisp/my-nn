(ns mylib.csvutils)

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
