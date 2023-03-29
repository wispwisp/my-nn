(ns mylib.model
  (:refer-clojure :exclude [* / + - == < <= > >= = min exp])
  (:require [clojure.core.matrix :refer [array mmul emap shape exp]]
            [clojure.core.matrix.operators :refer [+ - /]]
            [clojure.core.matrix.random :refer [randoms]]))

(defn make-a-random-matrix [n m]
  (array (for [_ (range 0 n)] (into [] (take m (randoms))))))

(defn add-bias-vector "Add a line to line. Need for ingnoring multiple samples" [X b]
  (let [x-shape (shape X)
        b-shape (shape b)
        x-space (x-shape 0)
        b-space (b-shape 0)]
    (when (or (not= x-space b-space) (not= (b-shape 1) 1))
      (throw (new Exception "Inconsistent bias vector")))
    (array (map (fn [X-i b-i] (array (map (partial + (b-i 0)) X-i))) X b))))

;; activations:

(defn activation-relu "$r(X, w) = max(0, Xw)$" [X]
  (emap (partial max 0) X))

(defn activation-sigmoid "s(X, W) = 1/(1+e^{-Xw})" [X]
  (emap (fn [x] (/ 1.0 (+ 1.0 (exp (- x))))) X))

;; activations end.

(defn apply-layer [X layer]
  (let [activation (layer 0)
        W (layer 1)
        b (layer 2)]
    (activation (add-bias-vector (mmul W X) b))))

(comment
  (def X (array [[1.0 2.0 10.0]
                 [1.0 2.0 10.0]
                 [1.0 2.0 10.0]
                 [1.0 2.0 10.0]]))
  (def Y (array [[1.0 0.0 1.0]]))

  ;; TODO: maps ? :activation :weights :bias
  (def layers [[activation-relu (array [[0.37 0.15 0.35 0.16]
                                        [0.45 0.3 0.3 0.28]]) (array [[0.1]
                                                                      [0.1]])]
               [activation-relu (array [[0.87 0.99]
                                        [0.15 0.19]
                                        [0.35 0.15]
                                        [0.75 0.66]
                                        [0.27 0.77]]) (array [[0.2]
                                                              [0.2]
                                                              [0.2]
                                                              [0.2]
                                                              [0.2]])]
               [activation-sigmoid (array [[0.11 0.13 0.76 0.55 0.17]]) (array [[0.3]])]])

  (reduce apply-layer X layers)

  nil)

(defn train-a-model [train labels]
  (let [a-shape (shape train)
        feature-space (a-shape 0)
        amount-of-samples (a-shape 1)
        layers [[activation-relu (make-a-random-matrix 12 feature-space) (make-a-random-matrix 12 1)]
                [activation-relu (make-a-random-matrix 10 12) (make-a-random-matrix 10 1)]
                [activation-relu (make-a-random-matrix 2 10) (make-a-random-matrix 2 1)]]]
    (println "feature space:" feature-space "amount of samples:" amount-of-samples)
    (println (shape labels) (shape train))
    (println labels train)
    (reduce apply-layer train layers)))

