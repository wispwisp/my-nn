(ns mylib.model
  (:refer-clojure :exclude [* / + - exp])
  (:require [clojure.core.matrix :refer [array mmul emap shape exp transpose]]
            [clojure.core.matrix.operators :refer [+ - * /]]
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

(defn sum-over-axis [X]
  (array (map (fn [x] [(reduce + x)]) X)))

(defn activation-relu "$r(X, w) = max(0, Xw)$" [X]
  (emap (partial max 0) X))

(defn activation-prime-relu [X]
  (emap (fn [x] (if (< x 0.0) 0.0 1.0)) X))

(defn activation-sigmoid "s(X, W) = 1/(1+e^{-Xw})" [X]
  (emap (fn [x] (/ 1.0 (+ 1.0 (exp (- x))))) X))

(defn activation-prime-sigmoid [X]
  (let [f' (fn [x] (* (/ 1 (+ 1 (exp (- x)))) (- 1 (/ 1 (+ 1 (exp (- x)))))))]
    (emap f' X)))

(defn make-a-layer [activation activation-prime weights-shape]
  (let [N (nth weights-shape 0)
        M (nth weights-shape 1)] {:activation activation
                                  :activation-prime activation-prime
                                  :weights (make-a-random-matrix N M)
                                  :bias (make-a-random-matrix N 1)}))

(defn apply-layer [prev-layers current-layer]
  (let [X (:model (last prev-layers))
        W (:weights current-layer)
        b (:bias current-layer)
        Z (add-bias-vector (mmul W X) b)]
    (conj prev-layers
          (merge current-layer {:Z Z
                                :input X
                                :model ((:activation current-layer) Z)}))))

(defn first-backpropagation-step
  "dZ3 = A3 - Y,
   dW3 = 1/N * dZ3 Dot A2.T,
   db3 = 1/N * SumOverAxis(dZ3)"
  [model Y]
  (let [A-last (:model model)
        A-prev (:input model)
        amount-of-samples ((shape A-last) 1)
        normalizator (/ 1.0 amount-of-samples)
        dZ (- A-last Y)
        dW (* normalizator (mmul dZ (transpose A-prev)))
        db (* normalizator (sum-over-axis dZ))]
    (merge model {:dZ dZ :dW dW :db db})))

(defn backpropagation
  "dZ2 = W3.T dot dZ3 * RELuPrime(Z2),
   dW2 = 1/N * dZ2 dot A1.T,
   db2 = 1/N * SumOverAxis(dZ2)
   ***
   dZ1 = W2.T dot dZ2 * RELuPrime(Z1),
   dW1 = 1/N * dZ1 dot X.T,
   db1 = 1/N * SumOverAxis(dZ1)"
  [prevs curr]
  (let [prev (last prevs)
        amount-of-samples ((shape (:model prev)) 1)
        normalizator (/ 1.0 amount-of-samples)
        dZ (* (mmul (transpose (:weights prev)) (:dZ prev)) ((:activation-prime curr) (:Z curr)))
        dW (* (mmul dZ (transpose (:input curr))) normalizator)
        db (* normalizator (sum-over-axis dZ))]
    (conj prevs (merge curr {:dZ dZ :dW dW :db db}))))

(comment
  (def X (array [[1.0 2.0 10.0]
                 [1.0 2.0 10.0]
                 [1.0 2.0 10.0]
                 [1.0 2.0 10.0]]))
  (def Y (array [[1.0 0.0 1.0]]))

  (def layers [{:activation activation-relu
                :activation-prime activation-prime-relu
                :weights (array [[0.37 0.15 0.35 0.16]
                                 [0.45 0.3 0.3 0.28]])
                :bias (array [[0.1]
                              [0.1]])}
               {:activation activation-relu
                :activation-prime activation-prime-relu
                :weights (array [[0.87 0.99]
                                 [0.15 0.19]
                                 [0.35 0.15]
                                 [0.75 0.66]
                                 [0.27 0.77]])
                :bias (array [[0.2]
                              [0.2]
                              [0.2]
                              [0.2]
                              [0.2]])}
               {:activation activation-sigmoid
                :activation-prime activation-prime-sigmoid
                :weights (array [[0.11 0.13 0.76 0.55 0.17]])
                :bias (array [[0.3]])}])

  (def models (reduce apply-layer [{:model X}] layers))

  (reduce backpropagation
          [(first-backpropagation-step (first (reverse models)) Y)]
          (rest (reverse (rest models)))))

(defn train-a-model [train labels]
  (let [a-shape (shape train)
        feature-space (a-shape 0)
        amount-of-samples (a-shape 1)
        layers [(make-a-layer activation-relu activation-prime-relu [12 feature-space])
                (make-a-layer activation-relu activation-prime-relu [10 12])
                (make-a-layer activation-sigmoid activation-prime-sigmoid [1 10])]
        models (reduce apply-layer [{:model train}] layers)]
    (println "feature space:" feature-space "amount of samples:" amount-of-samples)
    (println (shape labels) (shape train))
    (println labels train)
    models))

