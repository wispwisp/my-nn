(ns mylib.model
  (:refer-clojure :exclude [* / + -])
  (:require [clojure.core.matrix :refer [array mmul emap shape transpose columns ereduce esum
                                         set-current-implementation log exp]]
            [clojure.core.matrix.operators :refer [+ - * /]]
            [clojure.core.matrix.random :refer [randoms]]))

(set-current-implementation :vectorz)

(def learning-rate 0.1)
(def regularization_coef 0.1)

(defn make-a-random-matrix
  ([n m]
   (array (for [_ (range n)] (array (take m (randoms))))))
  ([n m d]
   (array (for [_ (range n)] (emap / (array (take m (randoms))) d)))))

(defn add-bias-vector "Add a line to line. Need for ingnoring multiple samples" [X b]
  (let [x-shape (shape X)
        b-shape (shape b)
        x-space (x-shape 0)
        b-space (b-shape 0)]
    (when (or (not= x-space b-space) (not= (b-shape 1) 1))
      (throw (new Exception "Inconsistent bias vector")))
    (array (map (fn [X-i b-i] (array (map (partial + (first b-i)) X-i))) X b))))

(defn sum-over-axis [X]
  (array (map (fn [x] [(reduce + x)]) X)))

(defn activation-relu "$r(X, w) = max(0, Xw)$" [X]
  (emap (partial max 0) X))

(defn activation-prime-relu [X]
  (emap (fn [x] (if (< x 0.0) 0.0 1.0)) X))

(defn activation-sigmoid "s(X, W) = 1/(1+e^{-Xw})" [X]
  (emap (fn [x] (/ 1.0 (+ 1.0 (exp (- x))))) X))

(defn activation-softmax [X]
  (let [a-columns (columns X)
        sums (->> a-columns
                  (map exp)
                  (map esum))]
    (transpose (array (map (fn [col sum] (map #(/ (exp %) sum) col)) a-columns sums)))))

(defn make-a-layer [activation activation-prime weights-shape]
  (let [N (nth weights-shape 0)
        M (nth weights-shape 1)] {:activation activation
                                  :activation-prime activation-prime
                                  :weights (make-a-random-matrix N M 10)
                                  :bias (make-a-random-matrix N 1 10)}))

(defn apply-layer [prev-layers current-layer]
  (let [X (:model (last prev-layers))
        W (:weights current-layer)
        b (:bias current-layer)
        Z (add-bias-vector (mmul W X) b)]
    (conj prev-layers
          (merge current-layer {:Z Z
                                :input X
                                :model ((:activation current-layer) Z)}))))

(defn forward-propagation
  "Z[L] = W[L] dot A[L-1] + b[L]
   A[L] = a(Z[L])"
  [train layers]
  (reduce apply-layer [{:model train}] layers))

(defn first-backpropagation-step
  "ex.: L=3
   dZ3 = A3 - Y,
   dW3 = 1/N * dZ3 Dot A2.T,
   db3 = 1/N * SumOverAxis(dZ3)"
  [model Y]
  (let [A-last (:model model)
        A-prev (:input model)
        amount-of-samples ((shape A-last) 1)
        normalizator (/ 1.0 amount-of-samples)
        regularization (* (/ regularization_coef amount-of-samples) (:weights model))
        dZ (- A-last Y)
        dW (+ (* normalizator (mmul dZ (transpose A-prev))) regularization)
        db (* normalizator (sum-over-axis dZ))]
    (merge model {:dZ dZ :dW dW :db db})))

(defn backpropagation
  "ex.: L=3
   dZ2 = W3.T dot dZ3 * RELuPrime(Z2),
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
        regularization (* (/ regularization_coef amount-of-samples) (:weights curr))
        dZ (* (mmul (transpose (:weights prev)) (:dZ prev)) ((:activation-prime curr) (:Z curr)))
        dW (+ (* normalizator (mmul dZ (transpose (:input curr)))) regularization)
        db (* normalizator (sum-over-axis dZ))]
    (conj prevs (merge curr {:dZ dZ :dW dW :db db}))))

(defn update-weights [model backpropagations-result]
  (let [updates (map (fn [layer backpropagation-result]
                       [(- (:weights layer) (* learning-rate (:dW backpropagation-result)))
                        (- (:bias layer) (* learning-rate (:db backpropagation-result)))])
                     model (reverse backpropagations-result))]
    (map (fn [layer [new-weight new-bias]] (assoc layer :weights new-weight :bias new-bias))
         model updates)))

(defn extract-lables [X]
  (map #(first (apply max-key second (map-indexed vector %))) (columns X)))

(defn count-correct [predictions classes]
  (when (not= (count predictions) (count classes))
    (throw (new Exception "Inconsistent vectors size")))
  (reduce + (map (fn [p c] (if (= p c) 1 0)) predictions classes)))

(defn compute-cost [X Y N]
  (let [cost-matrix (- (+ (* Y (log X)) (* (- 1.0 Y) (log (- 1.0 X)))))
        euclidean-norm (Math/sqrt (ereduce (fn [c n] (+ c (* n n))) 0 cost-matrix))]
    (/ euclidean-norm (double N))))

(defn train-step [iteration model labels classes amount-of-samples train]
  (let [backpropagation-result (reduce backpropagation
                                       [(first-backpropagation-step (first (reverse model)) labels)]
                                       (rest (reverse (rest model))))
        current-model (update-weights (rest model) backpropagation-result)
        evaluated-model (forward-propagation train current-model)
        correct-amount (count-correct classes (extract-lables (:model (last evaluated-model))))]
    (println "Epoch[" iteration "]:"
             "accuracity:" (/ (double correct-amount) (double (count classes)))
             "cost:" (compute-cost (:model (last evaluated-model)) labels amount-of-samples))
    evaluated-model))

(defn train-a-model [train labels epochs]
  (let [a-shape (shape train)
        feature-space (a-shape 0)
        amount-of-samples (a-shape 1)
        output-size ((shape labels) 0)
        classes (extract-lables labels)
        model (atom (forward-propagation
                     train
                     [(make-a-layer activation-relu activation-prime-relu [20 feature-space])
                      (make-a-layer activation-relu activation-prime-relu [5 20])
                      (make-a-layer activation-softmax nil [output-size 5])]))]
    (println "feature space:" feature-space "amount of samples:" amount-of-samples "output size:" output-size)
    (println "transposed lables shape:" (shape labels) "transposed train shape:" (shape train))
    (println "Lables[" (count classes) "]: " (take 15 classes) "...")
    (println "Start learning...")
    (doall
     (for [i (range epochs)]
       (swap! model (fn [current-model]
                      (train-step i current-model labels classes amount-of-samples train)))))
    (println "Done learning.")
    (rest @model)))

(defn evaluate-a-model [model test labels]
  (let [classes (extract-lables labels)
        evaluated (forward-propagation test model)
        amount (count classes)
        correct-amount (count-correct classes (extract-lables (:model (last evaluated))))]
    (/ (double correct-amount) (double amount))))
