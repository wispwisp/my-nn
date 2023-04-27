(ns mylib.model
  (:refer-clojure :exclude [* / + -])
  (:require [clojure.core.matrix :refer [array mmul emap shape transpose columns ereduce esum log exp]]
            [clojure.core.matrix.operators :refer [+ - * /]]
            [clojure.core.matrix.random :refer [randoms]]))

(defn make-a-random-matrix [n m]
  (array (for [_ (range n)] (array (take m (randoms))))))

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

(defn activation-softmax [X]
  (let [a-columns (columns X)
        sums (map esum a-columns)]
    (transpose (array (map (fn [col sum] (map #(/ % sum) col)) a-columns sums)))))

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

(defn forward-propagation
  "Z[L] = W[L] dot A[L-1] + b[L]
   A[L] = a(Z[L])"
  [train layers] (reduce apply-layer [{:model train}] layers))

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
        dZ (- A-last Y)
        dW (* normalizator (mmul dZ (transpose A-prev)))
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
        dZ (* (mmul (transpose (:weights prev)) (:dZ prev)) ((:activation-prime curr) (:Z curr)))
        dW (* normalizator (mmul dZ (transpose (:input curr))))
        db (* normalizator (sum-over-axis dZ))]
    (conj prevs (merge curr {:dZ dZ :dW dW :db db}))))

(defn update-layers [layers backpropagations-result]
  (let [alpha_coef 0.07
        updates (map (fn [layer backpropagation-result]
                       [(- (:weights layer) (* alpha_coef (:dW backpropagation-result)))
                        (- (:bias layer) (* alpha_coef (:db backpropagation-result)))])
                     layers (reverse backpropagations-result))]
    (map (fn [layer [new-weight new-bias]] (assoc layer :weights new-weight :bias new-bias))
         layers updates)))

(defn extract-lables [X] (map #(.indexOf % (apply max %)) (columns X)))

(defn count-correct [predictions classes]
  (when (not= (count predictions) (count classes))
    (throw (new Exception "Inconsistent vectors size")))
  (reduce + (map (fn [p c] (if (= p c) 1 0)) predictions classes)))

(defn compute-cost [X Y N]
  (let [cost-matrix (- (+ (* Y (log X)) (* (- 1.0 Y) (log (- 1.0 X)))))
        euclidean-norm (Math/sqrt (ereduce (fn [c n] (+ c (* n n))) 0 cost-matrix))]
    (/ euclidean-norm (double N))))

(defn train-step [iteration layers models labels classes amount-of-samples train]
  (swap! layers
         (fn [old-layers]
           (update-layers old-layers
                          (reduce backpropagation
                                  [(first-backpropagation-step (first (reverse @models)) labels)]
                                  (rest (reverse (rest @models)))))))
  (let [evaluated-model (forward-propagation train @layers)
        correct-amount (count-correct classes (extract-lables (:model (last evaluated-model))))]
    (swap! models (fn [_] evaluated-model))
    (println "Epoch[" iteration "]:"
             "accuracity:" (/ (double correct-amount) (double (count classes)))
             "cost:" (compute-cost (:model (first (reverse @models))) labels amount-of-samples))))

(defn train-a-model [train labels epochs]
  (let [a-shape (shape train)
        feature-space (a-shape 0)
        amount-of-samples (a-shape 1)
        output-size ((shape labels) 0)
        classes (extract-lables labels)
        layers (atom [(make-a-layer activation-relu activation-prime-relu [20 feature-space])
                      (make-a-layer activation-relu activation-prime-relu [5 20])
                      (make-a-layer activation-softmax nil [output-size 5])])
        models (atom (forward-propagation train @layers))]
    (println "feature space:" feature-space "amount of samples:" amount-of-samples "output size:" output-size)
    (println "transposed lables shape:" (shape labels) "transposed train shape:" (shape train))
    (println "Lables[" (count classes) "]: " (take 15 classes) "...")
    (println "Start learning...")
    (doall
     (for [i (range epochs)]
       (train-step i layers models labels classes amount-of-samples train)))
    (println "Done learning.")
    @layers))

(defn evaluate-a-model [network test labels]
  (let [classes (extract-lables labels)
        evaluated (forward-propagation test network)
        amount (count classes)
        correct-amount (count-correct classes (extract-lables (:model (last evaluated))))]
    (/ (double correct-amount) (double amount))))
