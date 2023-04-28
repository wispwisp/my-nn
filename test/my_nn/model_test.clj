(ns my-nn.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.matrix :refer [array]]
            [mylib.model :refer [add-bias-vector]]))

(deftest test-add-bias-vector
  (testing "FIXME, I fail."
    (let [result (add-bias-vector (array [[1.0 2.0 10.0]
                                          [3.0 6.0 60.0]
                                          [5.0 9.0 90.0]])
                                  (array [[100.0]
                                          [200.0]
                                          [100.0]]))
          expected (array [[101.0 102.0 110.0]
                           [203.0 206.0 260.0]
                           [105.0 109.0 190.0]])]
      (is (= result expected)))))
