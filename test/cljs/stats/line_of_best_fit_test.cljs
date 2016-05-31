(ns stats.line-of-best-fit-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.line-of-best-fit :as lobf]))

(defn- round-to [n precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* n factor)) factor)))

(deftest test-line-of-best-fit
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        lrl (lobf/LinearRegressionLine. xs ys)
        lf (lobf/line lrl)]
    (is (= (round-to (lf 1) 2) 65.53))))

(deftest correlation
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        lrl (lobf/LinearRegressionLine. xs ys)
        c (lobf/correlation lrl)]
    (is (= (round-to c 2) 0.53))))

(deftest test-residuals
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        lrl (lobf/LinearRegressionLine. xs ys)
        lf (lobf/residuals lrl)]
    (is (= 0 (round-to (reduce + lf) 1)))))

(deftest residual-variance
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        lrl (lobf/LinearRegressionLine. xs ys)
        rv (lobf/residuals-variance lrl)]
    (is (= 78.64 (round-to rv 2)))))
