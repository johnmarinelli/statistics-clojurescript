(ns stats.confidence-intervals-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.confidence-intervals :as cis]))


(deftest confidence-interval-proportion []
  (let [phat 0.576
        n 1000
        p 0.5
        ci (cis/confidence-interval-proportion phat n p)]
    (is (= ci (list 0.5450096789303498 0.6069903210696501)))))
