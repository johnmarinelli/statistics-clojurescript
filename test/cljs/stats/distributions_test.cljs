(ns stats.distributions-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.distributions :as dist]))

(comment "todo: move this to a common test utils file")
(defn- round-to [n precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* n factor)) factor)))

(deftest test-normal-distribution-probability-density []
  (let [mean 11
        sd 1
        nd (dist/NormalDistributionFunction. mean sd)
        x 12
        d (dist/probability-density nd x)]
    (is (= d 0.24197072451914337))))

(deftest test-normal-distribution-cumulative-density []
  (let [mean 0.0
        sd 1.0
        x -1.75
        nd (dist/NormalDistributionFunction. mean sd)
        prob (dist/cumulative-density nd x)]
    (is (= (round-to prob 4) 0.0401))))

(deftest test-t-distribution-probability-density []
  (let [dof 3
        n 0.25
        std (dist/StudentsTDistributionFunction. dof)
        t (dist/probability-density std n)]
    (is (= (round-to t 2) 0.35 ))))

(deftest test-t-distribution-cumulative-density []
  (let [dof 3
        n 0.1
        std (dist/StudentsTDistributionFunction. dof)
        c (dist/cumulative-density std n)]
    (is (= (round-to c 2) 0.53 ))))
