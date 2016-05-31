(ns stats.hypothesis-tests-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.hypothesis-tests :as hts]))
(deftest hypothesis-test-proportion-double-tailed
  (let [phat 0.52
        p 0.5
        n 1096
        hyp (hts/hypothesis-test-proportion-double-tailed p phat n)]
    (is (= hyp false))))
