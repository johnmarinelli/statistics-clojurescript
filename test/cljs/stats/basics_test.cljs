(ns stats.basics-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.basics :as basics]))

(deftest test-standard-deviation-population []
  (let [data (range 1 11)
        sd (basics/standard-deviation data)]
    (is (= sd 2.8722813232690143))))

(deftest test-standard-deviation-sample []
  (let [data (range 1 11)
        sd (basics/standard-deviation data true)]
    (is (= sd 3.0276503540974917))))

(deftest test-median []
  (let [data (range 1 11)
        median (basics/median data)]
    (is (= median 6))))

(deftest test-first-quartile []
  (let [data (range 1 11)
        fq (basics/first-quartile data)]
    (is (= fq 3))))

(deftest test-third-quartile []
  (let [data (range 1 11)
        tq (basics/third-quartile data)]
    (is (= tq 8))))

(deftest variance []
  (let [data (range 1 11)
        v (basics/variance data)]
    (is (= v 8.25))))
