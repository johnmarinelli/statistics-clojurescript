(ns stats.core-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.core :as app]))

(defn- between [lhs rhs n]
  (and (<= lhs n) (>= rhs n)))

(deftest test-random-in-range []
  (let [min 1
        max 10
        r (app/random min max)]
    (is (between min max r))))

(deftest test-random-in-negative-and-positive-range []
  (let [min -5
        max 5
        r (app/random min max)]
    (is (between min max r))))

(deftest test-random-in-negative-range []
  (let [min -10
        max -1
        r (app/random min max)]
    (is (between min max r))))

(deftest test-mean []
  (let [data (range 1 11)
        mean (app/mean data)]
    (is (= mean 5.5))))

(deftest test-standard-deviation-population []
  (let [data (range 1 11)
        sd (app/standard-deviation data)]
    (is (= sd 2.8722813232690143))))

(deftest test-standard-deviation-sample []
  (let [data (range 1 11)
        sd (app/standard-deviation data true)]
    (is (= sd 3.0276503540974917))))

(deftest test-median []
  (let [data (range 1 11)
        median (app/median data)]
    (is (= median 6))))

(deftest test-first-quartile []
  (let [data (range 1 11)
        fq (app/first-quartile data)]
    (is (= fq 3))))

(deftest test-third-quartile []
  (let [data (range 1 11)
        tq (app/third-quartile data)]
    (is (= tq 8))))
