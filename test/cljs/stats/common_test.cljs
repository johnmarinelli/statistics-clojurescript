(ns stats.common-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.common :as common]))

(defn- between [lhs rhs n]
  (and (<= lhs n) (>= rhs n)))

(defn- round-to [n precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* n factor)) factor)))

(deftest test-sum
  (let [coll (range 1 10)
        s (common/sum coll)]
    (is (= s 45))))

(deftest average
  (let [coll (range 1 10)
        avg (common/average coll)]
    (is (= avg 5))))

(deftest test-random-in-range []
  (let [min 1
        max 10
        r (common/random min max)]
    (is (between min max r))))

(deftest test-random-in-negative-and-positive-range []
  (let [min -5
        max 5
        r (common/random min max)]
    (is (between min max r))))

(deftest test-random-in-negative-range []
  (let [min -10
        max -1
        r (common/random min max)]
    (is (between min max r))))

(deftest get-area []
  (let [a 1
        b 3
        f #(identity %)
        a (common/get-area f a b)]
    (is (= a 4))))

(deftest get-rectangles-under-curve []
  (let [f #(identity %)
        a 0
        b 5
        n 5
        rects (common/get-rectangles-under-curve f a b n)
        expected-rects (loop [rs '() itr a]
                         (if (= itr b)
                           (reverse rs)
                           (recur (conj rs (common/Rectangle. itr 0 (/ b n) (+ 0.5 itr))) (inc itr))))]
    (is (= rects expected-rects))))

(deftest integrate []
  (let [a 1.5
        b 5.75
        n 100
        f #(identity %)
        definite-integral (common/integrate f a b n)
        rounded (round-to definite-integral 2)]
    (is (= rounded 15.41))))

(deftest is-integer?-true
  (let [n 1
        ii (common/is-integer? n)]
    (is (= ii true))))

(deftest is-integer?-false
  (let [n 1.5
        ii (common/is-integer? n)]
    (is (= ii false))))


(comment(deftest factorial
   (let [n 10
         fact (common/factorial n)]
     (is (= fact 1)))))

(deftest is-integer?
  (let [n "str"
        ii (common/is-integer? n)]
    (is (= ii false))))

(deftest gamma-function
  (let [n 0.1
        g (common/gamma-function n)]
    (is (= (round-to g 2) 9.51))))
