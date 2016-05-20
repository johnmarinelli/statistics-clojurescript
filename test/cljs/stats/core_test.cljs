(ns stats.core-test
  (:require-macros [cljs.test :refer [deftest testing is async]])
  (:require [cljs.test]
            [stats.core :as app]))

(defn- between [lhs rhs n]
  (and (<= lhs n) (>= rhs n)))

(defn- round-to [n precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* n factor)) factor)))

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

(deftest test-normal-distribution-density []
  (let [mean 11
        sd 1
        x 12
        d (app/normal-distribution-density mean sd x)]
    (is (= d 0.24197072451914337))))

(deftest variance []
  (let [data (range 1 11)
        v (app/variance data)]
    (is (= v 8.25))))

(deftest confidence-interval-proportion []
  (let [phat 0.576
        n 1000
        p 0.5
        ci (app/confidence-interval-proportion phat n p)]
    (is (= ci (list 0.5450096789303498 0.6069903210696501)))))

(deftest get-area []
  (let [a 1
        b 3
        f #(identity %)
        a (app/get-area f a b)]
    (is (= a 4))))

(deftest get-rectangles-under-curve []
  (let [f #(identity %)
        a 0
        b 5
        n 5
        rects (app/get-rectangles-under-curve f a b n)
        expected-rects (loop [rs '() itr a]
                         (if (= itr b)
                           (reverse rs)
                           (recur (conj rs (app/Rectangle. itr 0 (/ b n) (+ 0.5 itr))) (inc itr))))]
    (is (= rects expected-rects))))

(deftest integrate []
  (let [a 1.5
        b 5.75
        n 100
        f #(identity %)
        definite-integral (app/integrate f a b n)
        rounded (round-to definite-integral 2)]
    (is (= rounded 15.41))))

(deftest standard-normal-distribution-cumulative []
  (let [x -1.75
        prob (app/standard-normal-distribution-cumulative x)]
    (is (= prob 1))))

(deftest margin-of-error []
  (let [z 1.96 
        phat 0.5
        n 100
        moe (app/margin-of-error z phat n)]
    (is (= moe 1))))

(deftest hypothesis-test-proportion-double-tailed
  (let [phat 0.52
        p 0.5
        n 1096
        hyp (app/hypothesis-test-proportion-double-tailed p phat n)]
    (is (= hyp 1))))

(deftest factorial
  (let [n 10
        fact (app/factorial n)]
    (is (= fact 1))))

(deftest gamma-function-real
  (let [n 0.1
        g (app/gamma-function-real n)]
    (is = n 1)))
