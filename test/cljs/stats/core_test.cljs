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

(deftest standard-error []
  (let [phat 0.5
        n 100
        se (app/standard-error phat n)]
    (is (= se 0.05))))

(deftest hypothesis-test-proportion-double-tailed
  (let [phat 0.52
        p 0.5
        n 1096
        hyp (app/hypothesis-test-proportion-double-tailed p phat n)]
    (is (= hyp false))))

(deftest is-integer?-true
  (let [n 1
        ii (app/is-integer? n)]
    (is (= ii true))))

(deftest is-integer?-false
  (let [n 1.5
        ii (app/is-integer? n)]
    (is (= ii false))))


(comment(deftest factorial
   (let [n 10
         fact (app/factorial n)]
     (is (= fact 1)))))

(deftest is-integer?
  (let [n "str"
        ii (app/is-integer? n)]
    (is (= ii false))))

(deftest gamma-function
  (let [n 0.1
        g (app/gamma-function n)]
    (is (= (round-to g 2) 9.51))))

(deftest t-distribution-probability-density
  (let [dof 3
        n 0.25
        t (app/t-distribution-probability-density dof n)]
    (is (= (round-to t 2) 0.35 ))))

(comment(deftest t-distribution-cumulative
   (let [dof 3
         t-score 0.1
         c (app/t-distribution-cumulative dof t-score)]
     (is (= (round-to c 2) 0.53)))))

(comment
"http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-unknown-variance")
(deftest hypothesis-test-double-tailed
  (let [h0 15.4
        h1 14.6
        sd 2.5
        n 35
        r (app/hypothesis-test-mean-double-tailed h1 h0 sd n)]
    (is (= r false))))

(deftest line-of-best-fit
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        n 6
        lobf (app/line-of-best-fit xs ys n)]
    (is (= (round-to (lobf 1) 2) 65.53))))

(deftest correlation
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        c (app/correlation xs ys)]
    (is (= (round-to c 2) 0.53))))

(deftest residuals
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        r (app/residuals xs ys (count xs))]
    (is (= 0 (round-to (reduce + r) 1)))))

(deftest residual-variance
  (let [xs [43 21 25 42 57 59]
        ys [99 65 79 75 87 81]
        rv (app/residuals-variance xs ys n)]
    (is (= 78.64 (round-to rv 2)))))
