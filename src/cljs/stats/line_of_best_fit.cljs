(ns stats.line-of-best-fit
  (:require [stats.common :as common]
            [stats.basics :as basics :refer [variance]]))

(defprotocol RegressionLine
  (slope [this] "Slope of a reg. line")
  (y-intercept [this] "Y intercept of a reg. line")
  (line [this] "Value at a point of a reg. line")
  (correlation [this] "Correlation between input and output xs and ys.")
  (residuals [this] "Residuals of a reg. line")
  (residuals-variance [this] "Variance of the residuals."))

(deftype LinearRegressionLine [xs ys]
    RegressionLine
    (slope [this]
        (let [n (count xs)
              sumx (common/sum xs)
              sumy (common/sum ys)
              xy (common/dot-product xs ys)
              xx (common/sum (map #(* % %) xs))
              a (* n xy)
              b (* sumx sumy)
              c (* n xx)
              d (Math/pow sumx 2)
              e (- a b)
              d (- c d)]
          (/ e d)))

    (y-intercept [this]
      (let [n (count xs)
            s (slope this)
            a (/ 1 n)
            b (common/sum ys)
            c (* s (common/sum xs))
            d (- b c)]
        (* a d)))

    (line [this]
      (let [m (slope this)
            b (y-intercept this)]
        (fn [x] (+ b (* m x)))))
    
    (residuals [this]
      (let [m (slope this)
            b (y-intercept this)
            lobf (line this)]
        (map #(let [x (* m %1)] (- %2 b x)) xs ys)))

    (correlation [this]
      (let [sumx (common/sum xs)
            sumy (common/sum ys)
            meanx (common/mean xs)
            meany (common/mean ys)
            xdiffs (map #(- % meanx) xs)
            ydiffs (map #(- % meany) ys)
            a (common/dot-product xdiffs ydiffs)
            b (common/sum (map #(Math/pow (- % meanx) 2) xs))
            c (common/sum (map #(Math/pow (- % meany) 2) ys))
            d (* b c)
            e (Math/sqrt d)]
        (/ a e)))
    
    (residuals-variance [this]
      (let [vary (basics/variance ys)
            r (correlation this)]
        (* vary (- 1 (* r r))))))
