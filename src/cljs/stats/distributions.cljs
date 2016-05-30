(ns stats.distributions
  (:require [stats.common :as common]))

(defprotocol Distribution
  (probability-density [ndf x] "PDF of a dist. function")
  (cumulative-density [x] "CDF of a dist. function"))

(deftype NormalDistributionFunction [mean sd]
  Distribution
  (probability-density [ndf x]
    (let [denominator (* sd (Math/sqrt (* 2 (.-PI js/Math))))
          exp (* -1 (/ (Math/pow (- x mean) 2) (* 2 (Math/pow sd 2))))
          numerator (Math/pow (.-E js/Math) exp)]
      (/ numerator denominator)))
  (cumulative-density [x]
    1))

(deftype StudentsTDistribution [mean sd])
