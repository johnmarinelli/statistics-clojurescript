(ns stats.distributions
  (:require [stats.common :as common]))

(defprotocol Distribution
  (probability-density [this x] "PDF of a dist. function")
  (cumulative-density [this x] "CDF of a dist. function"))

(deftype NormalDistributionFunction [mean sd]
  Distribution
  (probability-density [this x]
    (let [denominator (* sd (Math/sqrt (* 2 (.-PI js/Math))))
          exp (* -1 (/ (Math/pow (- x mean) 2) (* 2 (Math/pow sd 2))))
          numerator (Math/pow (.-E js/Math) exp)]
      (/ numerator denominator)))
  (cumulative-density [this x]
    (common/integrate #(probability-density this %) -5.0 x 100)))

(def StandardNormalDistributionFunction (NormalDistributionFunction. 0 1))

(deftype StudentsTDistributionFunction [degrees-of-freedom]
  Distribution
  (probability-density [this t]
    (let [numeratorc1 (common/gamma-function (/ (inc degrees-of-freedom) 2))
          sqrt (Math/sqrt (* degrees-of-freedom Math/PI))
          denominatorc1 (* sqrt (common/gamma-function (/ degrees-of-freedom 2)))
          c1 (/ numeratorc1 denominatorc1)
          numeratorc2 (Math/pow t 2)
          denominatorc2 degrees-of-freedom
          expc2 (- (/ (inc degrees-of-freedom) 2))
          c2 (Math/pow (inc (/ numeratorc2 denominatorc2)) expc2)]
      (* c1 c2)))
  (cumulative-density [this t]
    (common/integrate #(probability-density this %) -7.0 t 1000)))
