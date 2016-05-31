(ns stats.hypothesis-tests
  (:require [stats.common :as common]
            [stats.distributions :as dist]))


(comment
"Significance level (or p value) is the difference between 
hypothesized mean and obesrved mean, divided by the standard error
of hypothesized mean.")
(defn hypothesis-test-proportion-double-tailed [p phat n]
  "double tailed = probability that zscore is less than -phat or greater than +phat
   p is sample proportion, phat is hypothesized value
   if p <= alpha (where alpha = 1 - significance level), reject H0 (return true)."
  (let [diff (- p phat)
        sd (Math/sqrt (/ (* p (- 1 p)) n))
        z-score (/ diff sd)
        sndf dist/StandardNormalDistributionFunction
        std-normal-prob (dist/cumulative-density sndf z-score)
        pval (* std-normal-prob 2)]
    (<= pval 0.05)))

(comment "not 100% sure this is right. pval is correct as far as cdf goes, but don't know why we don't use alpha")
(defn hypothesis-test-mean-double-tailed [observed-mean hyp-mean std-dev n]
  (let [t-score (/ (- observed-mean hyp-mean) (* std-dev (Math/sqrt n)))
        dof (dec n)
        tdf (dist/StudentsTDistributionFunction. dof)
        pval (* 2 (dist/cumulative-density tdf t-score))]
    (<= pval 0.05)))
