(ns stats.confidence-intervals
  (:require [stats.common :as common]))

(defn confidence-interval-proportion
  "If p is null, use p = phat"
  ([phat n] (confidence-interval-proportion phat n phat))
  ([phat n p]
   (let [z 1.96
         margin-of-error (/ p (Math/sqrt n))
         z-moe (* z margin-of-error)]
     (list (- phat z-moe) (+ phat z-moe)))))

(defn confidence-interval-mean
  ([sample-mean std-deviation n]
   (let [z 1.96
         margin-of-error (/ std-deviation (Math/sqrt n))
         z-moe (* z margin-of-error)]
     (list (- sample-mean z-moe) (+ sample-mean z-moe)))))

