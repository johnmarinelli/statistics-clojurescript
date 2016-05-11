(ns stats.core
  (:require [domina.core :refer [by-id value set-value!]]
            [clojure.string :as str]))

(enable-console-print!)

(def rand-min 0)
(def rand-max 32)
(def sample-size 100)

(defn random [begin end]
  (let [within (range begin end)
        width (- (count within) 1)]
    (nth within (mod (rand-int 100) width))))

(defn median [coll]
  (let [half-length (/ (count coll) 2)
        idx (Math/floor half-length)]
    (nth coll idx)))

(defn first-quartile [coll]
  (let [half-length (Math/floor (/ (count coll) 2))]
    (median (take half-length coll))))

(defn third-quartile [coll]
  (let [half-length (Math/floor (/ (count coll) 2))]
    (median (take-last half-length coll))))

(defn mean [coll]
  (let [len (count coll)
        sum (reduce + coll)]
    (/ sum len)))

(defn variance 
  ([coll] (Math/pow (standard-deviation coll) 2))
  ([coll sample] (Math/pow (standard-deviation coll sample) 2)))

(defn standard-deviation
  ([coll] (standard-deviation coll false))
  ([coll sample]
   (let [mean (mean coll)
         len (count coll)
         n (if sample (- len 1) len)
         sq-diff (map #(Math/pow (- % mean) 2) coll)
         sum-sq-diff (reduce + sq-diff)
         s-squared (/ sum-sq-diff n)]
     (Math/sqrt s-squared))))

(defn normal-distribution-density [mean sd x]
  (let [denominator (* sd (Math/sqrt (* 2 (.-PI js/Math))))
        exp (* -1 (/ (Math/pow (- x mean) 2) (* 2 (Math/pow sd 2))))
        numerator (Math/pow (.-E js/Math) exp)]
    (/ numerator denominator)))

(defn standard-normal-distribution-density [x]
  (normal-distribution-density 0 1 x))

(defn random-mean [begin end n]
  (let [rand-series (map #(random rand-min rand-max) (range 0 n))]
    (mean rand-series)))

(defn confidence-interval-proportion
  "If p is null, use p = phat"
  ([phat n] (confidence-interval phat n alpha phat))
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

(defn set-random-mean-html! 
  []
  (let [rm (random-mean rand-min rand-max sample-size)]
    (set-value! (by-id "random-mean") rm)))

(defn set-population-standard-deviation-html!
  []
  "Reads a csv list of values and writes the S.D of them to the screen"
  (let [psd-input (value (by-id "pop-sd-sample-input"))
        sample-coll (map #(js/parseInt %) (str/split psd-input #","))
        psd-output (by-id "pop-sd-sample-result")]
    (population-std-deviation sample-coll true)))

(comment(defn) init []
  (let [random-mean-generator (by-id "generate-random-mean")]
    (set! (.-onclick random-mean-generator) set-random-mean-html!)))




