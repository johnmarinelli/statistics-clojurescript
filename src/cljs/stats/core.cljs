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

(defn get-mean [coll]
  (/ (reduce + coll) (count coll)))

(defn random-mean [begin end n]
  (let [rand-series (map #(random rand-min rand-max) (range 0 n))]
    (get-mean rand-series)))

(defn population-std-deviation 
  ([coll sample]
   (let [n (if sample (- (count coll) 1) (count coll))
         mean (get-mean coll)
         sum (reduce + (map #(Math/pow (- % mean) 2) coll))]
     (Math/pow (/ sum n) 0.5)))
  ([coll] (population-std-deviation coll false)))
  
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

(defn init []
  (let [random-mean-generator (by-id "generate-random-mean")]
    (set! (.-onclick random-mean-generator) set-random-mean-html!)))

(println "this is nice")

(set! (.-onload js/window) init)
