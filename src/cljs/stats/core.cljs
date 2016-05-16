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

(defn variance 
  ([coll] (Math/pow (standard-deviation coll) 2))
  ([coll sample] (Math/pow (standard-deviation coll sample) 2)))

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

(defrecord Rectangle [x y width height])

(defn get-rectangles-under-curve [f a b n]
  (let [width (/ (- b a) n)]
    (loop [rects '() itr a]
      (if (>= itr b)
        (reverse rects)
        (recur (conj rects (Rectangle. itr 0 width (f (/ (+ itr (+ itr width)) 2)))) (+ itr width))))))

(defn get-area [f a b]
  (let [midpoint (/ (+ a b) 2)
        height (f midpoint)
        width (- b a)]
    (* width height)))

(defn integrate 
  ([rects] (reduce + (map #(* (:width %) (:height %)) rects)))
  ([f a b n] (integrate (get-rectangles-under-curve f a b n))))

(defn standard-normal-distribution-cumulative [x]
  (integrate #(standard-normal-distribution-cumulative %) -5.0 x 100))
  
(def svg-width 500)
(def svg-height 500)

(defn create-svg [width height]
  (-> js/d3 (.select "body") (.append "svg") (.attr "width" width) (.attr "height" height)))

(defn margin-of-error [z phat n]
  (let [numerator (* phat (- 1 phat))
        denominator n
        sqrt (Math/sqrt (/ numerator denominator))]
    (* z sqrt)))

(defn hypothesis-test-proportion-double-tailed [p phat n]
  "double tailed = probability that zscore is less than -phat or greater than +phat
   p is sample proportion, phat is hypothesized value"
  (let [diff (- p phat)
        moe (margin-of-error 1.96 phat n)
        z-score (/ diff moe)
        std-normal-prob (standard-normal-distribution-cumulative z-score)]
    (* std-normal-prob 2)))

(defn test-svg []
  (let [svg (create-svg svg-width svg-height)
        dataset '(1 2 3 4 5)
        circles (-> svg (.selectAll "circle") (.data (apply array dataset)))]
    (-> circles .enter (.append "circle") (.attr "cx" #(* 10 %)) (.attr "cy" 50) (.attr "r" 50))))

(defn display-rectangles [f svg-width svg-height a b n]
  (let [svg (create-svg svg-width svg-height)
        dataset (get-rectangles-under-curve f a b n)
        rects (-> svg (.selectAll "rect") (.data (apply array dataset)))]
    (-> rects .enter (.append "rect") (.style "fill" "steelblue") (.attr "x" #(* 10 (:x %))) (.attr "y" 10) (.attr "width" #(* 10 (:width %))) (.attr "height" #(* 10 (:height %))))))

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
    (standard-deviation sample-coll true)))

(comment(defn) init []
  (let [random-mean-generator (by-id "generate-random-mean")]
    (set! (.-onclick random-mean-generator) set-random-mean-html!)))
