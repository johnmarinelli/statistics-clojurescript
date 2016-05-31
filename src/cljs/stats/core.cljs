(ns stats.core
  (:require [domina.core :refer [by-id value set-value!]]
            [clojure.string :as str]
            [stats.distributions :as dist]))

(enable-console-print!)
  
(def svg-width 500)
(def svg-height 500)

(defn create-svg [width height]
  (-> js/d3 (.select "body") (.append "svg") (.attr "width" width) (.attr "height" height)))


(comment 
"to have high probability of rejecting h0, need high power.
power also gives a measure of how test performs over repeated sampling.
The power of a test is the probability of making a correct decision by rejecting H0 when H0 is false.
The higher the power, the more sensitive it is")
(defn power [] ())

(defn test-svg []
  (let [svg (create-svg svg-width svg-height)
        dataset '(1 2 3 4 5)
        circles (-> svg (.selectAll "circle") (.data (apply array dataset)))]
    (-> circles .enter (.append "circle") (.attr "cx" #(* 10 %)) (.attr "cy" 50) (.attr "r" 50))))

(comment(defn display-rectangles [f svg-width svg-height a b n]
   (let [svg (create-svg svg-width svg-height)
         dataset (get-rectangles-under-curve f a b n)
         rects (-> svg (.selectAll "rect") (.data (apply array dataset)))]
     (-> rects .enter (.append "rect") (.style "fill" "steelblue") (.attr "x" #(* 10 (:x %))) (.attr "y" 10) (.attr "width" #(* 10 (:width %))) (.attr "height" #(* 10 (:height %)))))))

(comment(defn set-random-mean-html! 
   []
   (let [rm (random-mean rand-min rand-max sample-size)]
     (set-value! (by-id "random-mean") rm))))

(comment(defn set-population-standard-deviation-html!
   []
   "Reads a csv list of values and writes the S.D of them to the screen"
   (let [psd-input (value (by-id "pop-sd-sample-input"))
         sample-coll (map #(js/parseInt %) (str/split psd-input #","))
         psd-output (by-id "pop-sd-sample-result")]
     (standard-deviation sample-coll true))))

(comment(defn) init []
  (let [random-mean-generator (by-id "generate-random-mean")]
    (set! (.-onclick random-mean-generator) set-random-mean-html!)))
