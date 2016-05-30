(ns stats.core
  (:require [domina.core :refer [by-id value set-value!]]
            [clojure.string :as str]
            [stats.distributions :as dist]))

(enable-console-print!)

(def rand-min 0)
(def rand-max 32)
(def sample-size 100)

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

(defn standard-normal-distribution-cumulative [x]
  (integrate #(standard-normal-distribution-density %) -5.0 x 100))
  
(def svg-width 500)
(def svg-height 500)

(defn create-svg [width height]
  (-> js/d3 (.select "body") (.append "svg") (.attr "width" width) (.attr "height" height)))

(defn standard-error [phat n]
  (let [numerator (* phat (- 1 phat))
        denominator n]
    (Math/sqrt (/ numerator denominator))))

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
        std-normal-prob (standard-normal-distribution-cumulative z-score)
        pval (* std-normal-prob 2)]
    (<= pval 0.05)))

(comment
"Todo: make this tail call recursive.  because i'm an asshole like that")
(defn factorial [n]
  (if (<= n 0)
    1
    (* n (factorial (dec n)))))

(defn is-integer? [n] 
  (= (bit-xor n 0) n))

(defn gamma-function [number]
  (if (is-integer? number)
    (let [n-minus-one (dec number)]
      (factorial n-minus-one))
    (if (< number 0.5)
      (/ Math/PI (* (Math/sin (* Math/PI number))
                 (gamma-function (- 1 number))))
      (let [n (dec number)
            c [0.99999999999980993 676.5203681218851 -1259.1392167224028
               771.32342877765313 -176.61502916214059 12.507343278686905
               -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]]
        (* (Math/sqrt (* 2 Math/PI))
          (Math/pow (+ n 7 0.5) (+ n 0.5))
          (Math/exp (- (+ n 7 0.5)))
          (+ (first c)
             (apply + (map-indexed #(/ %2 (+ n %1 1)) (next c)))))))))

(def E (.-E js/Math))

(defn square [n] (* n n))

(defn t-distribution-probability-density [degrees-of-freedom t]
  (let [numeratorc1 (gamma-function (/ (inc degrees-of-freedom) 2))
        sqrt (Math/sqrt (* degrees-of-freedom Math/PI))
        denominatorc1 (* sqrt (gamma-function (/ degrees-of-freedom 2)))
        c1 (/ numeratorc1 denominatorc1)
        numeratorc2 (Math/pow t 2)
        denominatorc2 degrees-of-freedom
        expc2 (- (/ (inc degrees-of-freedom) 2))
        c2 (Math/pow (inc (/ numeratorc2 denominatorc2)) expc2)]
    (* c1 c2)))

(defn t-distribution-cumulative [dof t-score]
  (integrate #(t-distribution-probability-density dof %) -7.0 t-score 1000))

(comment "not 100% sure this is right. pval is correct as far as cdf goes, but don't know why we don't use alpha")
(defn hypothesis-test-mean-double-tailed [observed-mean hyp-mean std-dev n]
  (let [t-score (/ (- observed-mean hyp-mean) (* std-dev (Math/sqrt n)))
        dof (dec n)
        pval (* 2 (t-distribution-cumulative dof t-score))]
    (<= pval 0.05)))

(comment 
"to have high probability of rejecting h0, need high power.
power also gives a measure of how test performs over repeated sampling.
The power of a test is the probability of making a correct decision by rejecting H0 when H0 is false.
The higher the power, the more sensitive it is")
(defn power [] ())

(def sum (partial reduce +))
(defn dot-product [xs ys] (sum (map * xs ys)))
(defn calculate-b1 [xs ys n]
  (let [sumx (sum xs)
        sumy (sum ys)
        xy (dot-product xs ys)
        xx (sum (map #(* % %) xs))
        a (* n xy)
        b (* sumx sumy)
        c (* n xx)
        d (Math/pow sumx 2)
        e (- a b)
        d (- c d)
        f (/ e d)]
    (print f)))

(def sum (partial reduce +))
(comment "TODO: variable length args")
(defn dot-product [xs ys]
  (sum (map * xs ys)))

(defn calculate-b1 [xs ys n]
  (let [sumx (sum xs)
        sumy (sum ys)
        xy (dot-product xs ys)
        xx (sum (map #(* % %) xs))
        a (* n xy)
        b (* sumx sumy)
        c (* n xx)
        d (Math/pow sumx 2)
        e (- a b)
        d (- c d)]
    (/ e d)))

(defn calculate-b0 [xs ys n b1]
  (let [a (/ 1 n)
        b (sum ys)
        c (* b1 (sum xs))
        d (- b c)]
    (* a d)))

(defn line-of-best-fit 
  [xs ys n]
  (let [sum (partial reduce +)
        dot-product (fn [v1 v2]
                        (reduce + (map * v1 v2)))
        b1 (calculate-b1 xs ys n)
        b0 (calculate-b0 xs ys n b1)]
    (fn [x] (+ b0 (* b1 x)))))

(comment "pearson's correlation. measures how much y varies w/ x")
(defn correlation [xs ys]
  (let [sumx (sum xs)
        sumy (sum ys)
        meanx (mean xs)
        meany (mean ys)
        xdiffs (map #(- % meanx) xs)
        ydiffs (map #(- % meany) ys)
        a (dot-product xdiffs ydiffs)
        b (sum (map #(Math/pow (- % meanx) 2) xs))
        c (sum (map #(Math/pow (- % meany) 2) ys))
        d (* b c)
        e (Math/sqrt d)]
    (/ a e)))

(defn residuals [xs ys n]
  (let [b1 (calculate-b1 xs ys n)
        b0 (calculate-b0 xs ys n b1)
        lobf (line-of-best-fit xs ys n)]
    (map #(let [x (* b1 %1)] (- %2 b0 x)) xs ys)))

(comment "Variance of the residuals.  r^2 is coefficient of determination - 
what fraction of var(y) has been explained by this linear relationship?
r^2 = 0 -> no linear relationship
r^2 = 1 -> 100% linear")
(defn residuals-variance [xs ys]
  (let [vary (variance ys)
        r (correlation xs ys)]
    (* vary (- 1 (* r r)))))

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
