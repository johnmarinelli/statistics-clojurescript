(ns stats.common)

(def sum (partial reduce +))

(defn average [coll] 
  (let [n (count coll)
        total (sum coll)]
    (try (/ total n)
         (catch js/Error e e))))

(defn random [begin end]
  (let [within (range begin end)
        width (- (count within) 1)]
    (nth within (mod (rand-int 100) width))))
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

(comment
"Todo: make this trampolined.  because i'm an asshole like that")
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

(defn dot-product [xs ys] (sum (map * xs ys)))
(def mean average)
