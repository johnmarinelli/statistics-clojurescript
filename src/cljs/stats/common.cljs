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
