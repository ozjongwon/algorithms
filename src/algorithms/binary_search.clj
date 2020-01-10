;; pp 47
(ns algorithms.binary-search)

(defn binary-search [vec target]
  (loop [low 0 high (dec (count vec))]
    (if (<= low high)
      (let [mid (+ low (quot (- high low) 2))
            mid-val (get vec mid)]
        (cond (> mid-val target) (recur low (dec high))
              (< mid-val target) (recur (inc low) high)
              :else mid))
      nil)))
