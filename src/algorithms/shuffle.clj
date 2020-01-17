(ns algorithms.shuffle)

(defn aswap [^objects arr i j]
  (let [elem-i (aget arr i)]
    (aset arr i (aget arr j))
    (aset arr j elem-i))
  arr)

(defn shuffle! [col]
  (let [n (count col)]
    (if (pos? n)
      (let [^objects array (object-array col)]
        (doseq [i (range 1 n)]
          (aswap array i (rand-int (inc i))))
        (vec array))
      col)))

;;;;

(defn vswap [v i j]
  (assoc v i (nth v j) j (nth v i)))

(defn shuffle1 [v]
  (let [n (count v)]
    (loop [i (- n 1) result v]
      (if (zero? i)
        result
        (recur (dec i) (vswap result i (rand-int (inc i))))))))

(defn shuffle2 [v]
  (let [n (count v)
        end-idx (dec n)]
    (loop [i 0 result v]
      (if (<= end-idx i)
        result
        (recur (inc i) (vswap result i (+ (rand-int (- n i)) i)))))))

(defn shuffle3 [v]
  (let [n (count v)]
    (loop [i 1 result v]
      (if (<= n i)
        result
        (recur (inc i) (vswap result i (rand-int (inc i))))))))

(defn shuffle-bug [v]
  (let [n (count v)]
    (loop [i 0 result v]
      (if (<= n i)
        result
        (recur (inc i) (vswap result i (+ (rand-int (- n i)) i)))))))

(comment
 (defonce counter {[1 2 3] 0
                   [1 3 2] 0
                   [2 1 3] 0
                   [2 3 1] 0
                   [3 2 1] 0
                   [3 1 2] 0})


 (let [v [1 2 3]]
   (loop [i 1000000 counter counter]
     (if (zero? i)
       counter
       (recur (dec i) (update counter (shuffle2 v) inc))))))
