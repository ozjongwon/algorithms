(ns algorithms.merge-sort
  (:refer-clojure :exclude [sort merge]))

(defn merge [^objects arr1 ^objects arr2 l m h]
  (let [lupper (inc m)
        rupper (inc h)]
    (loop [i l j (inc m) k l]
      (cond (> i m) (do
                      (doseq [[j k] (mapv list (range j rupper) (range k rupper))]
                        (aset arr2 k (aget arr1 j)))
                      arr2)

            (> j h) (do
                      (doseq [[i k] (mapv list (range i lupper) (range k rupper))]
                        (aset arr2 k (aget arr1 i)))
                      arr2)

            (< (aget arr1 j) (aget arr1 i)) (do (aset arr2 k (aget arr1 j))
                                                (recur i (inc j) (inc k)))

            :else
            (do (aset arr2 k (aget arr1 i)) (recur (inc i) j (inc k)))))))

(defn sort [^objects arr]
  (let [n (alength arr)]
    (loop [sz 1 arr1 arr arr2 (object-array n)]
      (if (< sz n)
        (do (loop [l 0]
              (when (<= l (- n sz))
                (merge arr1 arr2 l (dec (+ l sz)) (min (dec (+ l sz sz)) (dec n)))
                (recur (+ l sz sz))))
            (recur (+ sz sz) arr2 arr1))
        arr1))))
