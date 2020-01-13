(ns algorithms.deque
  (:refer-clojure :exclude [pop empty?]))

(defprotocol Deque
  (empty? [this])
  (size [this])
  (add-first [this item])
  (add-last [this item])
  (remove-first [this])
  (remove-last [this]))

(defrecord ArrayDeque [n front end inc-ratio dec-ratio ^objects array])

(defn make-deque
  ([]
   (make-deque :array))
  ([opt]
   (->ArrayDeque (atom 0) (atom 1) (atom 0) 2 1/4 (atom (object-array 2)))))

(defn- %index [array-dequeue direction ffn efn]
  (let [idx ((case direction
               :front ffn
               :end efn)
             @(get array-dequeue direction))
        alen (alength ^objects @(get array-dequeue :array))]
    (cond (neg? idx) (dec alen)
          (<= alen idx) 0
          :else idx)))

(defn- next-index [array-dequeue direction]
  (%index array-dequeue direction dec inc))

(defn- previous-index [array-dequeue direction]
  (%index array-dequeue direction inc dec))

(defn resize! [^ArrayDeque array-dequeue ratio]
  (let [old-array @(get array-dequeue :array)
        old-len (alength ^objects old-array)
        new-len (-> (* ratio old-len) (int) (inc))
        new-array (object-array new-len)
        front @(get array-dequeue :front)
        end @(get array-dequeue :end)]
    (if (< front end)
      (let [n-elem (- end front)]
        (dotimes [i n-elem]
          (aset new-array i (aget ^objects old-array (+ front i))))
        (reset! (:front array-dequeue) 0)
        (reset! (:end array-dequeue) n-elem))
      (do
        ;; copy 0 to end
        (dotimes [i end]
          (aset new-array i (aget ^objects old-array i)))
        ;; copy end-of-array down to front
        (let [n-elem (- old-len front)]
          (dotimes [i n-elem]
            (aset new-array (- new-len i 1) (aget ^objects old-array (- old-len i 1))))
          (reset! (:front array-dequeue) (- new-len n-elem)))))
    (reset! (:array array-dequeue) new-array)))

(defn- add-item [this item direction]
  (when (<= (alength ^objects @(get this :array)) (* @(:n this) (:inc-ratio this)))
    (resize! this (get this :inc-ratio)))
  (let [idx @(get this direction)
        ^objects array @(get this :array)
        array-len (alength array)]
    (println "*****" idx array-len)
    (aset array idx item)
    (reset! (get this direction) (next-index this direction))
    (swap! (:n this) inc)))

(defn- remove-item [this direction]
  (when (-> (alength ^objects @(get this :array))
            (* (:dec-ratio this))
            (> @(:n this)))
    (resize! this (:dec-ratio this)))
  (let [^objects array @(get this :array)
        idx (previous-index this direction)
        result (aget array idx)]
    (aset array idx nil)
    (reset! (get this direction) idx)
    (swap! (:n this) dec)
    result))

(extend-protocol Deque
  ArrayDeque
  (empty? [this]
    (<= @(:n this) 0))
  (size [this]
    @(:n this))
  (add-first [this item]
    (add-item this item :front))
  (add-last [this item]
    (add-item this item :end))
  (remove-first [this]
    (remove-item this :front))
  (remove-last [this]
    (remove-item this :end)))


