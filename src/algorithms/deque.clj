(ns algorithms.deque
  (:refer-clojure :exclude [pop empty?]))

(defprotocol Deque
  (empty? [this])
  (size [this])
  (add-first [this item])
  (add-last [this item])
  (remove-first [this])
  (remove-last [this]))

;;; Record & internal functions
(defrecord ArrayDeque [n head tail inc-ratio dec-ratio ^objects array])

(defn- inc-index [array-dequeue i]
  (mod (inc i) (capacity array-dequeue)))

(defn- dec-index [array-dequeue i]
  (mod (dec i) (capacity array-dequeue)))

(declare capacity resize!)

(defn- add-item [array-dequeue item direction]
  (when (= (capacity array-dequeue) (+ @(:n array-dequeue) 1))
    (resize! array-dequeue (get array-dequeue :inc-ratio)))
  (let [idx @(get array-dequeue direction)
        ^objects array @(get array-dequeue :array)
        array-len (alength array)]
    (aset array idx item)
    (reset! (get array-dequeue direction) (next-index array-dequeue direction))
    (swap! (:n array-dequeue) inc)))

(defn- remove-item [array-dequeue direction]
  (when (> (/ (capacity array-dequeue) 2) @(:n array-dequeue))
    (resize! array-dequeue (:dec-ratio array-dequeue)))
  (let [^objects array @(get array-dequeue :array)
        idx (previous-index array-dequeue direction)
        result (aget array idx)]
    (aset array idx nil)
    (reset! (get array-dequeue direction) idx)
    (swap! (:n array-dequeue) dec)
    result))

;;;
;;; Public interface
;;;
(defn make-deque
  ([]
   (make-deque :array))
  ([opt]
   (->ArrayDeque (atom 0) (atom 1) (atom 0) 2 1/2 (atom (object-array 2)))))

(defn capacity [array-dequeue]
  (alength ^objects @(get array-dequeue :array)))

(defn resize! [^ArrayDeque array-dequeue ratio]
  (let [start-idx @(get array-dequeue :head)
        ^objects old-array @(get array-dequeue :array)
        new-array (-> (alength old-array) (* ratio) (int) (inc) (object-array))
        n @(get array-dequeue :n)]
    (dotimes [i n] ;; copy from head to tail
      (aset new-array i (aget old-array (inc-index array-dequeue (+ start-idx i)))))
    (reset! (get array-dequeue :array) new-array)
    (reset! (get array-dequeue :head) (dec-index array-dequeue 0))
    (reset! (get array-dequeue :tail) n)))

(extend-protocol Deque
  ArrayDeque
  (empty? [this]
    (<= @(:n this) 0))
  (size [this]
    @(:n this))
  (add-first [this item]
    (add-item this item :head))
  (add-last [this item]
    (add-item this item :tail))
  (remove-first [this]
    (remove-item this :head))
  (remove-last [this]
    (remove-item this :tail)))
