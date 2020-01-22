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

(declare capacity resize!)

(defn- inc-index [array-dequeue i]
  (mod (inc i) (capacity array-dequeue)))

(defn- dec-index [array-dequeue i]
  (mod (dec i) (capacity array-dequeue)))

(defn- inc-direction [array-dequeue direction]
  (let [idx @(get array-dequeue direction)]
   (case direction
     :head (dec-index array-dequeue idx)
     :tail (inc-index array-dequeue idx))))

(defn- dec-direction [array-dequeue direction]
  (let [idx @(get array-dequeue direction)]
   (case direction
     :head (inc-index array-dequeue idx)
     :tail (dec-index array-dequeue idx))))

(defn- add-item [array-dequeue item direction]
  (when (= (capacity array-dequeue) (+ @(:n array-dequeue) 1))
    (resize! array-dequeue (get array-dequeue :inc-ratio)))
  (let [idx @(get array-dequeue direction)
        ^objects array @(get array-dequeue :array)
        array-len (alength array)]
    (aset array idx item)
    (reset! (get array-dequeue direction) (inc-direction array-dequeue direction))
    (swap! (:n array-dequeue) inc)))

(defn- remove-item [array-dequeue direction]
  (let [n @(:n array-dequeue)]
    (assert (pos? n) "Overflow!")
    (when (> (/ (capacity array-dequeue) 2) n)
      (resize! array-dequeue (:dec-ratio array-dequeue))))
  (let [^objects array @(get array-dequeue :array)
        idx (dec-direction array-dequeue direction)
        result (aget array idx)]
    (aset array idx nil)
    (reset! (get array-dequeue direction) idx)
    (swap! (:n array-dequeue) dec)
    result))

;;;
;;; Public interface
;;;
(defn make-deque []
  (->ArrayDeque (atom 0) (atom 1) (atom 0) 2 1/2 (atom (object-array 2))))

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
;;;;;;;;;
(comment
  (def dq (make-deque))
  (add-first dq 0)
  (add-first dq 1)
  (add-first dq 2)
  (add-last dq 10)
  (add-last dq 11)
  (add-last dq 12)

  )
