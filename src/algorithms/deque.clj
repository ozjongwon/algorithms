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

(defn- inc-index [array-deque i]
  (mod (inc i) (capacity array-deque)))

(defn- dec-index [array-deque i]
  (mod (dec i) (capacity array-deque)))

(defn- inc-direction [array-deque direction]
  (let [idx @(get array-deque direction)]
   (case direction
     :head (dec-index array-deque idx)
     :tail (inc-index array-deque idx))))

(defn- dec-direction [array-deque direction]
  (let [idx @(get array-deque direction)]
   (case direction
     :head (inc-index array-deque idx)
     :tail (dec-index array-deque idx))))

(defn- add-item [array-deque item direction]
  (when (= (capacity array-deque) (+ @(:n array-deque) 1))
    (resize! array-deque (get array-deque :inc-ratio)))
  (let [idx @(get array-deque direction)
        ^objects array @(get array-deque :array)
        array-len (alength array)]
    (aset array idx item)
    (reset! (get array-deque direction) (inc-direction array-deque direction))
    (swap! (:n array-deque) inc)))

(defn- remove-item [array-deque direction]
  (let [n @(:n array-deque)]
    (assert (pos? n) "Overflow!")
    (when (> (/ (capacity array-deque) 2) n)
      (resize! array-deque (:dec-ratio array-deque))))
  (let [^objects array @(get array-deque :array)
        idx (dec-direction array-deque direction)
        result (aget array idx)]
    (aset array idx nil)
    (reset! (get array-deque direction) idx)
    (swap! (:n array-deque) dec)
    result))

;;;
;;; Public interface
;;;
(defn make-deque []
  (->ArrayDeque (atom 0) (atom 1) (atom 0) 2 1/2 (atom (object-array 2))))

(defn capacity [array-deque]
  (alength ^objects @(get array-deque :array)))

(defn resize! [^ArrayDeque array-deque ratio]
  (let [start-idx @(get array-deque :head)
        ^objects old-array @(get array-deque :array)
        new-array (-> (alength old-array) (* ratio) (int) (inc) (object-array))
        n @(get array-deque :n)]
    (dotimes [i n] ;; copy from head to tail
      (aset new-array i (aget old-array (inc-index array-deque (+ start-idx i)))))
    (reset! (get array-deque :array) new-array)
    (reset! (get array-deque :head) (dec-index array-deque 0))
    (reset! (get array-deque :tail) n)))

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
