(ns algorithms.stack
  (:refer-clojure :exclude [pop empty?])
  (:import [java.util LinkedList]))

;; pp 121
(defprotocol Stack
  (push! [this item])
  (pop! [this])
  (empty? [this]))

(defrecord ArrayStack [n inc-ratio dec-ratio ^objects array])

(defn resize! [^ArrayStack array-stack ratio]
  (let [old-array @(:array array-stack)
        new-array (-> (* ratio (alength ^objects old-array))
                      (int)
                      (inc)
                      (object-array))]
    (dotimes [i @(:n array-stack)]
      (aset new-array i (aget ^objects old-array i)))
    (reset! (:array array-stack) new-array)))

(defn make-stack!
  ([]
   (make-stack :linked-list))
  ([opt]
   (case opt
     :linked-list (LinkedList.)
     :array (->ArrayStack (atom 0) 2 1/4 (atom (object-array nil)))))
  ([opt init-args]
   (case opt
     :linked-list (LinkedList. init-args)
     :array (->ArrayStack (atom (count init-args)) 2 1/4 (atom (object-array init-args))))))

(extend-protocol Stack
  LinkedList
  (push! [this item]
    (.push this item))
  (pop! [this]
    (.pop this))
  (empty? [this]
    (.isEmpty this))

  ArrayStack
  (push! [this item]
    (when (<= (alength ^objects @(get this :array)) (* @(:n this) (:inc-ratio this)))
      (resize! this (:inc-ratio this)))
    (aset ^objects @(get this :array) @(:n this) item)
    (swap! (:n this) inc))
  (pop! [this]
    (when (> (* (alength ^objects @(get this :array)) (:dec-ratio this)) @(:n this))
      (resize! this (:dec-ratio this)))
    (let [^objects array @(get this :array)
          idx (dec @(:n this))
          result (aget array idx)]
      (aset array idx nil)
      (swap! (:n this) dec)
      result))
  (empty? [this]
    (<= @(:n this) 0)))

#_
(let [stack (make-stack! :linked-list)]
  (loop [[in & more-ins] (clojure.string/split "to be or not to - be - - that - - - is " #" ")]
    (cond (clojure.core/empty? in) stack
          (= in "-") (do (println "pop -" (pop! stack)) (recur more-ins))
          :else (do (println "push -" in)  (push! stack in) (recur more-ins)))))
#_
(let [stack (make-stack! :array)]
  (loop [[in & more-ins] (clojure.string/split "to be or not to - be - - that - - - is " #" ")]
    (cond (clojure.core/empty? in) stack
          (= in "-") (do (println "pop -" (pop! stack)) (recur more-ins))
          :else (do (println "push -" in)  (push! stack in) (recur more-ins)))))
