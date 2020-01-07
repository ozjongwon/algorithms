(ns algorithms.quick-find
  (:refer-clojure :exclude [find count]))

;;;
;;; Algorithms, pp216
;;;
;;; Quick Find
(defn union [component p q]
  (let [pid (find component p)
        qid (find component q)]
    (if (= pid qid)
      component
      (mapv (fn [v i]
              (if (= v pid)
                qid
                v))
            component
            (range)))))

(defn find [component p]
  (nth component p))

(defn connected? [component p q]
  (= (find component p) (find component q)))

(defn count [component]
  (clojure.core/count component))

(defn make-component [n]
  (vec (range n)))

;;;
;;; Examples
;;; 
(defn connect-if-not-connected [component p q]
  (if (connected? component p q)
    (do (println "*** Already connected: " p " + " q)
        component)
    (do (println "*** Connect: " p " + " q)
        (union component p q))))

(defn ex1 []
  (loop [[[p q] & more-pairs] [[4 3] [3 8] [6 5] [9 4] [2 1] [8 9] [5 0] [7 2] [6 1] [1 0] [6 7]]
         component (make-component 10)]
    (println "*** " p ":" q)
    (if (and p q)
      (recur more-pairs (connect-if-not-connected component p q))
      component)))
