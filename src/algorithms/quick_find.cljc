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

(defn session [n pairs]
  (loop [[[p q] & more-pairs] pairs
         component (make-component n)]
    (if (and p q)
      (recur more-pairs (connect-if-not-connected component p q))
      component)))

(session 10 [[4 3] [3 8] [6 5] [9 4] [2 1] [8 9] [5 0] [7 2] [6 1] [1 0] [6 7]])
(session 10 [[1 2] [3 4] [5 6] [7 8] [7 9] [2 8] [0 5] [1 9]])

;;;
;;; Analysis
;;;
;;; make-component: N
;;; union: N
;;; connected?: 1
;;; find: 1, A connected? calls find twice
;;;
;;; N union on N elements: N ^ 2
;;; 
