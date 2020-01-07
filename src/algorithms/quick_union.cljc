(ns algorithms.quick-union
  (:refer-clojure :exclude [find count]))

;;;
;;; Algorithms, pp224
;;;
;;; Quick union
;;; Use the array as forest of trees, lazy approach.
(defrecord Components [vector count])

(defn find [components p]
  (let [v (:vector components)]
    (loop [idx p]
      (let [id (nth v idx)]
        (if (= id idx) ;; find the root
          id
         (recur id))))))

(defn union [components p q]
  (let [pid (find components p)
        qid (find components q)]
    (if (= pid qid)
      components
      (-> components
          (assoc-in [:vector pid] qid)
          (update :count dec)))))

(defn connected? [components p q]
  (= (find components p) (find components q)))

(defn count [components]
  (:count components))

(defn make-components [n]
  (->Components (vec (range n)) n))

;;;
;;; Examples
;;; 
(defn connect-if-not-connected [components p q]
  (if (connected? components p q)
    (do (println "*** Already connected: " p " + " q)
        components)
    (do (println "*** Connect: " p " + " q)
        (union components p q))))

(defn session [n pairs]
  (loop [[[p q] & more-pairs] pairs
         components (make-components n)]
    (if (and p q)
      (recur more-pairs (connect-if-not-connected components p q))
      components)))

;; (session 10 [[4 3] [3 8] [6 5] [9 4] [2 1] [8 9] [5 0] [7 2] [6 1] [1 0] [6 7] [7 3]])
;; (session 10 [[1 2] [3 4] [5 6] [7 8] [7 9] [2 8] [0 5] [1 9]])


;;;
;;; Analysis
;;;
;;; (Worst case)
;;; make-components: N
;;; union: N
;;; find: N
;;; connected?: 2N
;;;
;;; Now find is a bottleneck.
