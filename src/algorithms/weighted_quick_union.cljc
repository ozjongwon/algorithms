(ns algorithms.weighted-quick-union
  (:refer-clojure :exclude [find count]))

;;;
;;; Algorithms, pp227
;;;
;;; Improvement from quick-union:
;;; - Avoid tall trees
;;; - Keep track of size of each tree
;;; - Balance by linking root of smaller tree to root of larger tree
(defrecord Components [vector sizev count])

(defn find [components p]
  (let [v (:vector components)]
    (loop [idx p]
      (let [id (nth v idx)]
        (if (= id idx) ;; find the root
          id
         (recur id))))))

(defn union [components p q]
  (let [pid (find components p)
        qid (find components q)
        size-vector (get components :sizev)]
    (if (= pid qid)
      components
      (let [[smaller taller] (if (< (nth size-vector pid) (nth size-vector qid))
                               [pid qid]
                               [qid pid])]
        (-> (assoc-in components [:vector smaller] taller)
            (update-in [:sizev taller] + (get-in components [:sizev smaller]))
            (update :count dec)
        )))))

(defn connected? [components p q]
  (= (find components p) (find components q)))

(defn count [components]
  (:count components))

(defn make-components [n]
  (->Components (vec (range n)) (vec (repeat n 1))  n))

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
;;; union: lg N
;;; find: lg N
;;; connected?: 2 lg N
;;;
