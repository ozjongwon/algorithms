(ns algorithms.path-compression-weighted-quick-union
  (:refer-clojure :exclude [find count]))
;;;
;;; Algorithms, pp231
;;;
;;; Improvement from weighted-quick-union:
;;;
;;; NOTE:
;;; To implement 'Path Compression', mutating component vector is required.
;;; (Now renamed to array)

(defrecord Components [^ints array sizev count])

(defn find [components p]
  (let [arr (:array components)]
    (loop [idx p]
      (let [id (aget ^ints arr idx)]
        (if (= id idx) ;; find the root
          id
          (do
            (aset ^ints arr idx (aget ^ints arr id))
            (recur id)))))))

(defn union [components p q]
  (let [pid (find components p)
        qid (find components q)
        {:keys [^ints array sizev]} components]
    (if (= pid qid)
      components
      (let [[smaller ^int taller] (if (< (nth sizev pid) (nth sizev qid))
                               [pid qid]
                               [qid pid])]
        (aset array smaller taller)
        (-> components
            (update-in [:sizev taller] + (get-in components [:sizev smaller]))
            (update :count dec)
         )))))

(defn connected? [components p q]
  (= (find components p) (find components q)))

(defn count [components]
  (:count components))

(defn make-components [n]
  (let [array (make-array Integer/TYPE n)]
    (dotimes [i n]
      (aset ^ints array i i))
    (->Components array (vec (repeat n 1))  n)))

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
;;; union: close to linear ~ 1
;;; find: close to linear ~ 1
;;; connected?: ~ 2
;;;
