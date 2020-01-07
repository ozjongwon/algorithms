(ns algorithms.percolation
  (:refer-clojure :exclude [find])
  (:require [algorithms.path-compression-weighted-quick-union :refer [find union connected? make-components]]))

(defrecord Percolation [sites n components])
(defn make-percolation [n]
  ;; make n x n sites
  ;; 0: blocked
  ;; 1: opened
  (let [n*n (* n n)
        components (make-components (+ n*n 2))] ;; add two sites - top and bottom
    (dotimes [i n]
      (union components 0 (inc i))) ;; union top root sites
    (let [bottom-site (inc n*n)]
     (dotimes [i n]
       (union components bottom-site (- n*n i)))) ;; union bottom root sites
    (let [arr (make-array Integer/TYPE n*n)]
      (dotimes [i n*n]
        (aset arr i 0))
      (->Percolation arr n components))))

(defn- row-col->index [{:keys [n]} row col]
  (+ (* n row) col))

(defn index->row-col [{:keys [n]} index]
  [(quot index n) (rem index n)])

(defn- row-col->neighbour-indexes [{:keys [n]} row col]
  [[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]])

(defn- index->neighbour-indexes [{:keys [n]} index]
  (let [n*n (* n n)
        top (- index n)
        bottom (+ index n)
        left (dec index)
        right (inc index)]
    (cond-> []
      (<= 0 top) (conj top)
      (< bottom n*n) (conj bottom)
      (pos? (mod index n)) (conj left)
      (pos? (mod (inc index) n)) (conj right))))

(defn open [percolation row col]
  (let [idx (row-col->index percolation row col)]
   (aset ^ints (:sites percolation) idx 1)
   (doseq [n-idx (index->neighbour-indexes percolation idx)]
     (when (apply open? percolation (index->row-col percolation n-idx))
       (union (:components percolation) (inc idx) (inc n-idx))))))

(defn open? [percolation row col]
  (= (aget ^ints (:sites percolation) (row-col->index percolation row col)) 1))

(defn full? [percolation row col]
  ;; A full site is an open site that can be connected to an open site in the top row
  ;; via a chain of neighboring (left, right, up, down) open sites.
  (connected? (:components percolation) 0 (inc (row-col->index percolation row col))))

(defn number-of-open-sites [percolation]
  (apply + (:sites percolation)))

(defn percolates? [percolation]
  (connected? (:components percolation) 0 (inc (* (:n percolation) (:n percolation)))))

;;;;;;
(defn monte-carlo-simulation [n]
  (let [rand-max (* n n)
        percolation (make-percolation n)]
    (while (not (percolates? percolation))
      (let [[row col] (->> (rand-int rand-max) (index->row-col percolation))]
       (when-not (open? percolation row col)
         (open percolation row col))))
    percolation))
