(ns algorithms.percolation
  (:refer-clojure :exclude [find])
  (:require [algorithms.path-compression-weighted-quick-union :refer [find union connected? make-components]]))

(defrecord Percolation [sites n components])
(defn make-percolation [n]
  ;; make n x n sites
  ;; 0: blocked
  ;; 1: opened
  ;; 2: opened & unioned
  (let [n*n (* n n)
        components (make-components (+ n*n 2))] ;; add two sites - top and bottom
    (dotimes [i n]
      (union components 0 (inc i))) ;; union top root sites
    (let [bottom-site (inc n*n)]
     (dotimes [i n]
       (union components bottom-site (- n*n i)))) ;; union bottom root sites
    (let [arr ^ints (make-array Integer/TYPE n*n)]
      (dotimes [i n*n]
        (aset arr i 0))
      (->Percolation arr n components))))

(defn sites-row-col->index [{:keys [n]} row col]
  (+ (* n col) row))

(defn sites-index->row-col [{:keys [n]} index]
  [(rem index n) (quot index n)])

(defn- sites-index->neighbour-indexes [{:keys [n]} index]
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

(defn open? [percolation row col]
  (pos? (aget ^ints (:sites percolation) (sites-row-col->index percolation row col))))

(defn open [percolation row col]
  (let [idx (sites-row-col->index percolation row col)
        indexes-to-union (->> (sites-index->neighbour-indexes percolation idx)
                              (filterv #(apply open? percolation (sites-index->row-col percolation %))))
        ^ints sites (:sites percolation)]
    (aset sites idx 1) ;; open
    #_
    (when (or (zero? col) (zero? row))
      (println ">>> (x,y) == (" row "," col ") - idx: " idx " unions: " indexes-to-union))
    (doseq [n-idx indexes-to-union]
      #_
      (when (or (zero? col) (zero? row))
        (println ">>>*** UNION (nx,ny) == " (sites-index->row-col percolation n-idx)))
      (union (:components percolation) (inc idx) (inc n-idx)))))

(defn full? [percolation row col]
  ;; A full site is an open site that can be connected to an open site in the top row
  ;; via a chain of neighboring (left, right, up, down) open sites.

  ;; (connected? (:components percolation) 0 (inc (sites-row-col->index percolation row col)))
  (let [conn? (connected? (:components percolation) 0 (inc (sites-row-col->index percolation row col)))]
    conn?))

(defn number-of-open-sites [percolation]
  (reduce #(+ %1 (if (pos? %2)
                   1
                   0))
          (:sites percolation)))

(defn percolates? [percolation]
  (connected? (:components percolation) 0 (inc (* (:n percolation) (:n percolation)))))

;;;;;;
(defn monte-carlo-simulation [n]
  (let [rand-max (* n n)
        percolation (make-percolation n)]
    (while (not (percolates? percolation))
      (let [[row col] (->> (rand-int rand-max) (sites-index->row-col percolation))]
       (when-not (open? percolation row col)
         (open percolation row col))))
    percolation))
