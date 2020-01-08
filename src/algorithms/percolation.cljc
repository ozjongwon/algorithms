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
(defrecord PercolationStats [n trials confidence thresholds mean stddev confidence-low confidence-high])

(defn make-percolation-stats [n trials confidence]
  (->PercolationStats n trials confidence [] nil nil nil nil))

(defn set-mean [percolation-stats]
  (assoc percolation-stats :mean (/ (apply + (:thresholds percolation-stats)) (:trials percolation-stats))))

(defn set-stddev [percolation-stats]
  (let [mean (:mean percolation-stats)]
    (assoc percolation-stats :stddev (->  (fn [acc x]
                                            (let [x-mean (- x mean)]
                                              (+ acc (* x-mean x-mean))))
                                          (reduce 0 (:thresholds percolation-stats))
                                          (/ (dec (:trials percolation-stats)))))))

(defn set-confidence-ranges [percolation-stats]
  (let [{:keys [confidence trials mean stddev]} percolation-stats
        conf-const (* confidence (Math/sqrt (/ stddev trials)))]
    (-> percolation-stats
        (assoc :confidence-low (- mean conf-const) :confidence-high (+ mean conf-const)))))

(defn monte-carlo-simulation [n trials confidence]
  (let [n*n (* n n)]
   (loop [n-run trials percolation-stats (make-percolation-stats n trials confidence)]
     (if (zero? n-run)
       (-> (set-mean percolation-stats)
           (set-stddev)
           (set-confidence-ranges))
       (let [percolation (make-percolation n)]
         (while (not (percolates? percolation))
           (let [[row col] (->> (rand-int n*n) (sites-index->row-col percolation))]
             (when-not (open? percolation row col)
               (open percolation row col))))
         (recur (dec n-run) (update percolation-stats :thresholds conj (/ (number-of-open-sites percolation) n*n))))))))

;;(monte-carlo-simulation 20 40 1.96)

(defn run-monte-carlo [n trials]
  (let [{:keys [mean stddev confidence-low confidence-high]} (monte-carlo-simulation n trials 1.96)]
    {:mean (float mean)
     :stddev (float stddev)
     :confidence-low confidence-low
     :confidence-high confidence-high}))
