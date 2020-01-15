(ns algorithms.collinear-points
  (:refer-clojure :exclude [sort])
  (:require [algorithms.merge-sort :refer [sort]]))

(defrecord Point [x y])

(defn make-point [x y]
  (->Point x y))

(defn compare-to [p1 p2] ;; compare two points by y-coordinates, breaking ties by x-coordinates
  (let [y-diff (- (:y p1) (:y p2))]
    (if (zero? y-diff)
      (- (:x p1) (:x p2))
      y-diff)))

(defn slope-to [p1 p2] ;; the slope between point p1 and point p2
  (let [y-diff (- (:y p2) (:y p1))
        x-diff (- (:x p2) (:x p1))]
    (cond (and (zero? x-diff) (zero? y-diff)) ##-Inf
          (zero? x-diff) 0
          (zero? y-diff) ##Inf
          :else (/ y-diff x-diff))))

(defn make-slope-key-fn [p]
  (fn [p1]
    (slope-to p p1)))

;; (defrecord FastCollinearPoints [])

;; (defn fast-collinear-points [points]
;;   (sort points (make-slope-key-fn (make-point 0 0)))
;;   )

;; (defn numer-of-segments [])

;; (defn segments [])

;;;
(comment

  (let [point00 (make-point 0 0)]
    (group-by (fn [p1]
                (slope-to point00 p1))
              (-> (object-array (mapv (fn [[x y]]
                                        (make-point x y))
                                      (repeatedly 100 (fn [] (repeatedly 2 #(rand-int 10))))))
                  (sort (make-slope-key-fn point00) -))))

  )
