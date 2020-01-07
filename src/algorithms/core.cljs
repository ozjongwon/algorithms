(ns algorithms.core
  (:require [algorithms.percolation :refer [make-percolation open? percolates? open
                                            number-of-open-sites index->row-col row-col->index]]
            [cljs.core.async :refer [mix]]
            [clojure.edn :refer [read-string]]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom :include-macros true]))

(enable-console-print!)

(reacl/defclass algorithms-app
  this app-state []

  render
  (let [n (:n app-state)
        n*n (* n n)
        percolation (:percolation app-state)
        sites (:sites percolation)]
    (dom/div
     (dom/button {:disabled (percolates? percolation)
                  :onclick #(reacl/send-message! this percolation)}
                 "몬테카를로!(x10)")
     (dom/br)
     (dom/svg {:version "1.1" :base-profile "full" :width n*n :height n*n :xmlns "http://www.w3.org/2000/svg"}
              (dom/rect {:width "100%" :height "100%" :fill "LightCyan"})
              (for [x (range 0 n)
                    y (range 0 n)
                    :let [idx (row-col->index percolation x y)]]
                (dom/keyed idx
                           (dom/rect {:x (+ (* x n) 2) :y (+ (* y n) 2)
                                      :width (- n 2) :height (- n 2)
                                      :fill (case (aget sites idx)
                                              0 "Black"
                                              1 "White"
                                              2 "DeepSkyBlue")}))))))
  handle-message
  (fn [percolation]
    (let [rand-max (* (:n percolation) (:n percolation))]
      (loop [i 30]
        (when (and (not (percolates? percolation)) (pos? i))
          (let [[row col] (->> (rand-int rand-max) (index->row-col percolation))]
            (if (open? percolation row col)
              (recur i)
              (do (open percolation row col)
                  (recur (dec i)))))))
      (reacl/return :app-state (assoc app-state :num-open (number-of-open-sites percolation))))))

(reacl/render-component
 (.getElementById js/document "app")
 algorithms-app
 (let [percolation (make-percolation 20)]
   {:n 20 :percolation percolation :num-open 0}))
