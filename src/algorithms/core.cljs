(ns algorithms.core
  (:require [algorithms.percolation :refer [make-percolation open? percolates? open
                                            number-of-open-sites index->row-col]]
            [cljs.core.async :refer [mix]]
            [clojure.edn :refer [read-string]]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom :include-macros true]))

(enable-console-print!)

(reacl/defclass algorithms-app
  this app-state []

  render
  ;; 20 x 20
  ;; map to 400 x 400
  ;; 1 site is 20 x 20
  ;; padding is 2, 18 x 18
  ;;
  (let [n (:n app-state)
        n*n (* n n)]
    (println "**** # Open" (number-of-open-sites (:percolation app-state))
             " - " (percolates? (:percolation app-state)))
    (dom/div
     (dom/button {:disabled (percolates? (:percolation app-state))
                  :onclick #(reacl/send-message! this :run-10)}
                 "몬테카를로!(x10)")
     (dom/br)
     (dom/svg {:version "1.1" :base-profile "full" :width n*n :height n*n :xmlns "http://www.w3.org/2000/svg"}
              (dom/rect {:width "100%" :height "100%" :fill "LightCyan"})
              (for [x (range 0 n)
                    y (range 0 n)]
                (dom/keyed (str x ":" y)
                           (dom/rect {:x (+ (* x n) 2) :y (+ (* y n) 2)
                                      :width (- n 2) :height (- n 2)
                                      :fill (if (open? (:percolation app-state) x y)
                                              "White"
                                              "Black")}))))))
  handle-message
  (fn [msg]
    (let [percolation (:percolation app-state)
          rand-max (* (:n app-state) (:n app-state))]
      (loop [i 30]
;;        (println "**** # Open" (number-of-open-sites percolation) " - " (percolates? percolation))
        (when (and (not (percolates? percolation)) (pos? i))
          (let [[row col] (->> (rand-int rand-max) (index->row-col percolation))]
            (if (open? percolation row col)
              (recur i)
              (do (open percolation row col)
                  (recur (dec i)))))))
      (reacl/return :app-state (assoc app-state :percolation percolation))
      #_
      (reacl/return :app-state (-> (assoc-in app-state [:percolation :sites] (:sites percolation))
                                   (assoc-in [:percolation :components] (:components percolation)))))))

(reacl/render-component
 (.getElementById js/document "app")
 algorithms-app
 {:n 20 :percolation (make-percolation 20)})
