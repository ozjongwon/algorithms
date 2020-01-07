(ns algorithms.core
  (:require [cljs.core.async :refer [mix]]
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
  (dom/svg {:version "1.1" :base-profile "full" :width 400 :height 400 :xmlns "http://www.w3.org/2000/svg"}
           (dom/rect {:width "100%" :height "100%" :fill "LightCyan"})
           (for [x (range 0 20)
                 y (range 0 20)]
             (dom/keyed (str x ":" y)
                        (dom/rect {:x (+ (* x 20) 2) :y (+ (* y 20) 2) :width 18 :height 18 :fill "Black"})))
           ;; (dom/rect {:x 2 :y 2 :width 18 :height 18 :fill "Black"})
           ;; (dom/rect {:x 22 :y 2 :width 18 :height 18 :fill "Black"})
           )

;;   <rect width="100%" height="100%" fill="red" />

;;   <circle cx="150" cy="100" r="80" fill="green" />

;;   <text x="150" y="125" font-size="60" text-anchor="middle" fill="white">SVG</text>

;; </svg>

  ;;handle-message
  )

(reacl/render-component
 (.getElementById js/document "app")
 algorithms-app
 {})
