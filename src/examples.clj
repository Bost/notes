(ns examples
  (:require
   [clojupyter.kernel.version :as ver]
   [clojupyter.display :as display]
   [clojure.core.logic :as l])
  #_(:use [clojure.core.logic]))

(defn user-ver []
  (ver/version-string-long))

(def color "black")

(defn dot-base [x y r]
  [:circle {:cx x :cy y :r r
            :fill color
            :stroke "black" :stroke-width "4"
            }])

(defn dot [x y] (dot-base x y 4))

(defn path [d]
  "M = moveto
L = lineto
H = horizontal lineto
V = vertical lineto
C = curveto
S = smooth curveto
Q = quadratic Bézier curve
T = smooth quadratic Bézier curveto
A = elliptical Arc
Z = closepath"
  [:path {:d d :stroke color
                       :marker-end "url(#head)"
                       ;; :stroke-width "3"
                       :fill "transparent"}])

(defn go []
  ;; https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
  (display/hiccup-html
   [:svg
    (conj {:xmlns "http://www.w3.org/2000/svg"}
          #_{:viewBox "-50 -100 200 200"}
          {:height 200 :width 200}
          {:transform "translate(0 100)"})
    [:defs
     [:marker {:id "head" :orient "auto" :markerWidth "2" :markerHeight "4"
               :refX "0.1" :refY "2"}
      ;; triangle pointing right (+x)
      [:path {:d "M0,0 V4 L2,2 Z" :fill "black"}]]]
    (path "M 10 10 C 20 20, 40 20, 50 10")
    (path "M 70 10 C 70 20, 120 20, 120 10")
    (path "M 130 10 C 120 20, 180 20, 170 10")
    (path "M 10 60 C 20 80, 40 80, 50 60")
    (path "M 70 60 C 70 80, 110 80, 110 60")
    (path "M 130 60 C 120 80, 180 80, 170 60")
    (path "M 10 110 C 20 140, 40 140, 50 110")
    (path "M 70 110 C 70 140, 110 140, 110 110")
    (path "M 130 110 C 120 140, 180 140, 170 110")
    (dot 140 140)]))
