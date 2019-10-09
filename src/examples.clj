(ns user
  (:require
   [clojupyter.kernel.version :as ver]
   [clojupyter.display :as display]
   [clojure.core.logic :as l])
  #_(:use [clojure.core.logic]))

(defn user-ver []
  (ver/version-string-long))

(def color "black")

(defn dot-base [{:keys [x y r]}]
  [:circle {:cx x :cy y :r r
            :fill color
            :stroke "black" :stroke-width "4"
            }])

(def shift 10)

(defn dot [{:keys [x y] :as prm}] (dot-base {:x (+ x shift) :y (+ y shift) :r 4}))

(def x {:x 0 :y 0})
(def y {:x 100 :y 0})

(def dot-x (dot x))
(def dot-y (dot y))

(->> [dot-x dot-y]
     (into [:svg
            (conj {:xmlns "http://www.w3.org/2000/svg"}
                  #_{:viewBox "-50 -100 200 200"}
                  {:height 200 :width 200}
                  {:transform "translate(10 10)"})
            [:defs
             [:marker {:id "head" :orient "auto" :markerWidth "2" :markerHeight "4"
                       :refX "0.1" :refY "2"}
              ;; triangle pointing right (+x)
              [:path {:d "M0,0 V4 L2,2 Z" :fill "black"}]]]])
     display/hiccup-html)

(def d-prms {:M :moveto
             :L :lineto
             :H :horizontal-lineto
             :V :vertical-lineto
             :C :curveto
             :S :smooth-curveto
             :Q :quadratic-bezier-curve
             :T :smooth-quadratic-bezier-curveto
             :A :elliptical-arc
             :Z :closepath})

(defn path [{:keys [M C] :as prm}]
  (let [{mx :x my :y} m]
    [:path {:d (str "m" mx "," my " "
                    "c" )
            :stroke color
            :marker-end "url(#head)"
            ;; :stroke-width "3"
            :fill "transparent"}]))


(defn path [d]
  "Upper case - absolute, lower case - relative
M = moveto
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
