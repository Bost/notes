(ns user
  (:require
   [clojupyter.kernel.version :as ver]
   [clojupyter.display :as display]
   [clojure.core.logic :as l]
   [clojure.algo.generic.functor :as f])
  #_(:use [clojure.core.logic]))

(defn user-ver []
  (ver/version-string-long))

(def color "black")
(def dot-radius 4)

(defn dot-base [{:keys [x y r]}]
  [:circle {:cx x :cy y :r r
            :fill color
            :stroke "black" :stroke-width "4"
            }])

(defn shift-by-dot-radius [])
(defn dot [{:keys [x y] :as prm}] (dot-base (conj
                                             (f/fmap (fn [val] (+ val dot-radius)) prm)
                                             {:r dot-radius})))

(def x {:x 0 :y 0})
(def y {:x 100 :y 0})

(def dot-x (dot x))
(def dot-y (dot y))

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

(defn path
  [{:keys [M C] :as prm}]
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
  (let [{mx :x my :y} M]
    [:path {:d (str "M" mx "," my " "
                    "C" C)
            :stroke color
            :marker-end "url(#head)"
            ;; :stroke-width "3"
            :fill "transparent"}]))

(def elems [{:M {:x 10 :y 10} :C "20 20, 40 20, 50 10"}])

(defn go [elems]
  {:pre [(vector? elems)]}
  ;; https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
  (->> (map path elems)
       (into [:svg
              (conj {:xmlns "http://www.w3.org/2000/svg"}
                    #_{:viewBox "-50 -100 200 200"}
                    {:height 200 :width 200}
                    ;; translate shifts everything
                    {:transform "translate(0 0)"})
              [:defs
               [:marker {:id "head" :orient "auto" :markerWidth "2"
                         :markerHeight "4" :refX "0.1" :refY "2"}
                ;; triangle pointing right (+x) - i.e. the arrow-head
                [:path {:d "M0,0 V4 L2,2 Z" :fill "black"}]]]])
       (display/hiccup-html)))

(comment
  (display/hiccup-html
   [:svg (conj {:xmlns "http://www.w3.org/2000/svg"}
               #_{:viewBox "-50 -100 200 200"}
               {:height 200 :width 200})
    dot-x]))
