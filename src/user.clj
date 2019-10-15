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

(defprotocol Move
  "A simple protocol for moving"
  (move [this prm]
    "Default method to move"))

(defrecord Point [coordinates]
  Move
  (move [this {:keys [dx dy] :or {dx 0 dy 0}}]
    #_(str "this: " this "; keys " (:coordinates this))
    (-> (:coordinates this)
        (update :x (fn [x] (+ x dx)))
        (update :y (fn [y] (+ y dy))))))

(def a (Point. {:x 1 :y 1}))

(move a {:dx 12 :dy 20})

(def x (Point. {:x 0 :y 0}))
(def y (Point. {:x 100 :y 0}))

(defrecord QuadraticBezier [coordinates]
  Move
  (move [this {:keys [dx dy] :or {dx 0 dy 0}}]
    (-> this
        #_(update :x (fn [x] (+ x dx)))
        #_(update :y (fn [y] (+ y dy))))))

(def curve (QuadraticBezier. {:moveto {:x 10 :y 10}
                              :curveto {:control-point-beg {:x 20 :y 20}
                                        :control-point-end {:x 40 :y 20}
                                        :dst {:x 50 :y 10}}}))

(move curve {:dx 12 :dy 20})

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

(defn point-vals [{:keys [x y] :as point}]
  "E.g. {:x 0 :y 1} -> \"0,1\""
  (str x "," y))

(def elems [{:moveto {:x 10 :y 10}
             :curveto {:control-point-beg {:x 20 :y 20}
                       :control-point-end {:x 40 :y 20}
                       :dst {:x 50 :y 10}}}])

(defn path
  [{:keys [moveto curveto] :as prm}]
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
  (let [{cpb :control-point-beg
         cpe :control-point-end
         dst :dst} curveto]
    [:path {:d (str "M" (point-vals moveto)
                    "C" (point-vals cpb) " " (point-vals cpe) " " (point-vals dst))
            :stroke color
            :marker-end "url(#head)"
            ;; :stroke-width "3"
            :fill "transparent"}]))

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


(->> elems
     (mapv (fn [e]
             (update e :moveto (fn [p] (move {:point p :dx 20 :dy 10})))))
     (go))
