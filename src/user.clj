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
  (move [this] [this prm] "Default method to move"))

(defrecord Point []
  Move
  (move [this] (move this {}))
  (move [this {:keys [dx dy] :or {dx 0 dy 0}}]
    #_(println "this:" this "dx:" dx "dy:" dy)
    (-> this
        (update :x (fn [x] (+ x dx)))
        (update :y (fn [y] (+ y dy))))))

(defn make-point
  ([] (make-point {}))
  ([prm] (let [defaults {:x 0 :y 0}]
           (Point. nil (merge defaults prm)))))

#_(defmacro make [cfn prm]
  `(let [cfn# ~cfn
         prm# ~prm]
     #_(println "~prm" ~prm "(quote ~prm)" (quote ~prm) "prm#" prm#)
     (println "~cfn" ~cfn "(quote ~cfn)" (quote ~cfn) "cfn#" cfn# "(type ~cfn)" (type ~cfn))
     (let [defaults {:x 0 :y 0}]
       ((.newInstance cfn#) nil (merge defaults prm#)))))

(move (make-point {:x 1 :y 1}))
(move (make-point {:x 1 :y 1}) {:dx 12 :dy 20})

(defrecord QuadraticBezier []
  Move
  (move [this] (move this {}))
  (move [this {:keys [dx dy] :or {dx 0 dy 0} :as prm}]
    (let [{src :moveto curveto :curveto} this]
      (let [{cpb :ctrl-point-beg
             cpe :ctrl-point-end
             dst :dst} curveto]
        (let [move-fn (fn [p] (move (make-point p) prm))]
          (-> this
              (update :moveto move-fn)
              (update :curveto (fn [curveto]
                                 (-> curveto
                                     (update :ctrl-point-beg move-fn)
                                     (update :ctrl-point-end move-fn)
                                     (update :dst move-fn))))))))))
(defn make-curve
  ([] (make-curve {}))
  ([prm] (let [defaults {:moveto (make-point)
                         :curveto {:ctrl-point-beg (make-point)
                                   :ctrl-point-end (make-point)
                                   :dst (make-point)}}]
           (QuadraticBezier. nil (merge defaults prm)))))

(move (make-curve {
                   #_#_:moveto {:x 10 :y 10}
                   :curveto {:ctrl-point-beg {:x 20 :y 20}
                             #_#_:ctrl-point-end {:x 40 :y 20}
                             :dst {:x 50 :y 10}}})
      {:dx 100})

;; (def dot-x (dot x))
;; (def dot-y (dot y))

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
             :curveto {:ctrl-point-beg {:x 20 :y 20}
                       :ctrl-point-end {:x 40 :y 20}
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
  (let [{cpb :ctrl-point-beg
         cpe :ctrl-point-end
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


;; (->> elems
;;      (mapv (fn [e]
;;              (update e :moveto (fn [p] (move {:point p :dx 20 :dy 10})))))
;;      (go))
