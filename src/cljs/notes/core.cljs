(ns notes.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.collapsible :as collapse]
   [notes.events :as events]
   [notes.views :as views]
   [notes.config :as config]
   [katex :as k :refer [render renderMathInElement renderToString]]
   [goog.dom :as dom]
   [domina :as domina]
   ;; does spyscope exist for clojurescript?
   #_[spyscope.core]))

(enable-console-print!)

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

;; see LightTable/src/lt/util/dom.cljs
(defn lazy-nl-via-item
  ([nl] (lazy-nl-via-item nl 0))
  ([nl n] (when (< n (. nl -length))
            (lazy-seq
             (cons (. nl (item n))
                   (lazy-nl-via-item nl (inc n)))))))


(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel] (.getElementById js/document "app"))
  #_(collapse/doall-render-math))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (dev-setup)
  (mount-root))
