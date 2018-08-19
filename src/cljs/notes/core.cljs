(ns notes.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.collapsible :as collapse]
   [notes.events :as events]
   [notes.views :as views]
   [notes.config :as config]
   [katex :as k :refer [render renderToString renderMathInElement]]
   [goog.dom :as dom]
   ))

(enable-console-print!)

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn render-math [{:keys [id expr]}]
  (let [elem (.getElementById js/document id)]
    (k/render expr elem)
    (js/renderMathInElement elem)))

;; see LightTable/src/lt/util/dom.cljs
(defn lazy-nl-via-item
  ([nl] (lazy-nl-via-item nl 0))
  ([nl n] (when (< n (. nl -length))
            (lazy-seq
             (cons (. nl (item n))
                   (lazy-nl-via-item nl (inc n)))))))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (let [app-elem (dom/getElement "app")]
    (reagent/render [views/main-panel] app-elem)
    ;; TODO iterate over ::math
    (let [elems (
                 #_identity
                 lazy-nl-via-item (.getElementsByTagName js/document "math"))]
      (js/console.log "(.-length elems)" (.-length elems))
      (doall
       (map-indexed
        (fn [i _]
          #_(js/console.log "i" i)
          (let [eid (.getElementById js/document (str "m" i))]
            #_(js/console.log "i" i "eid" eid)
            (let [text (.-innerHTML eid)]
              (do
                (k/render text eid)
                (js/renderMathInElement eid)))))
        #_elems
        (range (count elems)))))))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (dev-setup)
  (mount-root))
