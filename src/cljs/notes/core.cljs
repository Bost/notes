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
   ))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn render-math [{:keys [id expr]}]
  (let [elem (.getElementById js/document id)]
    (k/render expr elem)
    (js/renderMathInElement elem)))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (let [app-elem (.getElementById js/document "app")]
    (reagent/render [views/main-panel] app-elem)
    ;; TODO iterate over ::math
    (let [hm {:id "math"
              :expr "c = \\pm\\sqrt{a^2 + b^2}"}]
      (render-math hm))))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (dev-setup)
  (mount-root))
