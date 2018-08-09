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
   [react-katex :as rk :refer [InlineMath BlockMath]]
   [react :as r :refer [createElement]]
   [react-dom :as rd]
   ["react-dom/server" :as rds :refer [renderToString]]
   ))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (let [elem (.getElementById js/document "app")]
    (k/render "c = \\pm\\sqrt{a^2 + b^5}" elem)
    #_(reagent/render [views/main-panel] elem)))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (dev-setup)
  (mount-root))
