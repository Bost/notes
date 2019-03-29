(ns notes.collapsible
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [katex :as k :refer [render renderMathInElement renderToString]]
            #_[clojure.string :as cs]
            [utils.core :refer [dbgv dbgi]]))

(re-frame/reg-event-db
 :notes/toggle-render-math
 (fn [db [_ _]]
   (update-in db [:render-math] not)))

(re-frame/reg-event-db
  :notes/toggle-panel
  (fn [db [_ id]]
    (update-in db [:open-panels id] not)))

(re-frame/reg-sub
 :notes/render-math-state
 (fn [db [_ _]]
   ;; (.log js/console "db" (pr-str db))
   (get-in db [:render-math])))

(re-frame/reg-sub
  :notes/panel-state
  (fn [db [_ id]]
    ;; (.log js/console "db" (pr-str db))
    (get-in db [:open-panels id])))

(defn component [id content]
  #_(.log js/console "content" content)
  (let [s (reagent/atom 0)]
    (js/setInterval #(swap! s inc) 1000)
    (fn []
      [:div #_@s content])))

(defn ex [txt ktx]
  [:span {:class "m" :ktx (k/renderToString ktx) :txt txt} txt])

(def replacements
  [
   ["◦" "\\circ"]
   ["{" "\\{"]
   ["}" "\\}"]
   ["->" "\\rarr"]
   ["<-" "\\larr"]
   ["|" "\\mid"]
   ["•" "\\bullet"]
   ["~" "\\thicksim"]
   ;; this is a hack
   [" α " "~α~"]
   [" α" "~α"]])


(def replace-all
  (fn [exp replacements]
    (loop [rec-exp exp
           rec-replacements replacements
           acc 0]
      (if (or (> acc 100) ;; do not run forever if something's screwed
              (empty? rec-replacements))
        rec-exp
        (let [[src dst] (first rec-replacements)]
          (recur
           (clojure.string/replace rec-exp src dst)
           (rest rec-replacements)
           (inc acc)))))))

(defn e
  ([txt    ] (ex txt (replace-all txt replacements)))
  ([txt ktx] (ex txt ktx)))

(defn render-math [render? el]
  ;; see k/renderMathInElement, (k/render exp3 el)
  ;; (.log js/console (.getAttribute el "txt"))
  ;; (.log js/console (.getAttribute el "ktx"))
  (set! (.-innerHTML el)
        (if render?
          (.getAttribute el "ktx")
          (.getAttribute el "txt"))))

(defn doall-render-math []
  (let [render-math? @(re-frame/subscribe [:notes/render-math-state])]
    #_(.log js/console "render-math?" render-math?)
    (doall
     (let [elems (.getElementsByClassName js/document "m")]
       #_(.log js/console "render-math in" (count elems) "elems")
       (map #(render-math render-math? %) elems)))))

(defn panel [id title & children]
  (let [s (reagent/atom {:open false})]
    (fn [id title & children]
      (let [open? @(re-frame/subscribe [:notes/panel-state id])]
        ;; (.log js/console "open?" open?)
        [:details [:summary title] children]))))

(defn ui [{:keys [id title content]}]
  #_(.log js/console "id" id)
  [:div
   [panel id
    title
    #_"Example component"
    [component id content]]])
