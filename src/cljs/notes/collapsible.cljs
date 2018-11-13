(ns notes.collapsible
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [katex :as k :refer [render renderMathInElement renderToString]]
            [utils.core :refer [dbgv dbgi]]))

(rf/reg-event-db
  :notes/toggle-panel
  (fn [db [_ id]]
    (update-in db [:open-panels id] not)))

(rf/reg-sub
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

(defn render-math [el]
  (let [text (.-innerText el)
        expr (clojure.string/replace text "◦" "\\circ")
        ]
    ;; (.log js/console expr)
    ;; (.log js/console (k/renderToString expr))
    ;; see k/renderMathInElement
    (k/render expr el)
    #_(set! (.-innerHTML el) (k/renderToString expr))))

(defn doall-render-math []
  (doall
   (let [elems (.getElementsByClassName js/document "m")]
     (.log js/console "on-click render-math in" (count elems) "elems")
     (map render-math elems))))

(defn panel [id title & children]
  (let [s (reagent/atom {:open false})]
    (fn [id title & children]
      (let [open? @(rf/subscribe [:notes/panel-state id])
            child-height (:child-height @s)]
        ;; (.log js/console "open?" open?)
        [:div
         [:div {:on-click (fn [] (rf/dispatch [:notes/toggle-panel id]))
                :style {:background-color "#ddd"
                        :padding "0 1em"}}
          [:div {:style {:float "right"}}
           (if open? "-" "+")]
          title]
         [:div {:style  {:overflow "hidden"
                         ;; :transition "max-height 0.8s"
                         :max-height (if open? child-height 0)}}
          [:div {:ref (fn [e]
                        (when e
                          (swap! s assoc :child-height (.-clientHeight e))))
                 :style {:background-color "#eee"
                         :padding "0 1em"}}
           (let [key-id (gensym)]
             (into [:div {:key key-id}] children))]]]))))

(defn ui [{:keys [id title content]}]
  (.log js/console "id" id)
  [:div
   [panel id
    title
    #_"Example component"
    [component id content]]])
