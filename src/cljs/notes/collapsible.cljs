(ns notes.collapsible
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [utils.core :refer [dbgv dbgi]]))

(rf/reg-event-db
  :notes/toggle-panel
  (fn [db [_ id]]
    (update-in db [:panels id] not)))

(rf/reg-sub
  :notes/panel-state
  (fn [db [_ id]]
    #_(.log js/console "db" (pr-str db))
    (get-in db [:panels id])))

(defn example-component [content]
  (let [s (reagent/atom 0)]
    (js/setInterval #(swap! s inc) 1000)
    (fn []
      [:div
       #_@s
       content])))

(defn panel [id title & children]
  (let [s (reagent/atom {:open false})]
    (fn [id title & children]
      (let [open? @(rf/subscribe [:notes/panel-state id])
            child-height (:child-height @s)]
        [:div
         [:div {:on-click #(rf/dispatch [:notes/toggle-panel id])
                :style {:background-color "#ddd"
                        :padding "0 1em"}}
          [:div {:style {:float "right"}}
           (if open? "-" "+")]
          title]
         [:div {:style  {:overflow "hidden"
                         ;; :transition "max-height 0.8s"
                         :max-height (if open? child-height 0)}}
          [:div {:ref #(when %
                         (swap! s assoc :child-height (.-clientHeight %)))
                 :style {:background-color "#eee"
                         :padding "0 1em"}
                 }
           children]]]))))

(defn ui [[id title content]]
  [:div
   [panel id
    title
    #_"Example component"
    [example-component content]]])
