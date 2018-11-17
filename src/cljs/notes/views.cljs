(ns notes.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.events :as events]
   [notes.subs :as subs]
   [notes.collapsible :as collapse]
   [notes.n01-Category-Theory :as n01-CAT]
   [notes.id1  :as n1]
   [notes.id2  :as n2]
   [notes.id3  :as n3]
   [notes.id4  :as n4]
   [notes.id5  :as n5]
   [notes.id6  :as n6]
   [notes.id7  :as n7]
   [notes.id8  :as n8]
   [notes.id9  :as n9]
   [notes.id10 :as n10]
   [notes.id11 :as n11]
   [notes.id12 :as n12]
   [notes.id13 :as n13]
   [notes.id14 :as n14]
   [notes.id15 :as n15]
   [notes.id16 :as n16]
   [notes.id17 :as n17]
   [notes.id18 :as n18]
   [notes.id19 :as n19]
   [notes.id20 :as n20]
   [notes.id21 :as n21]
   [notes.id22 :as n22]
   [notes.id23 :as n23]
   [notes.id24 :as n24]
   [notes.id25 :as n25]
   [notes.id26 :as n26]
   [notes.id27 :as n27]
   ))

(defn dispatch-keydown-rules []
  (re-frame/dispatch
   [::rp/set-keydown-rules
    {:event-keys [[[::events/set-re-pressed-example "Hello, world!"]
                   [{:which 72} ;; h
                    {:which 69} ;; e
                    {:which 76} ;; l
                    {:which 76} ;; l
                    {:which 79} ;; o
                    ]]]

     :clear-keys
     [[{:which 27} ;; escape
       ]]}]))

(defn display-re-pressed-example []
  (let [re-pressed-example (re-frame/subscribe [::subs/re-pressed-example])]
    [:div

     [:p
      "Re-pressed is listening for keydown events. However, re-pressed
      won't trigger any events until you set some keydown rules."]

     [:div
      [:button
       {:on-click dispatch-keydown-rules}
       "set keydown rules"]]

     [:p
      [:span
       "After clicking the button, you will have defined a rule that
       will display a message when you type "]
      [:strong [:code "hello"]]
      [:span ". So go ahead, try it out!"]]

     (when-let [rpe @re-pressed-example]
       [:div
        {:style {:padding          "16px"
                 :background-color "lightgrey"
                 :border           "solid 1px grey"
                 :border-radius    "4px"
                 :margin-top       "16px"
                 }}
        rpe])]))

(defn main-panel []
  [:div
   [:button {:on-click (fn []
                         (collapse/doall-render-math)
                         (re-frame/dispatch [:notes/toggle-render-math]))}
    "(doall-render-math)"]
   #_[:div " "]
   ;; [display-re-pressed-example]
   #_[:div
    id1/ui
    ]
   [:div
    ;; n01/ui
    n1/ui
    ;; n021-NATs/ui
    ;; n2/ui
    ;; n3/ui
    ;; n4/ui
    ;; n5/ui
    ;; n6/ui
    ;; n7/ui
    ;; n8/ui
    ;; n9/ui
    ;; n10/ui
    ;; n11/ui
    ;; n12/ui
    ;; n13/ui
    ;; n14/ui
    ;; n15/ui
    ;; n16/ui
    ;; n17/ui
    ;; n18/ui
    ;; n19/ui
    ;; n20/ui
    ;; n21/ui
    ;; n22/ui
    ;; n23/ui
    ;; n24/ui
    ;; n25/ui
    ;; n26/ui
    ;; n27/ui
    ]])
