(ns notes.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.events :as events]
   [notes.subs :as subs]
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   [katex :as k :refer [render renderToString renderMathInElement]]
   [notes.id0 :as id0]
   [notes.id1 :as id1]
   [notes.id2 :as id2]
   [notes.id3 :as id3]
   [notes.id4 :as id4]
   [notes.id5 :as id5]
   [notes.id6 :as id6]
   [notes.id7 :as id7]
   [notes.id8 :as id8]
   [notes.id9 :as id9]
   [notes.id10 :as id10]
   [notes.id11 :as id11]
   [notes.id12 :as id12]
   [notes.id13 :as id13]
   [notes.id14 :as id14]
   [notes.id15 :as id15]
   [notes.id16 :as id16]
   [notes.id17 :as id17]
   [notes.id18 :as id18]
   [notes.id19 :as id19]
   [notes.id20 :as id20]
   [notes.id21 :as id21]
   [notes.id22 :as id22]
   [notes.id23 :as id23]
   [notes.id24 :as id24]
   [notes.id25 :as id25]
   [notes.id26 :as id26]
   [notes.id27 :as id27]
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

(defn div-math [{:keys [id expr]}]
  #_(re-frame/dispatch [::events/set-math id] expr)
  [:div {:id id}])

(defn main-panel []
  #_[:div {:id "main-panel"}
   [:div "line1 " [:span {:class "m"} "c = \\pm\\sqrt{a^2 + b^1}"] " after"]
   [:div "line2 " [:span {:class "m"} "c = \\pm\\sqrt{c^2 + d^2}"] " after"]]
  ;; ◦ \\circ

  [:div
   [:button {:on-click #(collapse/doall-render-math)} "(doall-render-math)"]
   [:div " "]
   #_[display-re-pressed-example]
   id0/ui
   id1/ui
   id2/ui
   id3/ui
   id4/ui
   id5/ui
   id6/ui
   id7/ui
   id8/ui
   id9/ui
   id10/ui
   id11/ui
   id12/ui
   id13/ui
   id14/ui
   id15/ui
   id16/ui
   id17/ui
   id18/ui
   id19/ui
   id20/ui
   id21/ui
   id22/ui
   id23/ui
   id24/ui
   id25/ui
   id26/ui
   id27/ui
   ])
