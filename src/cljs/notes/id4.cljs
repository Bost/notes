(ns notes.id4
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id4"
    :title "* Operational: \"if state === stateX then state = stateY\":"
    :content
    [:div
     [:div "  for computers: local, progress oriented"]
     [:div "  Mind machine: We keep on imagining the if-then-else steps."]
     [:div "  This is bad way - computers are much better at it."]]}])
