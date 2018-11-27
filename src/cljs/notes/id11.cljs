(ns notes.id11
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id11"
    :title "* Views -> Change of perspective:"
    :content
    [:div
     [:div "** Set-theoretical: props of sets defined by elems of sets"]
     [:div "** Categorical: Shrink the set to a point \"I can't look at the structure of a set\""]
     [:div "describe different kinds of sets by their interraction with other sets i.e. by arrows"]
     [:div "- tell me who your friends are and I tell you who you are"]]}])
