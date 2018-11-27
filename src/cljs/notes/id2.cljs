(ns notes.id2
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id2"
    :title "* Ultimatelly the human lang to talk about ideas is the lang of math."
    :content
    [:div
     [:div "  Formulas, Multiplication, stupid mistakes in deriving, simplification etc."]
     [:div "  CT looks nicer: no numbers, it's about ideas"]]}])
