(ns notes.id9
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id9"
    :title "* Categorical view (simplification):"
    :content
    [:div
      [:div "** fns: arrows between objs"]
      [:div "** types: objs whose props are defined by arrows"]
      [:div "*** composition(!) \"this-fn after that-fn\", associativity, identity"]
      [:div "- no deeper specification of the Fns and Objs are"]]}])
