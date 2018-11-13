(ns notes.id9
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id9"
    :title "* Categorical view (simplification):"
    :content
    (join
     "\n"
     [
      "** fns: arrows between objs"
      "** types: objs whose props are defined by arrows"
      "*** composition(!) \"this-fn after that-fn\", associativity, identity"
      "- no deeper specification of the Fns and Objs are"
      ])}])
