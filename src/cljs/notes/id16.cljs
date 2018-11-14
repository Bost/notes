(ns notes.id16
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id16"
    :title "* Cartesian product (set of pairs): UP (universal construction) - best product triangle: for all other types there's the unique arrow"
    :content
    [:div
     [:div "  projections: f: C -> A, g: C -> B"]
     [:div "- tuple (pair aka record) is better than tripple"]
     [:div "??? loop-over-all-types: for each of all possible types: 38:20"]
     [:div "** intro: A -> B -> (A,B) tupple"]
     [:div "** elim: (A,B) -> A, (A,B) -> B"]
     ]}])
