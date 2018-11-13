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
    (join
     "\n"
     [
      "  projections: f: C -> A, g: C -> B"
      "- tuple (pair aka record) is better than tripple"
      "??? loop-over-all-types: for each of all possible types: 38:20"
      "** intro: A -> B -> (A,B) tupple"
      "** elim: (A,B) -> A, (A,B) -> B"
      ])}])
