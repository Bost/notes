(ns notes.id27
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id27"
    :title "* Functor Category:"
    :content
    (join
     "\n"
     [
      "** pick two Cats C, D; ftors from C to D form a Functor Category [C,D]:"
      "*** ??? Objs are ftors"
      "*** ??? Arrows are NaT (Natural Transformation)"
      ])}])
