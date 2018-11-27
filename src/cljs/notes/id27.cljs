(ns notes.id27
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id27"
    :title "* Functor Category:"
    :content
    [:div
     [:div "** pick two Cats C, D; ftors from C to D form a Functor Category [C,D]:"]
     [:div "*** ??? Objs are ftors"]
     [:div "*** ??? Arrows are NaT (Natural Transformation)"]]}])
