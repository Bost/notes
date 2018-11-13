(ns notes.id10
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id10"
    :title "Mapping between CT and FP:"
    :content ""}])
