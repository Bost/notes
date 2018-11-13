(ns notes.id3
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id3"
    :title "Programming - understanding the meaning i.e. semantics: what does it mean"
    :content
    [:div
     [:div ""]
     ]}])
