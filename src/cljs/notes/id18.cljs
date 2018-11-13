(ns notes.id18
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id18"
    :title "* Monoidal Cat: (objs, arrows, prods) looks kinda like multiplication / addition"
    :content
    [:div [:div " "]]}])
