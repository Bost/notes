(ns notes.id5
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id5"
    :title "* Denotational:"
    :content
    [:div
     [:div "  programs can be translated to math - math is a better lang for humans"]
     [:div "  \"Programm has a meaning i.e. it's a piece of math: operation, declaration, definition\""]]}])
