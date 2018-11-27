(ns notes.id23
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id23"
    :title "* Polymorphic function - a function for every single type i.e. multiplication"
    :content "  (Product) of all obj in a category. Also the dual - the Sum"}])
