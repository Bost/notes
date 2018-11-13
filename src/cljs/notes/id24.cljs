(ns notes.id24
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id24"
    :title "* Categorical End (i.e. Product) and CoEnd (i.e. CoProduct i.e. Sum)"
    :content "  notation is the integral sign"}])
