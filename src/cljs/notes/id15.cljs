(ns notes.id15
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id15"
    :title "* Unit (one-elem set): univ prop: terminal obj (opposite i.e. dual to init-obj); Duality - invert the arrows and you get something for free"
    :content
    (join
     "\n"
     [
      "** intro: A -> Unit (fn: just ignore the fn input)"
      "** elim: Unit -> A (fn: pick one elem of a type i.e. set; some sort of \"cheating\" - instead of an elem we pick a morphism)"
      ])}])
