(ns notes.id17
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id17"
    :title "* Sum type (dual to product - coproduct)"
    :content
    (join
     "\n"
     [
      "** intro: A -> either A or B, B -> either A or B"
      "** elim: case e of: left a -> f a, right b -> f b"
      "** in functional programming - tagged unions"
      ])}])
