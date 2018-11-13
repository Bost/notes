(ns notes.id8
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id8"
    :title "* Types and fns:"
    :content
    (join
     "\n"
     [
      "** types: sets of vals; it's not about \"how\" - fn body, it's about \"what\" - fn declaration; abstraction"
      "   For mathematicians Set Theory is a low level assembly lang of maths - recenty started to be avoided:"
      "*** HoTT"
      "*** CT (Sets form a Category)"
      "** (pure) fns: mappings between sets"
      ])}])
