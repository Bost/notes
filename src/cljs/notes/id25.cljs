(ns notes.id25
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]))


(def ui
  [collapse/ui
   {:id "id25"
    :title "* Monadic return-function: universally polymorphic function - works for any type"
    :content
    [:div
     [:div "  https://www.youtube.com/watch?v=CfoaY2Ybf8M&t=7m"]
     [:div ""]
     [:div "** Generalisation of everything. They sub-sume everything else, like adjuctions at a higher level"]
     [:div "** Limits Colimits Monads Adjunctions can be redefined as Khan Extentions"]
     [:div "** Intuition of Khan Extentions is dificult - they are more abstract than monads"]
     [:div "*** Adjunction between a Product and a Function Type is Currying"]]}])
