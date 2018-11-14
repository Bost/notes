(ns notes.id14
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id14"
    :title "* Void (empty set): we don't know that it has no elems; describe/define the props"
    :content
    [:div
     [:div "  using arrows, i.e. saying something universal; universal property UP"]
     [:div "  initial obj: Univ prop: unique(1.) arrow to every(2.) single other obj"]
     [:div "  (corresponds to falsehood in logic)"]
     [:div "** intro: can't be constructed (can't construct a fn returning an elem of empty set)"]
     [:div "   ??? Identity fn on void ???"]
     [:div "** elim: Void -> A (arrow from; polymorphic fn - works for any type)"]
     ]}])
