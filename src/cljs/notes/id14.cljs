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
    (join
     "\n"
     [
      "  using arrows, i.e. saying something universal; universal property UP"
      "  initial obj: Univ prop: unique(1.) arrow to every(2.) single other obj"
      "  (corresponds to falsehood in logic)"
      "** intro: can't be constructed (can't construct a fn returning an elem of empty set)"
      "   ??? Identity fn on void ???"
      "** elim: Void -> A (arrow from; polymorphic fn - works for any type)"
      ])}])
