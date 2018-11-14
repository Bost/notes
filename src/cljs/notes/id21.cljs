(ns notes.id21
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id21"
    :title "* Adjunction: A pair of ftors: one ftor F adjunct to another ftor G; F and U are not an inverse of each other"
    :content
    [:div
     [:div "  Obj in a Cat of Types such that: For every A, B there is a set of arrows from"]
     [:div "  A to B. This obj is called function-type. It can be defined by an adjuction of"]
     [:div "  two endo-ftors"]
     [:div "** It's more interesting if F, G are not an inverse of each other"]
     [:div "** F left adjoing to G:"]
     [:div "*** left side: prepare an argument for some function using functor F"]
     [:div "*** right side: modifying the output of some function using functor G"]
     [:div " F A === (A, C)   ftor F acts on A and creates a pair type (A, C)"]
     [:div " G B === C -> B   ftor G acts on B and creates a function type from C to B"]
     [:div ""]
     [:div "Currying arrises from n Adjunction:"]
     [:div "  (A, C) -> B is isomorphic (i.e. equivalent) to A -> (C -> B)"]
     [:div ""]
     [:div "If you have a pairing (product) and if you have such an adjunction in your Cat then you are able to define a function type (en exponential)."]
     [:div "A Cat with such pairing and adjunction is called cartesian closed (i.e. this Cat has a function type)"]
     [:div ""]
     [:div "** function intro: lambda"]
     [:div "** function elim: eval"]
     ]}])
