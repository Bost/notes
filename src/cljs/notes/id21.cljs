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
    (join
     "\n"
     [
      "  Obj in a Cat of Types such that: For every A, B there is a set of arrows from"
      "  A to B. This obj is called function-type. It can be defined by an adjuction of"
      "  two endo-ftors"
      "** It's more interesting if F, G are not an inverse of each other"
      "** F left adjoing to G:"
      "*** left side: prepare an argument for some function using functor F"
      "*** right side: modifying the output of some function using functor G"
      " F A === (A, C)   ftor F acts on A and creates a pair type (A, C)"
      " G B === C -> B   ftor G acts on B and creates a function type from C to B"
      ""
      "Currying arrises from n Adjunction:"
      "  (A, C) -> B is isomorphic (i.e. equivalent) to A -> (C -> B)"
      ""
      "If you have a pairing (product) and if you have such an adjunction in your Cat then you are able to define a function type (en exponential)."
      "A Cat with such pairing and adjunction is called cartesian closed (i.e. this Cat has a function type)"
      ""
      "** function intro: lambda"
      "** function elim: eval"
      ])}])
