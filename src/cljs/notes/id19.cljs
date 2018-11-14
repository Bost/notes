(ns notes.id19
  (:require
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]
   ))

(def ui
  [collapse/ui
   {:id "id19"
    :title "* Functor: structure preserving mapping between Cats (objs to objs, fns to fns):"
    :content
    [:div
     [:div "** i.e. if there's an arrow A -> B, then there must be arrow F(A) -> F(B)"]
     [:div "** may collapse things, preserves unit obj and composition"]
     [:div "** Endofunctor: mapping from the same Cat to the same Cat. Endo ~ inside, \"Endoscopy\""]
     [:div "   Category of Endofunctors: Cat of ftors from C to C [C,C]"]
     ]}])
