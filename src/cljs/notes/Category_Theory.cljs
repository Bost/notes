(ns notes.Category-Theory
  (:require
   [notes.collapsible :refer [e] :as collapse]))


(def ui
  [collapse/ui
   {:id "cat"
    :title "Category Theory"
    :content
    #_[:div
       [:div "line1"]
       [:div "line2"]]
    ;; [e ""]
    [:div
     [:div "Abstract algebra of abstract functions: \"The Arrows Count\""]
     [:div "  Category " [e "C = (Obj, Hom, ◦, id)"]]
     [:div "   - " [e "Obj"] ": Class of Objects: A, B, C, ... e.g. Types / Propositions / Algebras / Logic Formulas"]
     [:div "   - " [e "Hom"] ": Morphisms (arrows): f, g, h, ... e.g. Computation / Proofs / ??? / Implication between Log. Formulas"]
     [:div "   - " [e "◦"] ": Morphism composition - associative function " [e "(f ◦ g) ◦ h = f ◦ (g ◦ h)"]]
     [:div "        " [e "Hom(A, B) × Hom(B, C) → Hom(A, C): g ◦ f"] " ; it's a partialy binary operation on Mor(CAT::)"]
     [:div "        " [e "(g ◦ f)(x) = g(f(x))"]]
     [:div "   - " [e "id"] ": identity morphism on object " [e "A: id(A)"]]
     [:div " "]
     [:div "   A collection of arrows and morphism that can be composed if they are adjacent."]
     [:div "   A structure packing structures of the same type (same category) and structure preserving mappings between them."]
     [:div "   Small Category: all objs and morphisms are sets"]
     [:div "   Localy Small Category: " [e "∀ A,B: Hom(A, B)"] " is a set"]
     [:div " "]
     [:div "   (TODO arrow weight = price of calculation; preference for compositions)"]
     [:div "   \"Up to isomorphis\" - any such things are isomorphic (structurally the same)"]
     [:div "   i.e. working via analogy (i.e. X is just a renamed version of Y)"]
     [:div " "]
     [:div "   \"Commuting diagrams / It (i.e. the diagram) commutes\" - no matter which way you go around you get the same thing"]
     [:div " "]
     [:div "** Cartesian Closed Category CCC: Usefull in programming"]
     [:div "*** cartesian: has product " [e "A × B" ] " (conjunction) for any pair of objs A, B"]
     [:div "*** closed: has exponential " [e "B^A"] " (functions) for any pair of objs A, B"]
     [:div "*** 0-th power of an obj: has terminal obj 1 (for all objs there exists an unique map " [e "A → 1"] ")"]
     [:div "   - multiplying by terminal obj 1 gives back the original obj"]
     [:div "   - dual to terminal obj 1 is the initial obj; Top and Bottom objs"]
     [:div "   i.e. any one-element set (= singleton) is terminal"]
     [:div "   DTTO for poset 1 is such an object that any other obj is below it"]
     [:div " "]
     [:div "   - ? monoindal structure on objs ?"]
     [:div "*** Localy CCC: for every obj X sliced category is a CCC"]
     [:div "** Bi-Cartesian Closed Category BCCC: Algebra of Types can be made here"]
     [:div "*** has coproduct for every pair of objs"]
     [:div "*** has initial obj"]
     [:div " "]
     [:div "** Universal Mapping Property (UMP) - The 'double' triangle of Product"]
     [:div "   Consists of Initial and Terminal mapping (morphism)"]
     [:div " "]
     [:div "   Universal Construction - 3 steps (? the triangle ?):"]
     [:div "   1. Define a pattern:"]
     [:div " "]
     [:div "         Z'  (Z' x A) ---\\"]
     [:div "         ⎢       ⎢        \\"]
     [:div "       h ⎢       ⎢id       \\ g'"]
     [:div "         ⎢       ⎢          \\"]
     [:div "    a=>b v       v           ↘"]
     [:div "         Z   (Z x A) -------> B"]
     [:div "                 A      g"]
     [:div " "]
     [:div "    g' = g ∘ (h x id)"]
     [:div " "]
     [:div "   2. Define ranking between matches"]
     [:div "   3. The best match is \"our guy\""]]}])
