(ns notes.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.events :as events]
   [notes.subs :as subs]
   [notes.collapsible :as collapse]
   ))

(defn dispatch-keydown-rules []
  (re-frame/dispatch
   [::rp/set-keydown-rules
    {:event-keys [[[::events/set-re-pressed-example "Hello, world!"]
                   [{:which 72} ;; h
                    {:which 69} ;; e
                    {:which 76} ;; l
                    {:which 76} ;; l
                    {:which 79} ;; o
                    ]]]

     :clear-keys
     [[{:which 27} ;; escape
       ]]}]))

(defn display-re-pressed-example []
  (let [re-pressed-example (re-frame/subscribe [::subs/re-pressed-example])]
    [:div

     [:p
      "Re-pressed is listening for keydown events. However, re-pressed
      won't trigger any events until you set some keydown rules."]

     [:div
      [:button
       {:on-click dispatch-keydown-rules}
       "set keydown rules"]]

     [:p
      [:span
       "After clicking the button, you will have defined a rule that
       will display a message when you type "]
      [:strong [:code "hello"]]
      [:span ". So go ahead, try it out!"]]

     (when-let [rpe @re-pressed-example]
       [:div
        {:style {:padding          "16px"
                 :background-color "lightgrey"
                 :border           "solid 1px grey"
                 :border-radius    "4px"
                 :margin-top       "16px"
                 }}
        rpe])]))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     #_[:h1 "Hello from " @name]
     #_[display-re-pressed-example]
     [
      #_:div
      collapse/ui
      [
       "* Category Theory: abstract algebra of abstract functions: \"The Arrows Count\""
       "
  Category C = (Obj, Hom, ◦, id)
   - Obj: Class of Objects: A, B, C, ... e.g. Types / Propositions / Algebras / Logic Formulas
   - Hom: Morphisms (arrows): f, g, h, ... e.g. Computation / Proofs / ??? / Implication between Log. Formulas
   - ◦: Morphism composition - associative function \"(f ◦ g) ◦ h = f ◦ (g ◦ h)\"
        Hom(A, B) × Hom(B, C) → Hom(A, C): g ◦ f; it's a partialy binary operation on Mor(CAT::)
        (g ◦ f)(x) = g(f(x))
   - id: identity morphism on object A: id(A)

   A collection of arrows and morphism that can be composed if they are adjacent.
   A structure packing structures of the same type (same category) and structure preserving mappings between them.
   Small Category: all objs and morphisms are sets
   Localy Small Category: ∀ A,B: Hom(A, B) is a set

   (TODO arrow weight = price of calculation; preference for compositions)
   \"Up to isomorphis\" - any such things are isomorphic (structurally the same)
   i.e. working via analogy (i.e. X is just a renamed version of Y)

   \"Commuting diagrams / It (i.e. the diagram) commutes\" - no matter which way you go around you get the same thing
"]
      ]
     ]))
