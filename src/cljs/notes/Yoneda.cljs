(ns notes.Yoneda
  (:require
   [notes.collapsible :refer [e] :as collapse]
   [clojure.string :refer [join]]
   ))

;; [e ""]
(def ui
  [collapse/ui
   {:id "yoneda"
    :title "Yoneda Lemma and Yoneda Embeding"
    :content
    [:div
     [:div "*** Yoneda Lemma: " [e "[C,Set](C(a,-), F) ⋍ Fa"] " also: " [e "[C,Set](C(a,-), C(b,-)) ⋍ C(b,a)"]]
     [:div "    - Intuition: NaT and Functor (i.e. Container) can replace each other"]
     [:div "    - Description of integration over a special Ftor (i.e. Hom Functor)"]
     [:div " "]
     [:div "    a - some arbitrary Obj of C"]
     [:div "    F - some arbitrary Ftor acting on the Obj a"]
     [:div "    ⋍ - \"naturally isomorphic\" (i.e. a NaT exists such that its components are"]
     [:div "        all invertible isomorphisms)"]
     [:div " "]
     [:div "    Hom functors - Intuition:"]
     [:div "    - Play some special role in the Category of Ftors"]
     [:div "    - Serve for the same purposes as Free Monoids"]
     [:div " "]
     [:div "    It's enough to define this NaT on one Obj (i.e. set C(a,a)) and moreover"]
     [:div "    it's enough to define it on one Point in this Set i.e. the Identity on Obj a."]
     [:div "    The rest of the NaT is transported from this Point."]
     [:div " "]
     [:div "    " [e
                   (join
                    "\n"
                    [
                        "(                  ) ⋍ Fa"
                     "    ---------+----------   ⎜"
                     "             |             ⎜"
                     "             |             +-- Container of the Obj a (i.e. the data structure)"
                     "             +---------------- Polymorphic higher order Function"
                     "    (∀ x : (a → x) → Fx) ⋍ Fa"
                     "            --+--     ⎜    ⎜"
                     "           --------+----   |"
                     "              ⎜    ⎜  ⎜    +-- Container of the Obj a (i.e data structure)"
                     "              ⎜    ⎜  +------- Functor"
                     "              ⎜    +---------- NaT i.e. Polymorphic Higher Order Function"
                     "              +--------------- ..."
                     ])
                   "    (∀ x : (a → x) → Fx) ⋍ Fa"
                   ]]
     [:div " "]
     [:div "*** Yoneda Embedding https://youtu.be/JH_Ou17_zyU?t=1h8m9s"]
     [:div "    Idaa: Replace a content of an Obj a (picked i.e. fixed) by a totality of Arrows ending in this Obj. It's content and props."]
     [:div "    Set of Arrow from every possible Obj x to the Obj a"]
     [:div " "]
     [:div "    Mapping from Obj x to the Set of Arrows x → a:"]
     [:div "    1. for every Obj a I get a different Ftor from C to Set,"]
     [:div "    2. then vary the Obj a:"]
     ]}])
