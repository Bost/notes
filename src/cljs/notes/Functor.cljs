(ns notes.Functor
  (:require
   [notes.views :refer [e] :as collapse]
   [clojure.string :refer [join]]))


;; [e ""]
(def ui
  [collapse/ui
   {:id "ftor"
    :title "Functor"
    :content
    [:div
     [:div "    - preserves structure between 2 categories, i.e. a homomorphism by definition"]
     [:div "    - in programming: total mapping of types; (total = all objs from the source are mapped)"]
     [:div "    - Constant functor: collapses all objs into 1 obj and all morphisms into an identity"]
     [:div "    - Intuitive understanding: (endo) functor is a container - i.e. list contains values"]
     [:div "      (Comonad is a container that already comes prefilled with many values and"]
     [:div "      with an access point to one particular value. E.g. hidden params (for"]
     [:div "      hidden param propagation it's better to use comonad than monad), history,"]
     [:div "      neighbourhood etc.)"]
     [:div "    - function itself can be regarded as a container. E.g. identity function;"]
     [:div "      functions (when evaluated) return value i.e. data are represented as values"]
     [:div "      functions and data are the same"]
     [:div "      function type is an exponential which is a data type; (Exponential is like an interated product)"]
     [:div "    - Endo-Functor [C,C]: A ftor that maps a category to itself"]
     [:div " "]
     [:div
      [e
       (join
        "\n"
        [
         "  | Covariant Functor                       | Contravariant Functor                      |"
         "  |-----------------------------------------+--------------------------------------------|"
         "  | G f :: (a → b) → (G a → G b)            | G f :: (a → b) → (G b → G a)               |"
         "  | Same directions in src and dst Category | Reverse directions in src and dst Category |"])
       (join
        "\n"
        [
         ;; "\\def\\arraystretch{1.2}"
         ;; "\\boxed"
         "\\begin{array}{cc}"
         "| & \\text{Covariant Functor}                       & | & \\text{Contravariant Functor}                      & | \\\\"
         "| & G f :: (a → b) → (G a → G b)                    & | & G f :: (a → b) → (G b → G a)                       & | \\\\"
         "| & \\text{Same directions in src and dst Category} & | & \\text{Reverse directions in src and dst Category} & |"
         "\\end{array}"])]]

     [:div " "]
     [:div "*** Bifunctor: " [e "C × D → E"]]
     [:div "    Product is a bifunctor, i.e. it takes 2 objs and produces 3rd obj, but it"]
     [:div "    also takes two morphisms and produces 3rd morphism which is a product of"]
     [:div "    these two morphisms"]
     [:div "**** List: " [e "List(α) = Nil | Const α (List α)"]]
     [:div "    - most intuitive example of a Ftor"]
     [:div "    - type constructor: takes a type α and creates a list of α"]
     [:div " "]
     [:div "    Sum (+) and Product (*) are algebraic data types (Algebra on Types):"]
     [:div "    " [e "List(α) = Nil | Const α (List α) ~ L(α) = 1 + α * L(α) ⇒ .. ⇒ L(α) = 1 / (1 - α) = 1 + α + α*α + α*α*α + ..."]]
     [:div " "]
     [:div "    Is Product a Ftor?"]
     [:div " "]
     [:div "    Inlining and refactoring are the opposite.** Fibre: a buch of points mapped"]
     [:div "    to the same value; invertibility of a function to a fibre"]
     [:div " "]
     [:div "    Lifting (= applying functor) transforms a function into a corresponding"]
     [:div "    function within another (usually more general) setting"]
     [:div "                Ff"]
     [:div "          Fa -------> Fb"]
     [:div "           ↑           ↑"]
     [:div "           |    f      |"]
     [:div "           a  ------>  b"]
     [:div " "]
     [:div "Hom-Functor: Functor to category of Sets; has a NaT to every other functor; this NaT is not unique but limited"]
     [:div "   Reader functor in Haskell"]
     [:div "Covariant Functor: Hom(A,–) : C → Set;"]
     [:div "    G f :: (a → b) → (G a → G b); Same directions in src and dst Category"]
     [:div "    Hom(A,–) maps each object X in C to the set of morphisms, Hom(A, X)"]
     [:div "    Hom(A,–) maps each morphism f : X → Y to the function"]
     [:div "    Hom(A, f) : Hom(A, X) → Hom(A, Y) given by"]
     [:div " "]
     [:div "Contravariant Functor: Hom(–,B) : C → Set"]
     [:div "    G f :: (a → b) → (G b → G a); Reverse directions in src and dst Category"]
     [:div "    Hom(–,B) maps each object X in C to the set of morphisms, Hom(X, B)"]
     [:div "    Hom(–,B) maps each morphism h : X → Y to the function"]
     [:div "    Hom(h, B) : Hom(Y, B) → Hom(X, B) given by"]
     [:div " "]
     [:div "Representable Functor F: C → Set is naturally isomorphic to HomC(A,-) for some object A of C"]
     [:div "   Represents objs of C as sets and morphisms of C as morphisms between sets."]
     [:div "   i.e. functions \"tabulate\", \"index\" can be created; mapping of function to a data-type"]
     [:div " "]
     [:div "   fix obj A ∈ C there is HomC(A,-): HomC(A, X) → HomC(A, Y) where there is a morphism X → Y"]
     [:div "   e.g.:"]
     [:div "   The forgetful functor Grp  → Set on the category of groups (G, *, e) is represented by (Z, 1)."]
     [:div "   The forgetful functor Ring → Set on the category of rings is represented by (Z[x], x), the polynomial ring in one variable with integer coefficients."]
     [:div "   The forgetful functor Vect → Set on the category of real vector spaces is represented by (R, 1)."]
     [:div "   The forgetful functor Top → Set on the category of topological spaces is represented by any singleton topological space with its unique e"]]}])
