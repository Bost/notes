(ns notes.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [notes.events :as events]
   [notes.subs :as subs]
   [notes.collapsible :as collapse]
   [clojure.string :refer [join]]

   [katex :as k :refer [render renderToString renderMathInElement]]
   [react-katex :as rk :refer [InlineMath BlockMath]]
   [react :as r :refer [createElement]]
   [react-dom :as rd]
   ["react-dom/server" :as rds :refer [renderToString]]
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
  #_(js/console.log (rds/renderToString (r/createElement "div" nil "c =
                                                       \\pm\\sqrt{a^2 + b^1}"
                                                       #_"Hello World!")))

  [:div {:id "main-panel"}
   "main-panel"
   [:div {:id "math"} "math"]]

  #_[:div
   #_[display-re-pressed-example]
   [collapse/ui
    [:id0
     "* Category Theory: abstract algebra of abstract functions: \"The Arrows Count\""
     (join
      "\n"
      [
       "  Category C = (Obj, Hom, ◦, id)"
       "   - Obj: Class of Objects: A, B, C, ... e.g. Types / Propositions / Algebras / Logic Formulas"
       "   - Hom: Morphisms (arrows): f, g, h, ... e.g. Computation / Proofs / ??? / Implication between Log. Formulas"
       "   - ◦: Morphism composition - associative function \"(f ◦ g) ◦ h = f ◦ (g ◦ h)\""
       "        Hom(A, B) × Hom(B, C) → Hom(A, C): g ◦ f; it's a partialy binary operation on Mor(CAT::)"
       "        (g ◦ f)(x) = g(f(x))"
       "   - id: identity morphism on object A: id(A)"
       ""
       "   A collection of arrows and morphism that can be composed if they are adjacent."
       "   A structure packing structures of the same type (same category) and structure preserving mappings between them."
       "   Small Category: all objs and morphisms are sets"
       "   Localy Small Category: ∀ A,B: Hom(A, B) is a set"
       ""
       "   (TODO arrow weight = price of calculation; preference for compositions)"
       "   \"Up to isomorphis\" - any such things are isomorphic (structurally the same)"
       "   i.e. working via analogy (i.e. X is just a renamed version of Y)"
       ""
       "   \"Commuting diagrams / It (i.e. the diagram) commutes\" - no matter which way you go around you get the same thing"
       ""
       "** Cartesian Closed Category CCC: Usefull in programming"
       "*** cartesian: has product A x B (conjunction) for any pair of objs A, B"
       "*** closed: has exponential B^A (functions) for any pair of objs A, B"
       "*** 0-th power of an obj: has terminal obj 1 (for all objs there exists an unique map A → 1)"
       "   - multiplying by terminal obj 1 gives back the original obj"
       "   - dual to terminal obj 1 is the initial obj; Top and Bottom objs"
       "   i.e. any one-element set (= singleton) is terminal"
       "   DTTO for poset 1 is such an object that any other obj is below it"
       ""
       "   - ? monoindal structure on objs ?"
       "*** Localy CCC: for every obj X sliced category is a CCC"
       "** Bi-Cartesian Closed Category BCCC: Algebra of Types can be made here"
       "*** has coproduct for every pair of objs"
       "*** has initial obj"
       ""
       "** Universal Mapping Property (UMP) - The 'double' triangle of Product"
       "   Consists of Initial and Terminal mapping (morphism)"
       ""
       "   Universal Construction - 3 steps (? the triangle ?):"
       "   1. Define a pattern:"
       ""
       "         Z'  (Z' x A) ---\\"
       "         ⎢       ⎢        \\"
       "       h ⎢       ⎢id       \\ g'"
       "         ⎢       ⎢          \\"
       "    a=>b ↓       ↓           ↘"
       "         Z   (Z x A) -------> B"
       "                 A      g"
       ""
       "    g' = g ∘ (h x id)"
       ""
       "   2. Define ranking between matches"
       "   3. The best match is \"our guy\""
       ])]]

   [collapse/ui
    [:id1
     "* A Crash Course in Category Theory - Bartosz Milewski https://www.youtube.com/watch?v=JH_Ou17_zyU"
     (join
      "\n"
      [
       "  https://twitter.com/@bartoszmilewski : Computer Science, Haskell, C++. PhD in Quantum Physics"
       ""
       "  Most important features of a category: ?, Composability, Identity"
       "  Example in programming: Category of Types and Functions (set and functions between sets)"
       "*** Designing computer language:"
       "    Semantics must be provided; done by providing operational semantics"
       "    None of  the main prog. languages have (operational semantics) only partially provided;"
       "    Two ways of defining semantics:"
       "    - Operational: \"How it executes\"; reduction relation: e1 -> e2"
       "    - Denotational: mapping into mathematics; interpretation of terms: ⟦e⟧ = ?;"
       "      e.g.: ⟦ v : τ  ⊢  v : τ ⟧ = idτ - i.e. the meaning of is ⟦...⟧ is an identity on τ i.e. an access to variable v"
       ""
       "*** Cartesian Product CP: set of all pairs"
       "    Relation: A subset of CP; doesn't have a directionality; n-to-n relation"
       "*** Functions: (Sets and functions between sets)"
       "    Pure: must be memoizable (lookup table)"
       "    Total: defined for all arguments"
       "    Partial: defined only for some arguments"
       "    Directionality (arrow from->to i.e. functions are not symetric); n-to-1 relation"
       "    - Inverse of function is not guaranteed to exist"
       ""
       "    Domain, Codomain, Image"
       "*** Morphisms:"
       "    | Latin      | Greek                | Meaning                    | Functor  |"
       "    |------------+----------------------+----------------------------+----------|"
       "    | injective  | monic / monomorphism | distinct Xs -> distinct Ys | Faithful |"
       "    | surjective | epic / epimorphism   | all Ys are used            | Full     |"
       ""
       "*** Functor:"
       "    - preserves structure between 2 categories, i.e. a homomorphism by definition"
       "    - in programming: total mapping of types; (total = all objs from the source are mapped)"
       "    - Constant functor: collapses all objs into 1 obj and all morphisms into an identity"
       "    - Intuitive understanding: (endo) functor is a container - i.e. list contains values"
       "      (Comonad is a container that already comes prefilled with many values and"
       "      with an access point to one particular value. E.g. hidden params (for"
       "      hidden param propagation it's better to use comonad than monad), history,"
       "      neighbourhood etc.)"
       "    - function itself can be regarded as a container. E.g. identity function;"
       "      functions (when evaluated) return value i.e. data are represented as values"
       "      functions and data are the same"
       "      function type is an exponential which is a data type; (Exponential is like an interated product)"
       "    - Endofunctor [C,C]: A ftor that maps a category to itself"
       ""
       "  | Covariant Functor                       | Contravariant Functor                      |"
       "  |-----------------------------------------+--------------------------------------------|"
       "  | G f :: (a -> b) -> (G a -> G b)         | G f :: (a -> b) -> (G b -> G a)            |"
       "  | Same directions in src and dst Category | Reverse directions in src and dst Category |"
       ""
       "*** Bifunctor: C × D → E"
       "    Product is a bifunctor, i.e. it takes 2 objs and produces 3rd obj, but it"
       "    also takes two morphisms and produces 3rd morphism which is a product of"
       "    these two morphisms"
       "**** List: List(α) = Nil | Const α (List α)"
       "    - most intuitive example of a Ftor"
       "    - type constructor: takes a type α and creates a list of α"
       ""
       "    Sum (+) and Product (*) are algebraic data types (Algebra on Types):"
       "    List(α) = Nil | Const α (List α) ~ L(α) = 1 + α * L(α) => .. => L(α) = 1 / (1 - α) ="
       "    = 1 + α + α*α + α*α*α + ..."
       ""
       "    Is Product a Ftor?"
       ""
       "    Inlining and refactoring are the opposite.** Fibre: a buch of points mapped"
       "    to the same value; invertibility of a function to a fibre"
       ""
       "    Lifting (= applying functor):"
       "                F f"
       "          F a ------> F b"
       "           ↑           ↑"
       "           |     f     |"
       "           a --------> b"
       ""
       "*** Natural Transformation: a way of/for comparing functors"
       "   - maps Morphism(s) to commuting diagram(s) (naturality squares). i.e."
       "     comorphism: replacing a square of (complex) relations with a single morphism"
       "   - picks a morphish between two Objs; Picking 1 morphishm from a homset"
       "   - Components of NaT"
       "   - Composing Ftor acting on an Obj with a Ftor acting on a Morphishm: α b * F f"
       ""
       "   - Every polymorphic Fn is a NaT: it is defined for every single type"
       "     i.e. multiplication (Product) of all Objs in a Category. The same goes for"
       "     the dual - the Sum."
       "   - Functor is a container, NaT repackages the container"
       ""
       "**** Compositons of Natural Transformations"
       "   See https://math.vanderbilt.edu/dept/conf/tacl2013/coursematerials/SelingerTACL20132.pdf"
       "   E.g. NaT compositons is scala: https://gist.github.com/Mzk-Levi/752d1e0f2f7f30cd3bda"
       "   Legend:"
       "       A - an Obj in the Category C"
       "       (...)A / [...]A - an A-component of the NaT (...) / [...]"
       "       αA / βA - an A-component of the NaT α / β"
       "       αFA - an FA-component of the NaT α"
       "       βGA / βFA- GA/FA-component of the NaT β"
       ""
       "***** Verical Compositon of NaTs:"
       "     If α : F → G and β : G → H are natural transformations, then so is β • α : F → H."
       "     Is it defined by:"
       "         (β • α)A = βA ◦ αA : FA → HA"
       "     A - an Obj in the Category C"
       "     (...)A - an A-component of the NaT (...)"
       ""
       "     - is associative and has an id, and allows one to consider the collection"
       "       of all functors C → D itself as a category."
       ""
       "***** Right Whiskering"
       "     If F, G : C → D and H : D → E are Ftors, and if α : F → G is a NaT, the right whiskering"
       "         H ◦ α : H ◦ F → H ◦ G"
       "     is defined as (H ◦ α)A : H(FA) → H(GA) by (H ◦ α)A = H(αA)"
       ""
       "***** Left Whiskering"
       "     If F : C → D and G, H : D → E are Ftors, and if α : G → H is a NaT, the left whiskering"
       "         α ◦ F : G ◦ F → H ◦ F"
       "     is defined as (α ◦ F)A : G(FA) → H(FA) by (α ◦ F)A = αFA"
       ""
       "***** Horizontal Compositon of NaTs:"
       "     If F, G : C → D and H, K : D → E are Ftors, and if α : F → G and β : H → K"
       "     are NaTs, the horizontal composition:"
       "         β ◦ α : H ◦ F → K ◦ G"
       "     can be defined in two different ways:"
       "     - Right whiskering followed by left whiskering:"
       "         β ◦ α = (β ◦ G) • (H ◦ α)"
       "     - Left whiskering followed by right whiskering:"
       "         β ◦ α = (K ◦ α) • (β ◦ F)"
       ""
       "     The two definitions coincide, because"
       "         [(β ◦ G) • (H ◦ α)]A = βGA ◦ H(α A), and"
       "         [(K ◦ α) • (β ◦ F)]A = K(α A) ◦ βFA"
       ""
       "     - is associative with an id, and the id coincides with that for vertical"
       "       composition."
       ""
       ""
       "*** Yoneda Lemma: [C,Set](C(a,-), F) ⋍ F a   also: [C,Set](C(a,-), C(b,-)) ⋍ C(b,a)"
       "    - Intuition: NaT and Functor (i.e. Container) can replace each other"
       "    - Description of integration over a special Ftor (i.e. Hom Functor)"
       ""
       "    a - some arbitrary Obj of C"
       "    F - some arbitrary Ftor acting on the Obj a"
       "    ⋍ - \"naturally isomorphic\" (i.e. a NaT exists such that its components are"
       "        all invertible isomorphisms)"
       ""
       "    Hom functors - Intuition:"
       "    - Play some special role in the Category of Ftors"
       "    - Serve for the same purposes as Free Monoids"
       ""
       "    It's enough to define this NaT on one Obj (i.e. set C(a,a)) and moreover"
       "    it's enough to define it on one Point in this Set i.e. the Identity on Obj a."
       "    The rest of the NaT is transported from this Point."
       ""
       "    (                     ) ⋍ F a"
       "              ⎜                ⎜"
       "              ⎜                +-- Container of the Obj a (i.e. the data structure)"
       "              +------------------- Polymorphic higher order Function"
       ""
       "    (∀ x : (a -> x) -> F x) ⋍ F a"
       "              ⎜     ⎜   ⎜      ⎜"
       "              ⎜     ⎜   ⎜      +-- Container of the Obj a (i.e data structure)"
       "              ⎜     ⎜   +--------- Functor"
       "              ⎜     +------------- NaT i.e. Polymorphic Higher Order Function"
       "              +------------------- ..."
       ""
       ""
       "*** Yoneda Embedding https://youtu.be/JH_Ou17_zyU?t=1h8m9s"
       "    Ideaa: Replace a content of an Obj a (picked i.e. fixed) by a totality of Arrows ending in this Obj. It's content and props."
       "    Set of Arrow from every possible Obj x to the Obj a"
       ""
       "    Mapping from Obj x to the Set of Arrows x->a:"
       "    1. for every Obj a I get a different Ftor from C to Set,"
       "    2. then vary the Obj a:"
       ""
       "*** Khan Extentions: the next abstraction level"
       ""
       "*** Adjunctions: weakening of \"equality\" of Categories"
       "    \"inverse\" is defined only for functions not functors"
       "**** e.g. Currying: from a Pair to Function type"
       "*** Adjointness - constructing / generating principle"
       "    - Adjunctions/Adjoins are monads ???"
       "    Adjoin examples:"
       "    (-) x A (product) ⊣ (-)^A (exponential)"
       "    '+' (coproduct) ⊣ '∆' (pairing) ⊣ 'x' (product)"
       "    induction, recursion, Natural Numbers (inductively defined), Lists, ..."
       "    conjunction, disjunction, True, False, Exponentiation"
       "    Quantifiers: ∀ Every, ∃ Exists; Σ Sigma, Π Pi"
       ""
       "*** Fibre: a buch of points mapped to the same value; invertibility of a function to a fibre"
       "*** Abstraction: i.e. non-invertibility"
       "   - from all properties (i.e. all points of a fibre) I'm interested only in one"
       "   - e.g. I'm not interested in what was the exact input value of a function,"
       "     I'm interested only if it was an even or odd value"
       "*** Modeling: mapping / injecting"
       "*** HomSet: HomC(A,B) = {f: A → B} - set of all morphisms A → B in category C (Objs of C don't need to be sets)"
       "   External vs. Internal Homset"
       ""
       "*** Free Monoid: has an unique mapping to every other monoid"
       "*** HomFunctor: Functor to category of Sets; has a NaT to every other functor; this NaT is not unique but limited"
       "   Reader functor in Haskell"
       "*** Covariant functor: Hom(A,–) : C → Set;"
       "    G f :: (a -> b) -> (G a -> G b); Same directions in src and dst Category"
       "    Hom(A,–) maps each object X in C to the set of morphisms, Hom(A, X)"
       "    Hom(A,–) maps each morphism f : X → Y to the function"
       "    Hom(A, f) : Hom(A, X) → Hom(A, Y) given by"
       ""
       "*** Contravariant functor: Hom(–,B) : C → Set"
       "    G f :: (a -> b) -> (G b -> G a); Reverse directions in src and dst Category"
       "    Hom(–,B) maps each object X in C to the set of morphisms, Hom(X, B)"
       "    Hom(–,B) maps each morphism h : X → Y to the function"
       "    Hom(h, B) : Hom(Y, B) → Hom(X, B) given by"
       ""
       "*** Representable Functor F: C → Set"
       "   Represents objs of C as sets and functions of C as morphisms between sets."
       "   i.e. functions \"tabulate\", \"index\" can be created; mapping of function to a data-type"
       ""
       "   fix obj A ∈ C there is HomC(A,_): HomC(A, X) → HomC(A, Y) where there is a morphism X → Y"
       "   e.g.:"
       "   The forgetful functor Grp → Set on the category of groups (G, *, e) is represented by (Z, 1)."
       "   The forgetful functor Ring → Set on the category of rings is represented by (Z[x], x), the polynomial ring in one variable with integer coefficients."
       "   The forgetful functor Vect → Set on the category of real vector spaces is represented by (R, 1)."
       "   The forgetful functor Top → Set on the category of topological spaces is represented by any singleton topological space with its unique e"
       "*** Naturality condition: Gf ∘ αa = αb ∘ Ff i.e. the Naturality Square"
       "*** Homomorphism: structure-preserving mapping between 2 algebraic structures (e.g. monoids, groups, rings, vector spaces)."
       "    f(m * n) = f(m) * f(n)"
       ""
       "    Individual monoids themselves give category"
       "    Monoids with homomorphisms give category"
       ""
       "*** Kleisli category:"
       "   Monad: return: a -> m a; bind: m a -> (a -> m b) -> m b"
       "     You can operate on IO Monad"
       "     You can't extract anything from IO Monad (it's lost)"
       "     Monoind in Category of Endofunctors"
       "   Comonad: (w a -> b) -> (w b -> c) -> (w a -> c)"
       "     You can extract from IO Monad"
       "     You can't put anything to IO Monad"
       "*** Topos: a type of a Category being able to be a replacement for Set Theory; provides among other things a notion of a Subse"
       "*** TODO:"
       "   - Subobject Classifier etc.: see Bartosz's blog"
       "   - Sheaf (Garbe, Faisceau, zvazok)- tool for tracking locally defined data"
       "   - Presheaf: Functor F: Cop → Set"
       ""
       "*** Indexed Monad: IxMonad: ibind: m i j a -> (a -> m j k b) -> m i k b"
       "   state composition"
       "   Session Types, Dependent Types, Dependent State Types"
       "*** Curry-Howard-Lambek correspondence: Intuitionistic Logic <-> Type Theory <-> Category Theory:"
       "  Function A -> B is a proof of logical implication A => B"
       "  Direct relationship between computer programs and mathematical proofs; from 1940-ties"
       "  Link between Computation and Logic;"
       "  Proofs-as-programs and propositions- or formulae-as-types interpretation;"
       "  Proofs (= Programs) can be executed;"
       "  Typed lambda calculi derived from the Curry–Howard-Lambek paradigm led to software like Coq;"
       "  Curry-Howard-Lambek correspondence might lead to unification between mathematical logic and foundational computer science;"
       "  Popular approach: use monads to segregate provably terminating from potentially non-terminating code"
       ""
       "    | INTUITIONISTIC (Constructive) LOGIC (Howard)   | TYPE THEORY - Functional Programming (Curry)                         | CATEGORY THEORY (Lambek) |"
       "    |------------------------------------------------+----------------------------------------------------------------------+--------------------------|"
       "    | Proposition of some type - (something is true) | Type (contract - a set of values that passes the contract)           |                          |"
       "    | Proof of some type                             | Term (A program - guarded fn)                                        |                          |"
       "    | Normalisation (Proof equality)                 | Computation (substitute variable with value)                         |                          |"
       "    |------------------------------------------------+----------------------------------------------------------------------+--------------------------|"
       "    | P implies Q: P -> Q (i.e. there exists one)    | paricular fn of fn of P-contract to guarded fn of Q-contract: P -> Q |                          |"
       "    | -> is constructive implication                 | -> is function from-to                                               |                          |"
       "    | false      -> false (implies)                  | {}       ->  {}  no values (empty set); contract cannot be satisfied |                          |"
       "    | false      -> true                             | {}       ->  {.} (one element set)                                   |                          |"
       "    | true       -> true                             | {.}      ->  {.} (identity function)                                 |                          |"
       "    | true  (not ->) false (does not imply)          | {.} (not ->) {}                                                      |                          |"
       ""
       ""
       "** Correspondance of type habitation and proposition"
       "   inhabited - has elems / members"
       "   \"Either a b\" is inhabited if either a or b is inhabited (at least one of them is true / provable)"
       ""
       "   Curry: ((a,b) -> c) -> (a -> (b -> c))"
       "   Uncurry: (a -> (b -> c)) -> ((a,b) -> c)"
       ""
       "   Eval: a function of two args / a pair"
       "   \"((a => b), a) -> b\" this is modus-ponens in logic \"a => b ∧ a -> b\""
       ""
       "    | True proposition | False proposition | Conjunction a ∧ b         | Disjunction a ∨ b           | Implication a => b   |"
       "    | Unit-type        | Void-type         | Pair (a,b)                | Either a b                  | Function type a -> b |"
       "    | sinhabited       | not inhabited     |                           |                             |                      |"
       "    | Terminal obj     | Initial obj       | Categorical product a × b | Categorical coproduct a ⎥ b | Exponential obj b^a  |"
       ""
       ""
       "    0 - void type - ?"
       "    1 - unit type - 0th-power: terminal obj"
       "    2 - bool type (two possible values): 1st-power: the obj itself"
       "    3 - int type - 2nd-power: product"
       "    4 - real type (if continuum hypothesis holds :-)"
       "    5 - ? type"
       ""
       "    JavaScript & Category Theory: Category == Contracts + Functions guarded by contracts"
       ""
       "** Set vs. Category theory comparision:"
       "   | Set theory                  | Category theory                                          | JavaScript                     |"
       "   |-----------------------------+----------------------------------------------------------+--------------------------------|"
       "   | membership relation         | -                                                        |                                |"
       "   | elements                    | objects                                                  | contracts                      |"
       "   | sets                        | categories                                               |                                |"
       "   | -                           | morphisms (structure-preserving mapping between objects) | functions guarded by contracts |"
       "   | functions                   | functors  (maps between categories)                      |                                |"
       "   | equations between elements  | isomorphisms between objects                             |                                |"
       "   | equations between sets      | equivalences between categories                          |                                |"
       "   | equations between functions | natural transformations (maps between functors)          |                                |"
       ""
       "   Categorification: process of weakening structure, weakening equalities down to natural isomorphisms and then adding-in rules"
       "   that these natural isomorphisms have to follow (so it behaves well)"
       "   Counting number of elements in sets is decategorification; from category we get set or from set we get a number"
       ""
       "   Monoid homomorphisms: a function between the sets of monoid elements that preserved the monoid structure"
       "   Monoidal functors:    a functor between categories that preserves the monoidal structure (should preserve multiplication)"
       "   from functor(prodn([x, y, ..])) to prodn([functor(x), functor(y), ..])"
       "   Monoidal monad:       ???"
       ""
       "   Functor:"
       "   \"forget the indexing (domain functor)\""
       ""
       "*** Contract = Object"
       "*** Product: examples:"
       "    Objects   - numbers"
       "    Morphisms - functions 'less/greater or equal than'"
       ""
       "*** Isomorphism (bijection when f is a function on set / sets):"
       "  ∀ f: X → Y  there ∃ g: Y → X such that g ∘ f = idX and f ∘ g = idY; idX, idY are identity morphisms on X, Y"
       "  (f is invertible and g is the inverse of f)"
       ""
       "** Category theory - Modeling (new vocabulary)"
       "   | hierarchies                | partial orders     |"
       "   | symmetries                 | group elements ?   |"
       "   | data models                | categories         |"
       "   | agent actions              | monoid actions     |"
       "   | local-to-global principles | sheaves (lanovica) |"
       "   | self-similarity            | operads            |"
       "   | context                    | monads             |"
       ""
       ""
       "** olog = ontology log"
       "   Different branches of mathematics can be formalized"
       "   into categories. These categories can then be connected together by functors. And the"
       "   sense in which these functors provide powerful communication of ideas is that facts and"
       "   theorems proven in one category can be transferred through a connecting functor to"
       "   yield proofs of an analogous theorem in another category. A functor is like a conductor"
       "   of mathematical truth."
       ""
       "*** Mappings: X → Y (Zobrazenia):"
       "    Surjection: all Ys are used;                                     |X| ≥ |Y| (onto; \"at least as big\")"
       "    Injective:  distinct Xs -> distinct Ys;                          |X| ≤ |Y| (? one-to-one ?)"
       "    Bijection:  exact pairing between X, Y;                          |X| = |Y| (vzajomne jednoznacne zobrazenie, \"same size\")"
       "    Strict:     Surjection from X to Y but no bijection from Y to X; |X| < |Y| (? double usage of some Ys ?, \"strictly bigger\")"
       ])]]

   [collapse/ui
    [:id2
     "* Ultimatelly the human lang to talk about ideas is the lang of math."
     (join
      "\n"
      [
       "  Formulas, Multiplication, stupid mistakes in deriving, simplification etc."
       "  CT looks nicer: no numbers, it's about ideas"
       ])]]

   [collapse/ui
    [:id3
     "Programming - understanding the meaning i.e. semantics: what does it mean (+ 1 2)"
     ""]]


   [collapse/ui
    [:id4
     "* Operational: \"if state === stateX then state = stateY\":"
     (join
      "\n"
      [
       "  for computers: local, progress oriented"
       "  Mind machine: We keep on imagining the if-then-else steps."
       "  This is bad way - computers are much better at it."
       ])]]


   [collapse/ui
    [:id5
     "* Denotational:"
     (join
      "\n"
      [
       "  programs can be translated to math - math is a better lang for humans"
       "  \"Programm has a meaning i.e. it's a piece of math: operation, declaration, definition\""
       ])]]

   [collapse/ui
    [:id6
     "* Math: for humans by humans"
     ""]]

   [collapse/ui
    [:id7
     "Functional Programming - mathematical semantics:"
     ""]]


   [collapse/ui
    [:id8
     "* Types and fns:"
     (join
      "\n"
      [
       "** types: sets of vals; it's not about \"how\" - fn body, it's about \"what\" - fn declaration; abstraction"
       "   For mathematicians Set Theory is a low level assembly lang of maths - recenty started to be avoided:"
       "*** HoTT"
       "*** CT (Sets form a Category)"
       "** (pure) fns: mappings between sets"
       ])]]


   [collapse/ui
    [:id9
     "* Categorical view (simplification):"
     (join
      "\n"
      [
       "** fns: arrows between objs"
       "** types: objs whose props are defined by arrows"
       "*** composition(!) \"this-fn after that-fn\", associativity, identity"
       "- no deeper specification of the Fns and Objs are"
       ])]]

   [collapse/ui
    [:id10
     "Mapping between CT and FP:"
     ""]]


   [collapse/ui
    [:id11
     "* Views -> Change of perspective:"
     (join
      "\n"
      [
       "** Set-theoretical: props of sets defined by elems of sets"
       "** Categorical: Shrink the set to a point \"I can't look at the structure of a set\""
       "describe different kinds of sets by their interraction with other sets i.e. by arrows"
       "- tell me who your friends are and I tell you who you are"
       ])]]

   [collapse/ui
    [:id12
     "Phenomenons of Introduction and Elimination"
     ""]]


   [collapse/ui
    [:id13
     "Data types:"
     ""]]


   [collapse/ui
    [:id14
     "* Void (empty set): we don't know that it has no elems; describe/define the props"
     (join
      "\n"
      [
       "  using arrows, i.e. saying something universal; universal property UP"
       "  initial obj: Univ prop: unique(1.) arrow to every(2.) single other obj"
       "  (corresponds to falsehood in logic)"
       "** intro: can't be constructed (can't construct a fn returning an elem of empty set)"
       "   ??? Identity fn on void ???"
       "** elim: Void -> A (arrow from; polymorphic fn - works for any type)"
       ])]]


   [collapse/ui
    [:id15
     "* Unit (one-elem set): univ prop: terminal obj (opposite i.e. dual to init-obj); Duality - invert the arrows and you get something for free"
     (join
      "\n"
      [
       "** intro: A -> Unit (fn: just ignore the fn input)"
       "** elim: Unit -> A (fn: pick one elem of a type i.e. set; some sort of \"cheating\" - instead of an elem we pick a morphism)"
       ])]]


   [collapse/ui
    [:id16
     "* Cartesian product (set of pairs): UP (universal construction) - best product triangle: for all other types there's the unique arrow"
     (join
      "\n"
      [
       "  projections: f: C -> A, g: C -> B"
       "- tuple (pair aka record) is better than tripple"
       "??? loop-over-all-types: for each of all possible types: 38:20"
       "** intro: A -> B -> (A,B) tupple"
       "** elim: (A,B) -> A, (A,B) -> B"
       ])]]


   [collapse/ui
    [:id17
     "* Sum type (dual to product - coproduct)"
     (join
      "\n"
      [
       "** intro: A -> either A or B, B -> either A or B"
       "** elim: case e of: left a -> f a, right b -> f b"
       "** in functional programming - tagged unions"
       ])]]


   [collapse/ui
    [:id18
     "* Monoidal Cat: (objs, arrows, prods) looks kinda like multiplication / addition"
     ""]]


   [collapse/ui
    [:id19
     "Algebra of types ..."
     ""]]


   [collapse/ui
    [:id19
     "* Functor: structure preserving mapping between Cats (objs to objs, fns to fns):"
     (join
      "\n"
      [
       "** i.e. if there's an arrow A -> B, then there must be arrow F(A) -> F(B)"
       "** may collapse things, preserves unit obj and composition"
       "** Endofunctor: mapping from the same Cat to the same Cat. Endo ~ inside, \"Endoscopy\""
       "   Category of Endofunctors: Cat of ftors from C to C [C,C]"
       ])]]


   [collapse/ui
    [:id20
     "* Functor Category:"
     (join
      "\n"
      [
       "** pick two Cats C, D; ftors from C to D form a Functor Category [C,D]:"
       "*** ??? Objs are ftors"
       "*** ??? Arrows are NaT (Natural Transformation)"
       ])]]


   [collapse/ui
    [:id21
     "* Adjunction: A pair of ftors: one ftor F adjunct to another ftor G; F and U are not an inverse of each other"
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
       ])]]


   [collapse/ui
    [:id22
     "* Natural transformations NaTs: Polymorphic functions: mapping between ftors:"
     "  see picture at https://youtu.be/JH_Ou17_zyU?t=1h6m23s"]]


   [collapse/ui
    [:id23
     "* Polymorphic function - a function for every single type i.e. multiplication"
     "  (Product) of all obj in a category. Also the dual - the Sum"]]


   [collapse/ui
    [:id24
     "* Categorical End (i.e. Product) and CoEnd (i.e. CoProduct i.e. Sum)"
     "  notation is the integral sign"]]


   [collapse/ui
    [:id25
     "* Monadic return-function: universally polymorphic function - works for any type"
     (join
      "\n"
      [
       "  https://www.youtube.com/watch?v=CfoaY2Ybf8M&t=7m"
       ""
       "** Generalisation of everything. They sub-sume everything else, like adjuctions at a higher level"
       "** Limits Colimits Monads Adjunctions can be redefined as Khan Extentions"
       "** Intuition of Khan Extentions is dificult - they are more abstract than monads"
       "*** Adjunction between a Product and a Function Type is Currying"
       ])]]


   [collapse/ui
    [:id26
     "* Product generalisation: Tensor Product in a monoidal category"
     ""]]])
