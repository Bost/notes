(ns notes.id1
  (:require
   [notes.collapsible :refer [e] :as collapse]
   [clojure.string :refer [join]]
   [notes.Functor :as ftor]
   [notes.Natural-Transformation :as nat]
   [notes.Yoneda :as yoneda]))

;; [e ""]
(def ui
  [collapse/ui
   {:id "id1"
    :title "
A Crash Course in Category Theory - Bartosz Milewski https://www.youtube.com/watch?v=JH_Ou17_zyU"
    :content
    [:span
     "
https://twitter.com/@bartoszmilewski : Computer Science, Haskell, C++. PhD in Quantum Physics
Most important features of a category: Associative composability (x ∗ y) ∗ z = x ∗ (y ∗ z), Identity
Example in programming: Category of Types and Functions (set and functions between sets)
*** Designing computer language:
    Semantics must be provided; done by providing operational semantics
    None of the main prog. languages have (operational semantics) only partially provided;
    Two ways of defining semantics:
    - Operational: \"How it executes\"; reduction relation: " [e "e1 → e2"] "
    - Denotational: mapping into mathematics; interpretation of terms: " [e "⟦e⟧ = ?"] "
      e.g.: " [e "⟦ v : τ  ⊢  v : τ ⟧ = idτ "] "- i.e. the meaning of " [e "⟦...⟧"] " is an identity on " [e "τ"] " i.e. an access to variable " [e "v"]
      #_[:div {:class "language-klipse"} "(identity 1)"] "
*** Cartesian Product CP: set of all pairs
    Relation: A subset of CP; doesn't have a directionality; n-to-n relation
*** Functions: (Sets and functions between sets)
    Pure: must be memoizable (lookup table)
    Total: defined for all arguments
    Partial: defined only for some arguments
    Directionality (arrow \"from → to\" i.e. functions are not symetric); n-to-1 relation
    - Inverse of function is not guaranteed to exist
    Domain, Codomain, Image

*** Morphisms:
    | Latin      | Greek                | Meaning                   | Functor  |
    |------------+----------------------+---------------------------+----------|
    | injective  | monic / monomorphism | distinct Xs → distinct Ys | Faithful |
    | surjective | epic / epimorphism   | all Ys are used           | Full     |

"
      ;; ftor/ui
      ;; nat/ui
      ;; yoneda/ui
      "
*** Khan Extentions: the next abstraction level

*** Adjunctions: weakening of \"equality\" of Categories
    \"inverse\" is defined only for functions not functors
**** e.g. Currying: from a Pair to Function type
*** Adjointness - constructing / generating principle
    - Adjunctions/Adjoins are monads ???
    Adjoin examples:
    (-) x A (product) ⊣ (-)^A (exponential)
    '+' (coproduct) ⊣ '∆' (pairing) ⊣ 'x' (product)
    induction, recursion, Natural Numbers (inductively defined), Lists, ...
    conjunction, disjunction, True, False, Exponentiation
    Quantifiers: ∀ Every, ∃ Exists; Σ Sigma, Π Pi

*** Fibre: a buch of points mapped to the same value; invertibility of a function to a fibre
*** Abstraction: i.e. non-invertibility
   - from all properties (i.e. all points of a fibre) I'm interested only in one
   - e.g. I'm not interested in what was the exact input value of a function,
     I'm interested only if it was an even or odd value
*** Modeling: mapping / injecting
*** HomSet: " [e "HomC(A,B) = {f: A → B}"] " - set of all morphisms " [e "A → B"] " in a category C (Objs of C don't need to be sets)
   External vs. Internal HomSet

*** Free Monoid: has an unique mapping to every other monoid
*** Homomorphism: structure-preserving mapping between 2 algebraic structures (e.g. monoids, groups, rings, vector spaces).
    f(m * n) = f(m) * f(n)

    Individual monoids themselves give category
    Monoids with homomorphisms give category

*** Kleisli category:
   Monad: return: a → m a; bind: m a → (a → m b) → m b
     You can operate on IO Monad
     You can't extract anything from IO Monad (it's lost)
     Monoind in Category of Endofunctors
   Comonad: (w a → b) → (w b → c) → (w a → c)
     You can extract from IO Monad
     You can't put anything to IO Monad
*** Topos: a type of a Category being able to be a replacement for Set Theory; provides among other things a notion of a Subse
*** TODO:
   - Subobject Classifier etc.: see Bartosz's blog
   - Sheaf (Garbe, Faisceau, zvazok)- tool for tracking locally defined data
   - Presheaf: Functor F: Cop → Set

*** Indexed Monad: IxMonad: ibind: m i j a → (a → m j k b) → m i k b
   state composition
   Session Types, Dependent Types, Dependent State Types
*** Curry-Howard-Lambek correspondence: Intuitionistic Logic ↔ Type Theory ↔ Category Theory:
  Function A → B is a proof of logical implication A => B
  Direct relationship between computer programs and mathematical proofs; from 1940-ties
  Link between Computation and Logic;
  Proofs-as-programs and propositions- or formulae-as-types interpretation;
  Proofs (= Programs) can be executed;
  Typed lambda calculi derived from the Curry–Howard-Lambek paradigm led to software like Coq;
  Curry-Howard-Lambek correspondence might lead to unification between mathematical logic and foundational computer science;
  Popular approach: use monads to segregate provably terminating from potentially non-terminating code

    | INTUITIONISTIC (Constructive) LOGIC (Howard)   | TYPE THEORY - Functional Programming (Curry)                         | CATEGORY THEORY (Lambek) |
    |------------------------------------------------+----------------------------------------------------------------------+--------------------------|
    | Proposition of some type - (something is true) | Type (contract - a set of values that passes the contract)           |                          |
    | Proof of some type                             | Term (A program - guarded fn)                                        |                          |
    | Normalisation (Proof equality)                 | Computation (substitute variable with value)                         |                          |
    |------------------------------------------------+----------------------------------------------------------------------+--------------------------|
    | P implies Q: P → Q (i.e. there exists one)     | paricular fn of fn of P-contract to guarded fn of Q-contract: P → Q  |                          |
    | → is constructive implication                  | → is function from-to                                                |                          |
    | false      → false (implies)                   | {}       →  {}  no values (empty set); contract cannot be satisfied  |                          |
    | false      → true                              | {}       →  {.} (one element set)                                    |                          |
    | true       → true                              | {.}      →  {.} (identity function)                                  |                          |
    | true  (not →) false (does not imply)           | {.} (not →) {}                                                       |                          |


** Correspondance of type habitation and proposition
   inhabited - has elems / members
   \"Either a b\" is inhabited if either a or b is inhabited (at least one of them is true / provable)

   Curry: ((a,b) → c) → (a → (b → c))
   Uncurry: (a → (b → c)) → ((a,b) → c)

   Eval: a function of two args / a pair
   \"((a => b), a) → b\" this is modus-ponens in logic \"(a => b) ∧ a → b\"

    | True proposition | False proposition | Conjunction a ∧ b         | Disjunction a ∨ b           | Implication a => b   |
    | Unit-type        | Void-type         | Pair (a,b)                | Either a b                  | Function type a → b  |
    | sinhabited       | not inhabited     |                           |                             |                      |
    | Terminal obj     | Initial obj       | Categorical product a × b | Categorical coproduct a ⎥ b | Exponential obj b^a  |


    0 - void type - ?
    1 - unit type - 0th-power: terminal obj
    2 - bool type (two possible values): 1st-power: the obj itself
    3 - int type - 2nd-power: product
    4 - real type (if continuum hypothesis holds :-)
    5 - ? type

    JavaScript & Category Theory: Category == Contracts + Functions guarded by contracts

** Set vs. Category theory comparision:
   | Set theory                  | Category theory                                          | JavaScript                     |
   |-----------------------------+----------------------------------------------------------+--------------------------------|
   | membership relation         | -                                                        |                                |
   | elements                    | objects                                                  | contracts                      |
   | sets                        | categories                                               |                                |
   | -                           | morphisms (structure-preserving mapping between objects) | functions guarded by contracts |
   | functions                   | functors  (maps between categories)                      |                                |
   | equations between elements  | isomorphisms between objects                             |                                |
   | equations between sets      | equivalences between categories                          |                                |
   | equations between functions | natural transformations (maps between functors)          |                                |

   Categorification: process of weakening structure, weakening equalities down to natural isomorphisms and then adding-in rules
   that these natural isomorphisms have to follow (so it behaves well)
   Counting number of elements in sets is decategorification; from category we get set or from set we get a number

   Monoid homomorphisms: a function between the sets of monoid elements that preserved the monoid structure
   Monoidal functors:    a functor between categories that preserves the monoidal structure (should preserve multiplication)
   from functor(prodn([x, y, ..])) to prodn([functor(x), functor(y), ..])
   Monoidal monad:       ???

   Functor:
   \"forget the indexing (domain functor)\"

*** Contract = Object
*** Product: examples:
    Objects   - numbers
    Morphisms - functions 'less/greater or equal than'

*** Isomorphism (bijection when f is a function on set / sets):
  ∀ f: X → Y  there ∃ g: Y → X such that g ∘ f = idX and f ∘ g = idY; idX, idY are identity morphisms on X, Y
  (f is invertible and g is the inverse of f)

** Category theory - Modeling (new vocabulary)
   | hierarchies                | partial orders     |
   | symmetries                 | group elements ?   |
   | data models                | categories         |
   | agent actions              | monoid actions     |
   | local-to-global principles | sheaves (lanovica) |
   | self-similarity            | operads            |
   | context                    | monads             |


** olog = ontology log
   Different branches of mathematics can be formalized
   into categories. These categories can then be connected together by functors. And the
   sense in which these functors provide powerful communication of ideas is that facts and
   theorems proven in one category can be transferred through a connecting functor to
   yield proofs of an analogous theorem in another category. A functor is like a conductor
   of mathematical truth.

*** Mappings: X → Y (Zobrazenia):
    Surjection: all Ys are used;                                     |X| ≥ |Y| (onto; \"at least as big\")
    Injective:  distinct Xs → distinct Ys;                           |X| ≤ |Y| (? one-to-one ?)
    Bijection:  exact pairing between X, Y;                          |X| = |Y| (vzajomne jednoznacne zobrazenie, \"same size\")
    Strict:     Surjection from X to Y but no bijection from Y to X; |X| < |Y| (? double usage of some Ys ?, \"strictly bigger\")

" ;; there must be an extra empty line otherwise the last line won't be shown
]      
     }])
