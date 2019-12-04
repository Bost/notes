# Complexity classes & Zoo: https://complexityzoo.uwaterloo.ca/Complexity_Zoo
  NP      - decision problems solvable by non-deterministic TM (Turing Machine) in polynomial time
  PSPACE  - decision problems solvable by     deterministic TM                  in polynomial space
## computational problem type: e.g. problems: decision, function, counting, optimization, promise, etc.
## model of computational: e.g. deterministic TM (Turing Machine), non-deterministic TM, boolean circuits, quantum TM, monotone circuits
## bounded resource(s): e.g. space, time
## bounds: e.g. polynomial time, logarithmic space, constant depth

# Unary languages
# Sparse languages
# Untyped Pi π-Calculus
Business Process Modeling

# Untyped Lambda λ-Calculus
  I.e. Formal calculus for manipulating functions; Turing-complete (can simulate any Turing machine)
  e ::= x | λx.e | e e'
   e        - λ-term; expression of a λ-calculus; e.g. e, e', f, g, h are λ-terms
   x        - variable; e.g. x, y, z
   e e'     - left associative application of expression e to expression e'; e.g. M N L = ((M N)L)
   λx.e     - function abstractions of x over e; abstract away the details of expression e
   Currying: Partial function application; Fixing some function parameters
   Application takes precedence over abstraction e.g. λx.M N = λx.(M N)
   Successive abstraction λxy.e = λx.(λy.e)
   Variable occurences in a λ-term; Free / Bounded / Binding
   TODO what is a normal form of a λ-term
## TODO Positive properties of an Untyped λ-Calculus
### Turing-complete
## Negative properties of an Untyped λ-Calculus
### Self applications are allowed
### Existence of normal forms for λ-terms is not guaranteed; this may result in undesired infinite calculations
### Each λ-term has a fixed point. However some important functions e.g.:
    g(n) = 2^n
    succ(n) = n + 1
   don't have a fixed point and cannot belong an Untyped λ-Calculus

# Simply Typed Lambda λ-Calculus: λ->
  Keeps the positive and remove the negative points on Untyped λ-Calculus:
### No more self application
### No infinite reduction sequencies (i.e. calculations)
### Not every simply typed function needs to have a fixed point.
  Drawback: the λ-> is much too weak to encapsulate every computable function and hence it's not usable for the formalization of math.
  Contains simple (basic) and funtion types
  e ::= x:τ | (λ(x:σ).(e:τ)):(σ->τ) | e:τ e':τ | c:τ
   e:τ                   - λ-term; expression of a simply typed λ-calculus; e.g. e:τ, e':τ, f:τ, g:τ, h:τ are simply typed λ-terms
   x:τ                   - type variable of the simple type τ; e.g. x:τ, y:τ, z:τ
   e:τ e':τ              - left associative application of expression e:τ to expression e':τ; e.g. e:τ f:τ g:τ = ((e:τ f:τ)g:τ)
   (λ(x:σ).(e:τ)):(σ->τ) - function abstractions of an function with function type σ->τ; abstract away the details of the expression e:τ and declare the abstraction to be of the function type σ->τ
   c:τ                   - ? constant ?
   TODO simple type

# Damas-Hindley-Milner Calculus
  e ::= x | λx.e | e e' | let x = e in e'
  W Algorithm - for type inference: W(Γ,e) = (S,τ)
  Γ - typing env
  S - state
  τ - type

# Hindley-Milner type system - type inference
# Alpha α-conversion / α-equivalence of terms: Term are same up to a renaming of binding and bound vars
# Beta β-reduction (computational): Substitute var x with s in term t: (λx.t)s = [s/x]t
  β- reduction in computation a.k. calculation rule: Simplify proofs by replacing formal params with terms.
  fst<g , h > = g
  snd<g , h > = h
  (λx.h)g = [g/x]h      - substitute variable x in the expression (i.e λ-term) h by the (i.e λ-term) g
  (λx.f)x = [x/x]f = f  - ? do nothing, i.e. void substitution ?
## Fixed Point Theorem: for each λ-term L there exists a λ-term M (a.k.a fixed point) such that LM =β M
   =β   - β-conversion a.k.a β-equality
# TODO Eta η-extentionality principle

# Tautology - "this is always true"

# Type Theory: Extention of lambda calculus with explicit types
## ITT Inentional Type Theory
## OTT Observational Type Theory
   Type of a Variable - a set(?) of possible values of that type
   From context Gamma a M can be derrived such that x:A.B
   Context Gamma, x:A, y:Bx (e.g. x be a number with property Bx, y is a variable for the proof of type Bx)
   What's the point of having a number? Well we can count up to that number.
   Depandent type theory is the master theory of all programming languages. If you understand dependent type theory then you understand everything (every programming language)
   Maybe Type: A or B (e.g. A or Fail, A or AirMessage)
## Homotopy Type Theory: ? Equivalence ?

# Axiom of Choice - see "Type Theory Foundations, Lecture 3-wJLTE8rnqH0.mp4"
"the greatest intelectual achievement???"
Predicate = vyrok = tvrdenie
Proposition = ???
(Banach Tarsky Paradox: Slice up an object with a volume into parts with no volume, and by putting it together get 2 same objects - "create an object for free")

# Haskell / Agda comparison:
|             | Haskell                              | Agda                                     |
|-------------|--------------------------------------|------------------------------------------|
|             |                                      | full higher order logic with existential |
|             |                                      | and universal quantification             |
|-------------|--------------------------------------|------------------------------------------|
| Type system | Unsound                              | sound                                    |
|             | (arbitrary properties can be prooven |                                          |
|             | i.e. every single type is inhabited) |                                          |
|             | (loop : A, loop = loop)              |                                          |
|-------------|--------------------------------------|------------------------------------------|
|             |                                      | ? Always terminates ?                    |

# Modus Pones: applications of a function to an argument: Agda, Coq, Isabelle
## MP naturally generalizes to instationation of universal quantifiers

# Goedel's Incompleteness Theorem:
 Every principle is either (A) too restrictive or (leaves out a good programm) or (B) not restrictive enough (allows some bad programs)
# Full employment Theorem: take (A) and search for a new class to add in order to improve the language withouth allowing bad programs.
# Theory of Reflexive Domain 1:21 Video 2
# Impossibility of a perfect type-checker for a programming language
  It it’s impossible to have a procedure that figures out whether an arbitrary
  program halts, it’s easy to show that it’s impossible to have a procedure that
  is a perfect recognizer for any overall run time property.
  A program that type-checks is guaranteed not to cause a run-time type-error.
  But since it’s impossible to recognize perfectly when programs won’t cause
  type-errors, it follows that the type-checker must be rejecting programs that
  really wouldn’t cause a type-error. The conclusion is that no type-checker is
  perfect—you can always do better!

# Krakatoa and Jessie: verification tools for Java and C programs
  Why3: platform for deductive program verification
  git clone https://scm.gforge.inria.fr/anonscm/git/why3/why3.git
  A user can write WhyML programs directly and get correct-by-construction OCaml programs through an automated extraction mechanism

# Trinity 1. Logic & Proof Theory: Philosophy; 2. Type Theory: Computer Science; 3. Category Theory: Mathematics
# Proof Theory: Proof of soundness, proof of completeness
# PT 1 - Judgements & Propositions
## judgement (obj of knowledge); judgemens are made about propositions
   'A is true', 'A is false', M : A - M is a proof of A i.e. M is a program which has a type of A
   'A' - proposition
   'true' - judgement on a proposition
  experiment, observation
  sampling, counter examples
  judge, jury, religion, boss, conviction "No bugs in my code!"
  "I don't see why not": psycho (the oposing party must find argument)
  A mathematical proof:
  - an argument that convinces other mathematicians :)
  - verification of a proposition by a chain of logical deductions from a set of axioms
  - deduction from hypotheses to conclusion in which each step is justified by one of a finite list of rules of inference
    https://home.sandiego.edu/~shulman/papers/rabbithole.pdf
  Proposition is a statement: can be true or false
  Predicate is a proposition: truth depends on the values of variable(s)
  Verificationist: The meaning of a connective is given by it's introduction rule(s)
  'A and B' is true; A & B : true
## Local soundness of the elimination rules: elim. rules are not too strong
   - no information is gained by applying and eliminating a particular rule
## Local completeness: elim. rules not too weak
## LOcal expantion: witness for the completeness of the rules

# PT 2 - Computational Interpretation: Curry Howard Isomorphism
  - Capturing generic notion of effect: functional programming monad from logical point of view
  - Computational interpretations of monad come out of logical considerations
  - Quote & Eval (not presented in the lecture)
  Lax proposition: 'there is something weaker then truth'; 'Possible truth'; It
  may or may not be true (in case of non-terminating computation or it might
  terminate with some other effects on the way)
  ◯ A : true  A - proposition, ◯ - 'circle'; 'A is true in the lax sense'; Monad A
  Monad - when interacting with real world a failure is always one of the eventualities
        - monad laws are proof-equalities
# PT 3 - Proof Search & Sequent Calculus

# Chomsky hierarchy
  | Grammar | Languages              | Automaton                                       | Production rules (constraints)* | Examples[3]                                                     |
  | Type-0  | Recursively enumerable | Turing machine                                  | α A β -> β                      | L = {w \vertical-line w describes a terminating Turing machine} |
  | Type-1  | Context-sensitive      | Linear-bounded non-deterministic Turing machine | α A β -> α γ β                  | L = {a^n b^n c^n \vertical-line n > 0}                          |
  | Type-2  | Context-free           | Non-deterministic pushdown automaton            | A -> α                          | L = {a^n b^n \vertical-line n > 0}                              |
  | Type-3  | Regular                | Finite state automaton                          | A -> a and A -> a B             | L = {a^n \vertical-line n >= 0}                                 |
## Meaning of symbols:
   a = terminal
   A, B = non-terminal
   α, β, γ = string of terminals and/or non-terminals
   α, β = maybe empty
   γ = never empty

# Indirect left recursion https://en.wikipedia.org/wiki/Left_recursion
  Indirect left recursion occurs when the definition of left recursion is satisfied via several substitutions. It entails a set of rules following the pattern
  [e "A0 -> β0 A1 α"]
  [e "A1 -> β1 A2 α"]
  [e "An -> βn A0 α"]
  where [e "β0,β1, ... ,βn"] are sequences that can each yield the empty string,
  while [e "α0,α1, ... ,αn"] may be any sequences of terminal and nonterminal
  symbols at all. Note that these sequences may be empty. The derivation
  [e "A0 => β0 A1 α0 =>^+ A1 α0 =>^+ β1 A2 α1 α0 =>^+ ... =>^+ A0 αn ... α1 α0"]
  then gives A0 as leftmost in its final sentential form.
# Normal Forms of Formulas
## DNF - Disjunctive Normal Form / Formula e.g. (A ∧ ¬B ∧ C) ∨ (¬D ∧ E ∧ F)
## CNF - Conjunctive Normal Form / Formula e.g. (A ∨ ¬B ∨ C) ∧ (¬D ∨ E ∧ F)
