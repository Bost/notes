#lang notes

@block{@block-name{Crypthography}
  Pseudo Random Number Generator PRNG and Random Number Generator RNG are only
  as good as their underlying entropy source.

  entropy creation / usage / requirements / amount
  https://www.blackhat.com/docs/us-15/materials/us-15-Potter-Understanding-And-Managing-Entropy-Usage.pdf

  Entropy:
  Quantity of uncertainty of an outcome. Entropy generation - shuffling play
  cards Full entropy is 100% random. Can be measured by tests

  Randomness:
  Quality of uncertainty of an outcome. PRNG - dealing the deck of play cards
  to. Randomness either is or is not.

  The better the card shuffling (entropy), the more random the card deal will
  be(???)

  $ cat /proc/sys/kernel/random/entropy_avail
  256

  OpenSSL PFS - OpenSSL Perfect Forward Security
  National Institute of Standards and Technology NIST
}

@block{@block-name{Rethinking Reactivity}
  Rich Harris - Rethinking reactivity
  https://youtu.be/AdNJ3fydeao

  Reactivity = forward referencing
  Dependency graph of elements

  Functional Reactive Programming:
  Specify dynamic behavior of a value completelly at the time of declaration.

  I.e. it's about data flow, tracking values through the application. I.e. when
  a value changes the application should react.

  to reconcile ~ to compare, e.g. two bank accounts

  See also svelte
  https://svelte.dev/
}

@block{@block-name{Complexity classes & Complexity Zoo}
  https://complexityzoo.uwaterloo.ca/Complexity_Zoo
  NP      - decision problems solvable by non-deterministic TM (Turing Machine) in polynomial time
  PSPACE  - decision problems solvable by     deterministic TM                  in polynomial space

  @block{@block-name{computational problem type: e.g. problems:}
    decision
    function
    counting
    optimization
    promise
    etc.
  }

  @block{@block-name{model of computational: e.g. }
    deterministic TM (Turing Machine)
    non-deterministic TM
    boolean circuits
    quantum TM
    monotone circuits
  }

  @block{@block-name{bounded resource(s): e.g. space, time}
  }

  @block{@block-name{bounds: e.g. polynomial time, logarithmic space, constant depth}
  }
}

@block{@block-name{Unary languages}
}

@block{@block-name{Sparse languages}
}

@block{@block-name{Untyped Pi π-Calculus}
  Business Process Modeling
}

@block{@block-name{Lambda λ-Calculus Notation history / development}
  John Harrison Introduction to Functional Programming
  http://www.cl.cam.ac.uk/teaching/Lectures/funprog-jrh-1996/all.pdf

  Bertrand Russell and Alfred North Whitehead, Principia Mathematica, where they
  used the notation f(xˆ) [^ above x] to mean a function of x, rather than the
  value of f in the point x. Alonzo Church modified the notation by taking the
  xˆ before the function application, i.e. xˆ.f(x). A typesetter wrote that as
  Λx.f(x), someone read that as the Greek letter lambda.
}

@block{@block-name{Untyped Lambda λ-Calculus}
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

  @block{@block-name{TODO Positive properties of an Untyped λ-Calculus}
    @block{@block-name{Turing-complete}
    }
  }

  @block{@block-name{Negative properties of an Untyped λ-Calculus}
     - Self applications are allowed
     - Existence of normal forms for λ-terms is not guaranteed; this may result in
       undesired infinite calculations
     - Each λ-term has a fixed point. However some important functions e.g.:
      g(n) = 2^n
      succ(n) = n + 1
     don't have a fixed point and cannot belong an Untyped λ-Calculus
   }
}

@block{@block-name{Simply Typed Lambda λ-Calculus: λ->}
  Keeps the positive and remove the negative points on Untyped λ-Calculus:

  @block{@block-name{No more self application}
  }

  @block{@block-name{No infinite reduction sequencies (i.e. calculations)}
  }

  @block{@block-name{Not every simply typed function needs to have a fixed point.}
    Drawback: the λ-> is much too weak to encapsulate every computable function
    and hence it's not usable for the formalization of math.

    Contains simple (basic) and funtion types
    e ::= x:τ | (λ(x:σ).(e:τ)):(σ->τ) | e:τ e':τ | c:τ
    e:τ                   - λ-term; expression of a simply typed λ-calculus; e.g. e:τ, e':τ, f:τ, g:τ, h:τ are simply typed λ-terms
    x:τ                   - type variable of the simple type τ; e.g. x:τ, y:τ, z:τ
    e:τ e':τ              - left associative application of expression e:τ to expression e':τ; e.g. e:τ f:τ g:τ = ((e:τ f:τ)g:τ)
    (λ(x:σ).(e:τ)):(σ->τ) - function abstractions of an function with function type σ->τ; abstract away the details of the expression e:τ and declare the abstraction to be of the function type σ->τ
    c:τ                   - ? constant ?
    TODO simple type
    }
}

@block{@block-name{Damas-Hindley-Milner Calculus}
  e ::= x | λx.e | e e' | let x = e in e'
  W Algorithm - for type inference: W(Γ,e) = (S,τ)
  Γ - typing env
  S - state
  τ - type
}

@block{@block-name{Hindley-Milner type system - type inference}
}

@block{@block-name{Alpha α-conversion / α-equivalence of terms}
  Terms are same up to a renaming of binding and bound vars
}

@block{@block-name{Beta β-reduction (computational)}
  Substitute var x with s in term t: (λx.t)s = [s/x]t
  β-reduction in computation a.k. calculation rule:
  Simplify proofs by replacing formal params with terms.
  fst<g , h > = g
  snd<g , h > = h
  (λx.h)g = [g/x]h      - substitute variable x in the expression (i.e λ-term) h by the (i.e λ-term) g
  (λx.f)x = [x/x]f = f  - ? do nothing, i.e. void substitution ?

  Fixed Point Theorem:
  for each λ-term L there exists a λ-term M (a.k.a fixed point) such that LM = β M
  =β   - β-conversion a.k.a β-equality

  β-computational reduction looks a bit like the ϵA the "evaluation" natural
  transformation from ("eval") from
  "F. William Lawvere: Diagonal arguments and cartesian closed categories."
  http://tac.mta.ca/tac/reprints/articles/15/tr15abs.html

}

@block{@block-name{TODO Eta η-extentionality principle}
  λA​ looks a bit like the η-extentionality principle. See
  "F. William Lawvere: Diagonal arguments and cartesian closed categories."
  http://tac.mta.ca/tac/reprints/articles/15/tr15abs.html

}

@block{@block-name{Tautology - "this is always true"}
}

@block{@block-name{Type Theory: Extention of lambda calculus with explicit types}
  @block{@block-name{ITT Inentional Type Theory}
  }

  @block{@block-name{OTT Observational Type Theory}
  Type of a Variable - a set(?) of possible values of that type
  ? From context Gamma a M can be derrived such that x:A.B ?
  Context $\Gamma, x:A, y:Bx$ (e.g. $x$ be a number with property $Bx$, $y$ is a variable for the proof of type $Bx$)
  What's the point of having a number? Well we can count up to that number.
  Depandent type theory is the master theory of all programming languages. If you understand dependent type theory then you understand everything (every programming language)
  Maybe Type: A or B (e.g. A or Fail, A or AirMessage)
  }

  @block{@block-name{Homotopy Type Theory: ? Equivalence ?}
  }
}

@block{@block-name{Axiom of Choice - see "Type Theory Foundations, Lecture 3-wJLTE8rnqH0.mp4"}
  "the greatest intelectual achievement???"
  Predicate = vyrok = tvrdenie
  Proposition = ???
  (Banach Tarsky Paradox: Slice up an object with a volume into parts with no
  volume, and by putting it together get 2 same objects - "create an object for
  free")
}

@block{@block-name{Theorem provers vs. proof assistants}
  https://en.wikipedia.org/wiki/Proof_assistant
  both are used for formal verification and automated reasoning.
  TPs focuse on automated proof search and providing a "yes" or "no" answer.
  PAs provide an interactive environment for users to construct, verify, and
  interact with formal proofs. PAs offer a higher level of interactivity and
  control, allowing users to manually guide the proof process and explore
  different proof strategies.
}

@block{@block-name{Haskell / Agda comparison}
  Coq:
  formal proof management system. It provides a formal language to write
  mathematical definitions, executable algorithms and theorems together with an
  environment for semi-interactive development of machine-checked proofs.

  Agda:
  dependently typed functional programming language and a proof assistant (i.e. an
  interactive system for writing and checking proofs), based on intuitionistic
  type theory, a foundational system for constructive mathematics developed by Per
  Martin-Löf.

  Idris:
  Language for Type-Driven Development

  Lean:
  theorem prover and programming language, based on the calculus of
  constructions with inductive types.

  ACL2 A Computational Logic for Applicative Common Lisp
  theorem prover based on Common Lisp. It uses a variant of Lisp syntax called
  ACL2 syntax, which extends Common Lisp with additional constructs for
  specifying logical formulas and proof goals.

  HOL Higher Order Logic
  theorem prover, which has various implementations, including HOL4 and HOL
  Light. HOL Light, in particular, is implemented in OCaml but provides a
  Lisp-like syntax for specifying and manipulating formal proofs.

  Haskell:
  statically typed, purely functional programming language with type inference
  and lazy evaluation.

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
}

@block{@block-name{Modus Pones: applications of a function to an argument: Agda, Coq, Isabelle}
  @block{@block-name{MP naturally generalizes to instationation of universal quantifiers}
  }
}

@block{@block-name{Goedel's Incompleteness Theorem:}
  Every principle is either (A) too restrictive or (leaves out a good programm) or
  (B) not restrictive enough (allows some bad programs)
}

@block{@block-name{Full employment Theorem}
  Take (A) and search for a new class to add in order to improve the language
  without allowing bad programs.
}

@block{@block-name{Theory of Reflexive Domain 1:21 Video 2}
}

@block{@block-name{Impossibility of a perfect type-checker for a programming language}
  It's impossible to have a procedure that figures out whether an arbitrary
  program halts, it's easy to show that it's impossible to have a procedure that
  is a perfect recognizer for any overall run time property. A program that
  type-checks is guaranteed not to cause a run-time type-error. But since it's
  impossible to recognize perfectly when programs won't cause type-errors, it
  follows that the type-checker must be rejecting programs that really wouldn't
  cause a type-error. The conclusion is that no type-checker is perfect.
}

@block{@block-name{Krakatoa and Jessie: verification tools for Java and C programs}
  Why3: platform for deductive program verification
  https://why3.lri.fr/
  git clone https://scm.gforge.inria.fr/anonscm/git/why3/why3.git

  A user can write WhyML programs directly and get correct-by-construction OCaml
  programs through an automated extraction mechanism
}

@block{@block-name{The trinity of Philosophy, Computer Science and Mathematics}
  - Philosophy       : Logic & Proof Theory
  - Computer Science : Type Theory
  - Mathematics      : Category Theory
}

@block{@block-name{Proof Theory: Proof of soundness, proof of completeness}
}

@block{@block-name{PT 1 - Judgements & Propositions}
  @block{@block-name{judgement (obj of knowledge); judgemens are made about propositions}
  'A is true', 'A is false', M : A - M is a proof of A i.e. M is a program which has a type of A
  'A' - proposition
  'true' - judgement on a proposition
  experiment, observation
  sampling, counter examples
  judge, jury, religion, boss, conviction "No bugs in my code!"
  "I don't see why not": psycho (the oposing party must find argument)
  A mathematical proof:
    - an argument that convinces other mathematicians
    - verification of a proposition by a chain of logical deductions from a set of axioms
    - deduction from hypotheses to conclusion in which each step is justified by one of a finite list of rules of inference
      https://home.sandiego.edu/~shulman/papers/rabbithole.pdf
  Proposition is a statement: can be true or false
  Predicate is a proposition: truth depends on the values of variable(s)
  Verificationist: The meaning of a connective is given by it's introduction rule(s)
  'A and B' is true; A & B : true
  }

  @block{@block-name{Local soundness of the elimination rules: elim. rules are not too strong}
    - no information is gained by applying and eliminating a particular rule
  }

  @block{@block-name{Local completeness: elim. rules not too weak}
  }

  @block{@block-name{LOcal expantion: witness for the completeness of the rules}
  }
}

@block{@block-name{PT 2 - Computational Interpretation: Curry Howard Isomorphism}
  - Capturing generic notion of effect: functional programming monad from logical point of view
  - Computational interpretations of monad come out of logical considerations
  - Quote & Eval (not presented in the lecture)
  Lax proposition: 'there is something weaker then truth'; 'Possible truth'; It
  may or may not be true (in case of non-terminating computation or it might
  terminate with some other effects on the way)
  ◯ A : true  A - proposition, ◯ - 'circle'; 'A is true in the lax sense'; Monad A
  Monad - when interacting with real world a failure is always one of the eventualities
        - monad laws are proof-equalities
}

@block{@block-name{PT 3 - Proof Search & Sequent Calculus}
}

@block{@block-name{Chomsky hierarchy of formal grammars}
  | Grammar | Languages | Automaton | Production rules (constraints)* | Examples[3] |
  ** Meaning of symbols:
  a = terminal
  A, B = non-terminal
  α, β, γ = string of terminals and/or non-terminals
  α, β = maybe empty
  γ = never empty
}

@block{@block-name{Indirect left recursion}
  Wikipedia: Left Recursion
  https://en.wikipedia.org/wiki/Left_recursion

  Occurs when the definition of LR is satisfied via several substitutions. It
  entails a set of rules following the pattern:

    | A0 -> β0 A1 α |
    | A1 -> β1 A2 α |
    | An -> βn A0 α |

  where [e "β0,β1, ... ,βn"] are sequences that can each yield the empty string,
  while [e "α0,α1, ... ,αn"] may be any sequences of terminal and nonterminal
  symbols at all. Note that these sequences may be empty. The derivation
  [e "A0 => β0 A1 α0 =>^+ A1 α0 =>^+ β1 A2 α1 α0 =>^+ ... =>^+ A0 αn ... α1 α0"]
  then gives A0 as leftmost in its final sentential form.
}

@block{@block-name{Normal Forms of Formulas}
  DNF Disjunctive Normal Form / Formula: e.g. (A ∧ ¬B ∧ C) ∨ (¬D ∧ E ∧ F)
  CNF Conjunctive Normal Form / Formula: e.g. (A ∨ ¬B ∨ C) ∧ (¬D ∨ E ∧ F)
}

@block{@block-name{Homoiconicity}
  LISP programs are represented as LISP structures.

  Emacs Lisp form can be represented by a data structure called an s-expression
  that prints out the same as the form.

  "five times the sum of seven and three" can be written as a s-expression with
  prefix notation. In Lisp, the s-expression might look like (* 5 (+ 7 3))

  s-expression / sexpr or sexp - symbolic expression is:
  1. an atom, or
  2. an expression of the form (x . y) where x and y are s-expressions.
}

@block{@block-name{Quotation}
  Quotation:
  To refer to certain expressions of the language whose meanings we were trying
  to paraphrase, put the expression in quotes, i.e. quote the expression.

  Quasiquote:
  - Allows to quote only a part of a list
  - More general mechanism than quote for creating nested list structure without
    using constructors explicitly.
}

@block{@block-name{Variable scope within a certain function}
  @block{@block-name{static / lexical}
    Scope of a variable v is the program block (e.g. a function): within that
    block, the variable name exists, and is bound to the variable's value, but
    outside that block, the variable name does not exist.
  }

  @block{@block-name{dynamic}
    Scope of a variable v is the time-period during which the program block
    (e.g. a function) is executing: while the function is running, the variable
    name exists, and is bound to its value, but after the function returns, the
    variable name does not exist.
  }
}

@block{@block-name{Fixed-point combinator / Y-combinator}
  "implement recursion in a language without recursion"
  Essentials: Functional Programming's Y Combinator - Computerphile
  https://youtu.be/9T8A89jgeTI

  Fixed-point combinators in lambda calculus
  https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus
  Definition:
  Y = λf.(λx.f(x x) λx.f(x x))

  Y g = (λf.(λx.f(x x) λx.f(x x)))g   (by definition of Y)
      = (λx.g(x x) λx.g(x x))         (by β-reduction of λf: applied Y to g)
      = g(λx.g(x x) λx.g(x x))        (by β-reduction of λx: applied left function to right function)
      = g(Y g)                        (by second equality)
      = g(g(Y g))
      = g(g(g(Y g)))
      = g(...(g(Y g))...)

  YouTube: Encoding of Boolean values
  https://youtu.be/9T8A89jgeTI?t=217
  One is opposite of the other:
  TRUE  = λx.λy.x
  FALSE = λx.λy.y

  TODO Wikipedia: SKI combinator calculus
  https://en.wikipedia.org/wiki/SKI_combinator_calculus

  Bartosz Milewski: The Fall of the SKI Civilization
  https://bartoszmilewski.com/2020/09/06/the-fall-of-the-ski-civilization/
}

@block{@block-name{Self-balancing binary search trees}
 for basic operations
 - AVL-Tree (Adelson-Velsky-Landis)
 - RB-Tree (Red-Black):
   Wikipedia: Red-Black Tree
   https://en.wikipedia.org/wiki/Red%E2%80%93black_tree
}

@block{@block-name{Endophora - Linguistics}
  Wikipedia: Endophora
  https://en.wikipedia.org/wiki/Endophora

  @block{@block-name{Subcategories of Endophoras: φέρω (phérō, "I carry")}
    @block{@block-name{Anaphora "carrying back" ἀνά (aná, "up")}
      Wikipedia: Anaphora (linguistics)
      https://en.wikipedia.org/wiki/Anaphora_(linguistics)
      - Usage of an expression that depends upon an antecedent (previous)
        expression.
      - "Sally arrived, but nobody saw her" - 'her' is an anaphor (= anaphoric
        term)
    }

    @block{@block-name{Cataphora "carrying forward" κατά (kata, "downwards")}
      Wikipedia: Cataphora
      https://en.wikipedia.org/wiki/Cataphora
      - Usage of an expression that depends upon a postcedent (later) expression.
      - "If you want some, there are cookies in the kitchen" - 'some' is a
        cataphor (= cataphoric expression)
    }

    @block{@block-name{Self-reference}
      Wikipedia: Self-reference
      https://en.wikipedia.org/wiki/Self-reference
    }
  }
}

@block{@block-name{Anaphoric Macro}
  Wikipedia: Anaphoric Macro
  https://en.wikipedia.org/wiki/Anaphoric_macro
  - captures some form supplied to the macro which may be referred to by an
    anaphor (i.e. expression that depends on a previous expression)

  - Example (ANSI Common Lisp; works also in sbcl - Steel Bank Common Lisp ):
    Sum the value of non-nil elements, where 'it' refers to the values of
    elements that do not equal nil:
    @lisp{
      (loop for element in '(nil 1 nil 2 nil nil 3 4 6)
            when element sum it)
      ;; >= 16
      ;;
      (defmacro aif (test-form then-form &optional else-form)
        `(let ((it ,test-form))
               (if it ,then-form ,else-form)))

      (aif (+ 2 7)
        (format nil "~A does not equal NIL." it)
        (format nil "~A does equal NIL." it))
   }
}

@block{@block-name{Method Chaining}
  @javascript{
   someThings
     .filter(x => x.count > 10)
     .sort((a, b) => a.count - b.count)
     .map(x => x.name)
     }
}

@block{@block-name{Typing}
  Gradual Typing, Occurrence Typing

  @block{@block-name{The Little Typer}
    Atom is a type.

    Judgment is an attitude taken towards an expression. E.g. "Something is an Atom".

    Four forms of Judgment
    1. ___ is a ___. (declaration)
    2. ___ is the same ___ as ___. (value comparison)
    3. ___ is a type. (type declaration)
    4. ___ and ___ are the same types. (type comparison)

    Pre-supposition "Some forms of judgment only make sense after an earlier judgment."

    Normal Form of an expression described by a type - the most direct way of
    writing the form.

    Normal Form of '(+ 0 26)' is '26'. An expression with '+' at the top is a
    value, a.k.a canonical expression.

    'define' associates a name with an expression.
   }
}

@block{@block-name{Semantics}
  @block{@block-name{Denotational Semantics aka Denotational Model}
    denotation ~ meaning
    Mapping of syntactic constructs to a semantic domain.
    It explains how programs in <language> are evaluated.

    It provides a way to define the behavior of a program by mapping it to a
    mathematical object that represents its meaning.

    E.g. DS in a simple programming language, the meaning of an arithmetics
    expression might be represented by a function mapping the expression to a
    number (its value). In this case, semantic domain is the set of numbers.

    DS/DM contrasted with:
    - Operational Semantics
      OS defines the meaning of a programming construct in terms of state
      transitions and execution of abstract machines
    - Axiomatic Semantics:
      Uses logical assertions to describe the behavior of a program
  }

  @block{@block-name{Operational Semantics}
    https://en.wikipedia.org/wiki/Operational_semantics

    formal programming language semantics

    correctness, safety, security, etc. are verified by constructing proofs from
    logical statements about its execution and procedures, rather than by
    attaching mathematical meanings to its terms (denotational semantics).
  }
}

@block{@block-name{A Pamphlet against R}
  Programmers write code which gets in the way to understanding, rather than
  facilitate it.
  One of the most extreme examples is the code from Donald Knuth's The Art of
  Computer Programming. Written in the MIX assembly, the code only hides the
  conceptual structure of presented solutions from the reader.
}

@block{@block-name{Scheme Lisp}
  Scheme as a meta-language for itself – this technique, called meta-circular
  evaluation, has been explored in the grand book Structure and Interpretation
  of Computer Programs. This idea was at the heart of the seminal paper
  Recursive Functions of Symbolic Expressions and their Computation by Machine
  by John McCarthy, which gave birth to the predecessor of Scheme called Lisp.

  A nesting of pairs (cons cells) whose last right value is not the empty list
  is called improper list

  Each occurrence of the (random n) expression can be replaced with a value
  randomly chosen from the range 0 to n − 1. That's nondeterminism - i.e. reason
  prefer the term expression-based programming to functional programming.
  Disallowing such nondeterministic expressions would make many simple things
  complex.
}

@block{@block-name{Dynamic Dispatching}
  Tiago Cogumbreiro - CS450 S21 30 01 Dynamic dispatching
  https://youtu.be/UoWWNVYyJKw

  i.e. Operator Overloading, i.e. the same operation for multiple purposes
}

@block{@block-name{Dynamic Programming}
  Dynamic Programming - Learn to Solve Algorithmic Problems & Coding Challenges
  https://youtu.be/oBt53YbR9Kk

  Memoization
  Tabulation
}

@block{@block-name{Authentication AuthN vs. Authorization AuthZ}
  (1.) AuthN process verifies identity, then (2.) AuthZ process verifies access
  rights.
  https://www.onelogin.com/learn/authentication-vs-authorization

  Authentication AuthN process verifies that someone / something is who they say
  they are. "Is that thing authentic, i.e. real / original? Or a fake?"
  AuthN nechanisms:
  - username / password
  - USB security tokens / mobile tokens
  - One Time Pin OTP via SMS / App, etc
  - Biometric

  Authorization AuthZ process determines a user or service's level of access.
  AuthZ defines policies. And Access Control AC is part of AuthZ that enforces
  policies.
  AuthZ mechanisms:
  - Access Control List ACL: allow / deny rules
  - Public / Protected / Private / etc. access levels
}

@block{@block-name{Referential Transparency}
  Referentially Transparent ~ can be replaced with some value.
}

@block{@block-name{Continuations}
 - concept. not a language feature
 - like "scope" or "value"

 Parts of calculated expression:
 - redex - reducible expression, "foreground"

 - evaluation context - "background" how to continue evaluation process, i.e. the
 continuation
 it changes over the course of the program
 rest, remainder of the program

 why is continuation interesting - compilers care

 exit - discarding continuation

 in try-catch, the catch part delimits the continuation

 first classs continuations - continuations are values in a given programming language

 continuations can be through as functions of one value
 "call with current continuation" is an old (not really usefull) concept

 delimited continuations
 prompt / control - ie.e. delimit continuation up to the prompt
}

@block{@block-name{A very minimal introduction to some terms from type theory}
  https://wscp.dev/posts/tech/java-pattern-matching/#a-very-minimal-introduction-to-some-terms-from-type-theory

  Bottom, or Empty Type: ⊥, `Nothing`
  describes the set of all values which can't be computed in the Turing-complete
  sense. This set is usually empty for any normal programming language (Ø).

  Top Type: ⊤, `Any`
  represents every value of every type - the universal set of values, U

  Unit Type: () 1 with the unique element ★.
  has only one value. There is only one instance of that one value, and it is
  impossible to create more of it.

  Product type 𝜏1 × 𝜏2 with pairs written ⟨−; −⟩.
  composed of two or more constituent types. In general, a product type is a
  list of two or more types grouped together. A product type’s arity, or degree,
  is the number of constituent types within it.

  Sum type 𝜏1 + 𝜏2 with injections inl and inr.
  encodes that a type can be any one of its constituents at a single time. They
  are also known as tagged union types because, in type theory, they are usually
  represented as a type whose range of values is the union set of its
  components, where each component type is “tagged” with a label.
  You could express a sum type like this if you were to use my
  pseudo-type-theory notation: T = A + B + C
  The set of values that are in T could be expressed with this logical
  predicate: T = { x | x ∈ A ⋃ B ⋃ C }

  (Strictly positive) inductive types 𝜇𝑎.𝜏 with their recursor fold𝜏′ 𝑎.𝜏 . The
  type variable 𝑎 must be strictly positive in 𝜏, a condition formally written
  as 𝑎 ∈++ 𝜏.
}

@block{@block-name{Fixed point - "mapped to itself"}
  The "fixed point property" and "fixed point theorems" refer to fundamental
  concepts in mathematics that identify conditions under which a function is
  guaranteed to have a fixed point. A fixed point of a function is an element
  that is mapped to itself by the function, i.e., for a function ff, an element
  x is a fixed point if f(x)=x.

  @block{@block-name{Fixed-point combinator / Y-combinator}
    "Implement recursion in a language without recursion"
    Essentials: Functional Programming's Y Combinator - Computerphile
    https://youtu.be/9T8A89jgeTI

    Fixed-point combinators in lambda calculus
    https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus
    Definition:
    Y = λf.(λx.f(x x) λx.f(x x))

    Y g = (λf.(λx.f(x x) λx.f(x x)))g   (by definition of Y)
        = (λx.g(x x) λx.g(x x))         (by β-reduction of λf: applied Y to g)
        = g(λx.g(x x) λx.g(x x))        (by β-reduction of λx: applied left function to right function)
        = g(Y g)                        (by second equality)
        = g(g(Y g))
        = g(g(g(Y g)))
        = g(...(g(Y g))...)

    YouTube: Encoding of Boolean values
    https://youtu.be/9T8A89jgeTI?t=217
    One is opposite of the other:
    TRUE  = λx.λy.x
    FALSE = λx.λy.y

    TODO Wikipedia: SKI combinator calculus
    https://en.wikipedia.org/wiki/SKI_combinator_calculus

    Bartosz Milewski: The Fall of the SKI Civilization
    https://bartoszmilewski.com/2020/09/06/the-fall-of-the-ski-civilization/
  }
}

@block{@block-name{Contrapositive}
  "If P, then Q" (symbolically represented as P→Q), the contrapositive of this
  statement is "If not Q, then not P" (symbolically, ¬Q→¬P).
}

@block{@block-name{Cantor's theorem}
  ∣A∣<∣P(A)∣
  The set of all possible subsets of A (i.e.the power set of A, denoted by 2^A
  or P(A)) has a strictly greater cardinality (size of a set, denoted by∣⋅∣)
  than the set A itself. I.e. there is no one-to-one correspondence (bijection)
  between the elements of a set and the elements of its power set.
  A = {1 2 3}
  P(A) = {{} 1 2 3 {1 2} {1 3} {2 3} {1 2 3}} - hash 2^3 = 8 elements
}

@block{@block-name{Possible interpretation of A × X -> Y}
  family of morphisms A -> Y indexed by the elements of X.
}


In the chapter "1. Exponentiation, surjectivity, and a fixed-point theorem" the paper explains what is a cartesian closed category:

"is meant a category C equipped with the following three kinds of right-adjoints: a right adjoint 1 to the unique C -> 1"

Here in this context the 1 is meant to be a category. (The C and 1 are written with bold font weight)

However on the page 7 the paper mentions "1 is a generator for C". (The 1 is written with normal font weight, the C is bold.) I assume that here the 1 is an object with a property of being a generator object. Can you explain me please what exactly that means?

