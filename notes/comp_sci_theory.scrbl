#lang notes

# generate pdf: M-x org-latex-export-to-pdf


# https://en.wikipedia.org/wiki/List_of_mathematical_symbols_by_subject
# latexpreview / nolatexpreview C-c C-x C-l
# #+STARTUP: nolatexpreview
#+STARTUP: showeverything inlineimages nolatexpreview

@block{@block-name{Complexity classes & Complexity Zoo}
  [[https://complexityzoo.uwaterloo.ca/Complexity_Zoo][Complexity Zoo]]
  NP      - decision problems solvable by non-deterministic TM (Turing Machine) in polynomial time
  PSPACE  - decision problems solvable by     deterministic TM                  in polynomial space

  @block{@block-name{computational problem type: e.g. problems: decision, function, counting, optimization, promise, etc.}
  }

  @block{@block-name{model of computational: e.g. deterministic TM (Turing Machine), non-deterministic TM, boolean circuits, quantum TM, monotone circuits}
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
  [http://www.cl.cam.ac.uk/teaching/Lectures/funprog-jrh-1996/all.pdf]

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

@block{@block-name{Alpha α-conversion / α-equivalence of terms: Term are same up to a renaming of binding and bound vars}
}

@block{@block-name{Beta β-reduction (computational): Substitute var x with s in term t: (λx.t)s = [s/x]t}
  β- reduction in computation a.k. calculation rule: Simplify proofs by replacing formal params with terms.
  fst<g , h > = g
  snd<g , h > = h
  (λx.h)g = [g/x]h      - substitute variable x in the expression (i.e λ-term) h by the (i.e λ-term) g
  (λx.f)x = [x/x]f = f  - ? do nothing, i.e. void substitution ?

  @block{@block-name{Fixed Point Theorem: for each λ-term L there exists a λ-term M (a.k.a fixed point) such that LM =β M}
    =β   - β-conversion a.k.a β-equality
   }
}

@block{@block-name{TODO Eta η-extentionality principle}
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

@block{@block-name{Haskell / Agda comparison:}
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
  It it’s impossible to have a procedure that figures out whether an arbitrary
  program halts, it’s easy to show that it’s impossible to have a procedure that
  is a perfect recognizer for any overall run time property. A program that
  type-checks is guaranteed not to cause a run-time type-error. But since it’s
  impossible to recognize perfectly when programs won’t cause type-errors, it
  follows that the type-checker must be rejecting programs that really wouldn’t
  cause a type-error. The conclusion is that no type-checker is perfect—you can
  always do better!
}

@block{@block-name{Krakatoa and Jessie: verification tools for Java and C programs}
  Why3: platform for deductive program verification
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
  | Grammar | Languages              | Automaton                                       | Production rules (constraints)* | Examples[3]                                                     |
  ** Meaning of symbols:
  a = terminal
  A, B = non-terminal
  α, β, γ = string of terminals and/or non-terminals
  α, β = maybe empty
  γ = never empty
}

@block{@block-name{Indirect left recursion}
  [[https://en.wikipedia.org/wiki/Left_recursion][Wikipedia: Left Recursion]]
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
  @block{@block-name{DNF - Disjunctive Normal Form / Formula e.g. (A ∧ ¬B ∧ C) ∨ (¬D ∧ E ∧ F)}
  }

  @block{@block-name{CNF - Conjunctive Normal Form / Formula e.g. (A ∨ ¬B ∨ C) ∧ (¬D ∨ E ∧ F)}
  }
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
  [[https://youtu.be/9T8A89jgeTI][Essentials: Functional Programming's Y Combinator - Computerphile]]

  [[https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus][Fixed-point combinators in lambda calculus]]
  Definition:
  Y = λf.(λx.f(x x) λx.f(x x))

  Y g = (λf.(λx.f(x x) λx.f(x x)))g   (by definition of Y)
      = (λx.g(x x) λx.g(x x))         (by β-reduction of λf: applied Y to g)
      = g(λx.g(x x) λx.g(x x))        (by β-reduction of λx: applied left function to right function)
      = g(Y g)                        (by second equality)
      = g(g(Y g))
      = g(g(g(Y g))
      = g(...(g(Y g)...)

  [[https://youtu.be/9T8A89jgeTI?t=217][YouTube: Encoding of Boolean values]]
  One is opposite of the other:
  TRUE  = λx.λy.x
  FALSE = λx.λy.y

  TODO [[https://en.wikipedia.org/wiki/SKI_combinator_calculus][Wikipedia: SKI combinator calculus]]
  [[https://bartoszmilewski.com/2020/09/06/the-fall-of-the-ski-civilization/][Bartosz Milewski: The Fall of the SKI Civilization]]
}

@block{@block-name{Self-balancing binary search trees}
 for basic operations
 - AVL-Tree (Adelson-Velsky-Landis)
 - RB-Tree (Red-Black) [[https://en.wikipedia.org/wiki/Red%E2%80%93black_tree][Wikipedia: Red-Black Tree]]
}

@block{@block-name{Endophora - Linguistics}
  [[https://en.wikipedia.org/wiki/Endophora][Wikipedia: Endophora]]

  @block{@block-name{Subcategories of Endophoras: φέρω (phérō, "I carry")}
    @block{@block-name{Anaphora "carrying back" ἀνά (aná, "up")}
      [[https://en.wikipedia.org/wiki/Anaphora_(linguistics)][Wikipedia: Anaphora (linguistics)]]
      - Usage of an expression that depends upon an antecedent (previous)
        expression.
      - "Sally arrived, but nobody saw her" - 'her' is an anaphor (= anaphoric
        term)
    }

    @block{@block-name{Cataphora "carrying forward" κατά (kata, "downwards")}
      [[https://en.wikipedia.org/wiki/Cataphora][Wikipedia: Cataphora]]
      - Usage of an expression that depends upon a postcedent (later) expression.
      - "If you want some, there are cookies in the kitchen" - 'some' is a
        cataphor (= cataphoric expression)
    }

    @block{@block-name{Self-reference}
      [[https://en.wikipedia.org/wiki/Self-reference][Wikipedia: Self-reference]]
    }
  }
}

@block{@block-name{Anaphoric Macro}
  [[https://en.wikipedia.org/wiki/Anaphoric_macro][Wikipedia: Anaphoric Macro]]
  - captures some form supplied to the macro which may be referred to by an
    anaphor (i.e. expression that depends on a previous expression)

  - Example (ANSI Common Lisp; works also in sbcl - Steel Bank Common Lisp ):
    Sum the value of non-nil elements, where 'it' refers to the values of
    elements that do not equal nil:
    #+BEGIN_SRC lisp
    (loop for element in '(nil 1 nil 2 nil nil 3 4 6)
          when element sum it)
    ;; ⇒ 16
    #+END_SRC

    #+BEGIN_SRC lisp
    (defmacro aif (test-form then-form &optional else-form)
      `(let ((it ,test-form))
             (if it ,then-form ,else-form)))

    (aif (+ 2 7)
      (format nil "~A does not equal NIL." it)
      (format nil "~A does equal NIL." it))
    #+END_SRC
}

@block{@block-name{Method Chaining}
  #+BEGIN_SRC javascript
  somethings
    .filter(x => x.count > 10)
    .sort((a, b) => a.count - b.count)
    .map(x => x.name)
  #+END_SRC
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
  @block{@block-name{Denotational Semantics}
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
  facilitate it. Apparently even excellent programmers and great erudites have
  been failing at this task. One of the most extreme examples is the code from
  Donald Knuth’s The Art of Computer Programming. Written in the MIX assembly,
  the code only hides the conceptual structure of presented solutions from the
  reader.
}

@block{@block-name{Scheme Lisp}
  Scheme as a meta-language for itself – this technique, called meta-circular
  evaluation, has been explored in the grand book Structure and Interpretation
  of Computer Programs. This idea was at the heart of the seminal paper
  Recursive Functions of Symbolic Expressions and their Computation by Machine
  by John McCarthy, which gave birth to the predecessor of Scheme called Lisp.

  A nesting of pairs whose last right value is not the empty list is called
  improper list

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
