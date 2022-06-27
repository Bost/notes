#lang notes

@block{@block-name{Logic}
  Study of the principles of inference and reasoning. It's not about "what is
  true".

  Inductive inference:
  We try to inference something about a phenomenon we don't understand. What
  properties it might have?

  Deductive inference:
  Something is true. What else can we derive from it?

  Inference rules:
  $$ \infer{Conclusion}{Premise} $$

  From the fact(s) / premise above the line we can deduce / assert / derive the
  fact / conclusion below the line. It may contain are schematic variables.

  TODO List of Substructural logics

  Examples-of-Logics:
  | Logical Judgement   | Branch of Logic      | Computation phenomenon                                      |
  |---------------------+----------------------+-------------------------------------------------------------|
  |                     | Intuitionistic Logic | Curry-Howard Correspondance                                 |
  | K knows A           | Epistemic Logic      | Distributed Computing                                       |
  | A is true at time t | Temporal Logic       | Reactive Programming (partial evaluation)                   |
  | A is resource       | Linear Logic         | Concurrent Computation (\pi calculus)                       |
  | A is possible       | Monadic Lax Logic    | Generic effects (monads, state, exception)                  |
  | A is valid          | Modal Logic          | Runtime code generation, code & eval constructs in the lang |
  |                     |                      | Different modes of truth                                    |

  TODO difference between branches of logic
}

@block{@block-name{Intuitionistic logic}
  Philip Wadler: A taste of linear logic
  [https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf]
  based on Girard’s Logic of Unity ([[https://news.ycombinator.com/item?id=17641476][discussion]])

  In logic: terms encode proofs, In programming: terms are programming language
  a.k.a Traditional a.k.a Constructive Logic: endless resources, no need for construction / destruction

  Expressions:
  | Expression               | Name / Logical connective  | Explanation / Noqte                                      |
  |--------------------------+----------------------------+----------------------------------------------------------|
  | $A,B,C$                  | propositions               |                                                          |
  | $X$                      | propositional constant     |                                                          |
  | $\Gamma, \Delta, \Theta$ | sequences of assumptions   | Gama, Delta, Theta                                       |
  | $A \rightarrow B$        | implication in proposition | proposition $A$ implies proposition $B$                  |
  | $\Gamma \vdash A$        | implication in judgement   | from assumptions \Gamma one can conclude proposition $A$ |
  | <vertical line>          | implication in rule        |                                                          |
  | $A \times B$             | product of propositions    | $A$ and $B$; conjunction                                 |
  | $A + B$                  | sum of propositions        | $A$ or $B$; disjunction                                  |

  \begin{tabbing}
  Grammar    \hspace{7em} \= $A, B, C ::= X \mid A \rightarrow B \mid A \times B \mid A + B$ \\
  Rules                   \> Exchange, Contraction, Weakening, etc.                          \\
  \end{tabbing}
}

@block{@block-name{Intuitionistic terms}
  Expressions:
  | Expression  | Name / Logical Connective |
  |-------------+---------------------------+
  | $s,t,u,v,w$ | terms                     |
  | $x,y,z$     | variables for terms       |
}

@block{@block-name{Linear logic}
  notions of:
  - state
  - limited resources: construction / descruction

  ? Unique pointers in C\texttt{++} ?
  "Stuff moved from place to place and it cannot be used twice, only once"\\

  Grammar:
  \begin{tabbing}
  Grammar    \hspace{7em} \= $A, B, C ::= X \mid A \multimap B \mid A \otimes B \mid A \with B \mid A \oplus B \mid \with A$ \\
  Rules                   \> ???                          \\
  \end{tabbing}

  | Expression               | Name                                           | How to read it                                         |
  |--------------------------+------------------------------------------------+--------------------------------------------------------|
  | $A,B,C$                  | propositions                                   |                                                        |
  | $X$                      | propositional constant                         |                                                        |
  | $\Gamma, \Delta, \Theta$ | sequences of assumptions                       | Gama, Delta, Theta                                     |
  | $\langle A \rangle$      | linear assumption: 1 occurence of $A$          | \langle\rangle can appear only to the left of $\vdash$ |
  | $[\Gamma]$               | seqs of 0 or more intuitionistic assumptions   | $[]$ can appear only to the left of $\vdash$           |
  | $[A]$                    | linear assumption: unlimited occurences of $A$ | $[]$ can appear only to the left of $\vdash$           |
  | $\Gamma \vdash A$        | turnstile, implication in judgement            | from assump Gamma one can conclude prop $A$            |
  | $A \multimap B$          | lollipop                                       | consuming $A$ yields $B$                               |
  | $A \otimes B$            |                                                | both $A and $B$; tensor                                |
  | $A \with B$              |                                                | choose from $A$ and $B$; 'with'                        |
  | $A \oplus B$             |                                                | either $A$ or $B$; disjunction                         |
  | $\oc A$                  |                                                | of course $A$; bang / pling                            |

  Embedding intuitionistic logic into linear logic: \\
  $A \rightarrow B = \oc A \vdash B$ \\
  $A \times B = A \with B$ alternativelly $A \times B = \oc A \otimes \oc B$ \\
  $A + B = \oc A \oplus \oc B$
}

@block{@block-name{Propositional Logic:}
  AND, OR, NOT, IF-THEN, IS-EQUIVALENT-TO

  A Pamphlet against R
  https://github.com/panicz/pamphlet
  Atomic formulas - simplest units of propositional logic. I.e propositions that
  aren’t analyzed in simpler terms, and that can only be asserted or rejected.

  Propositional variables: p, q, r, ... standing for atomic formulas.
  Junctions of expressions: φ ∨ ψ, φ∧ψ, φ ⇒ ψ, φ ≡ ψ, ¬φ. (Compound expressions)

  | Disjunction | φ or ψ                    |
  | Conjunction | φ and ψ                   |
  | Implication | if φ then ψ               |
  | Equivalence | φ if and only if ψ        |
  | Negation    | it is not the case that φ |

  The semantics of propositional logic determines the logical value (i.e.
  truth or falsehood) of each formula with respect to some given valuation,
  i.e. a mapping from propositional variables to logical values.

  A formula is satisfiable if there exists a valuation under which it is true.
}

@block{@block-name{Predicate Logic}
  Dependence on free variables, has Universal quantifiers, variables, functions
}

@block{@block-name{Higher order logic}
}

@block{@block-name{Temporal logic}
  Temporal Logic of Actions TLA^{+} developed by Leslie Lamport:
  It combines temporal logic with a logic of actions and describes behaviours of
  concurrent systems.
  Thinking Above the Code
  [https://www.youtube.com/watch?v=-4Yp3j_jk8Q&t]
  The TLA^{+} Video Course
  [http://lamport.azurewebsites.net/video/videos.html]

  Usefull models:
  - Function map input to output, can be represented as a set of ordered pairs
    thus runs in no-time.
  - Operating System runs forever (see sequence of states) does no in/out
    mapping

  sequence of states == behavior
  state: assignment of values to variables
  programm is modeled by a set of behaviors representing all possible executions

  Theorem:
  intersection of behaviors satisfying properties:
  Liveness L: a complete behavior is needed
  Safety S: e.g. partial correctness

  Specification:
  a set of possible init states (described using math!): uprimed variables
  next state relation: describes all possible successor states using math(!):
  primed variables

  Nondeterminism:
  multiple next states possible

  Formal specification:
  needed only to apply tools: TLA^{+}: Temporal Logic is for liveness
  Model checking of TLA^{+} spec

  Informal Specification:
  e.g. pretty-printing can't be exactly specified
  Set of rules/requiremens/axioms is usually a bad spec: consequesces of rules
  are hard to understand
  }
