#lang notes

@block{@block-name{Parsing stages}
  (sometimes divided into two) stages:
  1. Lexical analysis
    Flex - tool for generating scanners; recognition of lexical patterns
    https://cobweb.cs.uga.edu/~kochut/teaching/x570/tools/flex.pdf
  2. Parsing itself
    https://www.gnu.org/software/bison
    the general-purpose parser generator that converts an annotated context-free
    grammar into a deterministic LR or generalized LR (GLR) parser employing
    LALR(1) parser tables.
}

@block{@block-name{Parsec}
  - Parser combinator library
  - Combine small parsing functions to build more sophisticated parsers
  - Can perform both lexical analysis and parsing
}
