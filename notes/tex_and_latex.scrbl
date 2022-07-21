#lang notes

#+title: TeX and LaTex

@block{@block-name{TeX and LaTex}
  LaTeX is a document preparation system, a package containing a set of macros,
  written by Leslie Lamport, built on top of TeX.

  TeX is not just a typesetter, it is a typesetter with an embedded scripting
  language, whose commands configure and control the behaviour of the typesetter.
  LaTeX and Plain TeX are two different customised typesetting environments, with
  their own commands and conventions, based on TeX (TeX is a "formatting system")

  AUCTeX - an extensible package for writing and formatting TeX files in Emacs

  The DSLs of TeX and LaTeX are too-limiting as programming languages. Better alternatives:
  - [[https://docs.racket-lang.org/scribble/index.html][Scribble: The Racket Documentation Tool]]
  - [[https://www.nongnu.org/skribilo/][Skribilo: The Ultimate Document Programming Framework]]
  See [[id:f1109efa-e1ea-4605-8850-19b3b11f1cec][Better Ideas]] in general

  Examples:
  # LaTeX to ODT (OpenDocument Text / LibreOffice) conversion
  pandoc -s document.tex -o document.odt
  #
  # install own latex package system-wide
  sudo cp <package>.sty /usr/share/texmf-texlive/tex/latex/base/
  sudo mktexlsr
  #
  # 1. install latex: disk space requirements:
  sudo apt install texlive-base              # 136 MB
  sudo apt install texlive-latex-recommended # 177 MB
  sudo apt install texlive                   # 240 MB
  sudo apt install texlive-latex-extra       # 404 MB
  sudo apt install texlive-full              # 4714 MB
  #
  # 2. create example document
  echo "\
          Hello world \LaTeX
          " > hello-world.tex
  #
  # 3. compile to pdf and view
  pdflatex hello-world.tex; and evince hello-world.pdf
  #
  # install own latex package (sty-file) locally
  mkdir -p ~/texmf/tex/latex/<package>
  texhash ~/texmf
  #
  # latex macros https://youtu.be/j--6zhiWDJ8

  #+LATEX: % generate pdf: M-x org-latex-export-to-pdf
    (defn foo [] (println "foo"))

    #!/usr/bin/bash
    for (( i=0; i != 10; i=i+1)); do
      echo $i
    done
}
