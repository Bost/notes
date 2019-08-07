### :tex :latex :document-processing
TeX processes layout. LaTeX processes content. LaTeX has a collection of TeX
macros and a program to process LaTeX documents, and because the plain TeX
formatting commands are elementary, it provides ready-made commands for
formatting and layout requirements e.g. chapter headings, footnotes,
cross-references, bibliographies.

### :tex :latex
- `\usepackage[T1]{fontenc}` - output oriented, i.e. what fonts to use for printing characters.
- `\usepackage[utf8x]{inputenc}` - allows to input accented characters directly from the keyboard.

### numeric tower - numbers:
Natural ℕ ⊆ Integer ℤ ⊆ Rational ℚ ⊆ Real ℝ ⊆ Complex ℂ ⊆ Quaternion ℍ

### structure type - record datatype composing a number of fields
Sum type (OCaml): Generalization of enumeration. Bring heterogeneous values
together into a common type

(OCaml) type export: concrete / abstract - for fine-grained data encapsulation control
concrete export - full type definition remains known: so, clients of the modules can build or examine values of this type.
abstract export - only the type name is known outside the module. It then becomes impossible, from the outside, to create or inspect values of this type.

### Pattern matching
- generalize the traditional case analysis construct
- examine and name data simultaneously

### CLOS-based object orientation

### Foreign Function Interface FFI a.k.a. Language bindings
using different languages in one programm;
e.g. Java Native Interface JNI / Java Native Access JNA; extern "C"

### TODO delimited continuations, module system / OCaml, hygienic macros, multiple values, threads

### WSL
Windows Subsystem for Linux 2 (WSL 2)

### Data structures (are / do):
- static (Algorithms - dynamic)
- reasoning by an induction: consider the base case(s) and the general.
- remove the notion of time. Change over time is one of the major sources of program complexity.
- declarative. Most of the algorithms are imperative. Under declarative approach you don't have to trace the flow of time through a data structure.

