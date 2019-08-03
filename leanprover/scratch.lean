-- M-x lean-std-exe: C-c C-l

#eval 1 + 1

variables p q : Prop

example (hpq : p → q) (hnq : ¬q) : ¬p :=
assume hp : p,
show false, from hnq (hpq hp)

example (hp : p) (hnp : ¬p) : q := false.elim (hnp hp)
#check p
def x := false
#check x

#check eq.subst --

constants p q : Prop

theorem t1 : p → q → p := λ hp : p, λ hq : q, hp

#check t1

#print t1

universe u
inductive foo (a : α) : Sort u
| constructor₁ : Π (b : β₁), foo
| constructor₂ : Π (b : β₂), foo
