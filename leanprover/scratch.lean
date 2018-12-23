#eval 1 + 1

variables p q : Prop
-- BEGIN
example (hpq : p → q) (hnq : ¬q) : ¬p :=
assume hp : p,
show false, from hnq (hpq hp)

example (hp : p) (hnp : ¬p) : q := false.elim (hnp hp)
#check p
def x := false
#check x

#check eq.subst --

-- END
