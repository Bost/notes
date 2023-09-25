#lang notes

@block{@block-name{}

# testing matching of 'x-x' sexp with ripgrep
# See https://docs.rs/regex/1.9.5/regex/#syntax
# Exclude testfile.scm so that `make all` i.e. `raco setup notes` doesn't fail.


x-x
x-x1
x-x!
?x-x
x-x x-x
x-x zzz x-x
xx-xx
  x-x
x-x  
xxxz
zxxx
zxxxz
zzz/x-x
zzz\\x-x
x-x/zzz
zzz/x-x/zzz
aaaaa x-x bbbbb
aaaaa x-x bbbbb
aaaaa -x-x- bbbbb
aaaaa yyy-x-x bbbbb
aaaaa x-x-zzz bbbbb
aaaaa yyy-x-x-zzz bbbbb

(x-x)
(x-x!)
(?x-x)
(x-x) (x-x)
(x-x) zzz (x-x)
(xx-xx)
  (x-x)
(x-x)  
(xxxz)
(zxxx)
(zxxxz)
(zzz/x-x)
(zzz\\x-x)
(x-x/zzz)
(zzz/x-x/zzz)
aaaaa (x-x) bbbbb
aaaaa (-x-x-) bbbbb
aaaaa (yyy-x-x) bbbbb
aaaaa (x-x-zzz) bbbbb
aaaaa (yyy-x-x-zzz) bbbbb

[x-x]
[x-x!]
[?x-x!]
[x-x] [x-x]
[x-x] zzz [x-x]
[xx-xx]
  [x-x]
[x-x]  
[xxxz]
[zxxx]
[zxxxz]
[zzz/x-x]
[zzz\\x-x]
[x-x/zzz]
[zzz/x-x/zzz]
aaaaa [x-x] bbbbb
aaaaa [-x-x-] bbbbb
aaaaa [yyy-x-x] bbbbb
aaaaa [x-x-zzz] bbbbb
aaaaa [yyy-x-x-zzz] bbbbb

{x-x}
{x-x!}
{?x-x}
{x-x} {x-x}
{x-x} zzz {x-x}
{xx-xx}
{x-x}
{x-x}  
{xxxz}
{zxxx}
{zxxxz}
{zzz/x-x}
{zzz\\x-x}
{x-x/zzz}
{zzz/x-x/zzz}
aaaaa {x-x} bbbbb
aaaaa {-x-x-} bbbbb
aaaaa {yyy-x-x} bbbbb
aaaaa {x-x-zzz} bbbbb
aaaaa {yyy-x-x-zzz} bbbbb


[x-x] x-x (x-x) {x-x}
(   x-x   )
}
