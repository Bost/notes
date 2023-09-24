# testing matching of 'x-x' sexp with ripgrep
# See https://docs.rs/regex/1.9.5/regex/#syntax

x-x
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

[x-x] x-x (x-x)
