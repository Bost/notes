function crn
    set d $dev/notes
    cheat-grep $argv $d/category-theory.md \
                     $d/computer-sciences.md \
                     $d/logics.org \
                     $d/math.org \
                     $d/notes.org \
                     $d/math-structures.md
end
