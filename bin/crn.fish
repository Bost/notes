function crn
    set pth $dev/notes
    set files $files $pth/category-theory.md
    set files $files $pth/computer-sciences.md
    set files $files $pth/logics.org
    set files $files $pth/math.org
    set files $files $pth/notes.org
    set files $files $pth/math-structures.md
    cheat-grep --grep-args="$argv" --files="$files"
end
