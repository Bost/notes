function crn --description="Search through the notes"
    set pth $dev/notes
    set files $files $pth/category-theory.org
    set files $files $pth/comp-sci-theory.org
    set files $files $pth/comp-sci-engineering.org
    set files $files $pth/logics.org
    set files $files $pth/math.org
    set files $files $pth/notes.org
    set files $files $pth/math-structures.org
    cheat-grep --grep-args="$argv" --files="$files"
end
