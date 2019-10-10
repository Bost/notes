#!/usr/bin/env fish

set jarfile clojupyter-standalone.jar
set ident clojupyter-logic

set cmd clj -A:depstar -m hf.depstar.uberjar $jarfile
echo $cmd
eval $cmd
if test $status = 0
    set cmd clj -m clojupyter.cmdline remove-install $ident
    echo $cmd
    eval $cmd
    if test $status = 0
        set cmd clj -m clojupyter.cmdline install --ident $ident --jarfile $jarfile
        echo $cmd
        eval $cmd
    end
end
