# json2dir-commonlisp

json2dir in Common Lisp (SBCL, sb-posix only, hand-written JSON parser): creates the directory tree a JSON document describes in the current directory.

    sbcl --non-interactive --eval '(with-compilation-unit () (load "json2dir.lisp"))' --eval '(sb-ext:save-lisp-and-die "out/json2dir" :executable t :toplevel (function json2dir:main) :save-runtime-options t)'
    out/json2dir < document.json
