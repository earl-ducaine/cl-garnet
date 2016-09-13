(load "garnet-prepare-compile.lisp")
(load "garnet-loader.lisp")
(load "garnet-compiler.lisp")
#+clisp (ext:quit)
#+cmu (quit)
#+sbcl (sb-ext:quit)
#+allegro (excl:exit)

