(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload :hyperspec :silent t)
(sb-ext:save-lisp-and-die #P"clhs" :toplevel #'hs::main :executable t)
