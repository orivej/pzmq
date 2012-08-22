(require '#:pzmq)
(load "gendocs.lisp")
(sb-ext:save-lisp-and-die
 "gendocs"
 :toplevel (lambda () (require '#:pzmq) (load "gendocs.lisp"))
 :executable t
 :compression t)
