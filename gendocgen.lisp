(require '#:pzmq)
(load "gendocs.lisp")

(defun toplevel ()
  (require '#:pzmq)
  (setf *default-pathname-defaults*
        (make-pathname :name nil :type nil :defaults *core-pathname*))
  (load "gendocs.lisp"))

(sb-ext:save-lisp-and-die
 "gendocs"
 :toplevel #'toplevel
 :executable t
 :compression t)
