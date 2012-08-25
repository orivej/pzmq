(asdf:defsystem pzmq
  :description "ZeroMQ 3.2+ bindings."
  :author "Orivej Desh <orivej@gmx.fr>"
  :licence "MIT"
  :depends-on (cffi)
  :serial t
  :components ((:file "package")
               (:file "c-api") ; C API and obvious wrappers
               (:file "lisp-api") ; more elaborate wrappers
               ))

(asdf:defsystem pzmq-compat
  :depends-on (pzmq)
  :components ((:file "compat")))

(asdf:defsystem pzmq-test
  :depends-on (pzmq fiveam let-plus bordeaux-threads)
  :components ((:file "tests")))

(asdf:defsystem pzmq-examples
  :depends-on (pzmq split-sequence iterate local-time bordeaux-threads)
  :components ((:file "examples")))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :pzmq))))
  (asdf:load-system :pzmq-test)
  (funcall (intern (string '#:run!) :fiveam) :pzmq))
