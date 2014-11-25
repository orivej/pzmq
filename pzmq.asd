(asdf:load-system '#:cffi-grovel)

(asdf:defsystem pzmq
  :description "ZeroMQ 3.2+ bindings."
  :encoding :utf-8
  :author "Orivej Desh <orivej@gmx.fr>"
  :licence "Unlicense" ; http://unlicense.org/UNLICENSE
  :depends-on (cffi)
  :serial t
  :components ((:file "package")
               (cffi-grovel:grovel-file "grovel") ; error constants
               (:file "c-api") ; C API and obvious wrappers
               (:file "lisp-api") ; more elaborate wrappers
               )
  :in-order-to ((test-op (load-op :pzmq-test)))
  :perform (test-op (o c) (symbol-call :fiveam :run! :pzmq)))

(asdf:defsystem pzmq-compat
  :depends-on (pzmq)
  :encoding :utf-8
  :components ((:file "compat")))

(asdf:defsystem pzmq-test
  :depends-on (pzmq fiveam let-plus bordeaux-threads)
  :encoding :utf-8
  :components ((:file "tests")))

(asdf:defsystem pzmq-examples
  :depends-on (pzmq split-sequence iterate local-time bordeaux-threads)
  :encoding :utf-8
  :components ((:file "examples")))
