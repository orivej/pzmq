(require '#:atdoc)

(atdoc:generate-html-documentation
 '(pzmq) #p"./doc/"
 :index-title "PZMQ"
 :heading "ZeroMQ 3.2+ Common Lisp bindings"
 :single-page-p t
 :include-internal-symbols-p nil)
