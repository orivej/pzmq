(in-package #:pzmq)

(defmacro with-message (name &body body)
  "Initialize and free ZMQ message around body."
  `(with-foreign-object (,name '(:struct %msg))
     (msg-init ,name)
     (unwind-protect (progn ,@body)
       (msg-close ,name))))

(defun recv-string (socket &key dontwait (encoding cffi:*default-foreign-encoding*))
  "Receive a message part from a socket as a string."
  (with-message msg
    (msg-recv msg socket :dontwait dontwait)
    (values
     (foreign-string-to-lisp (msg-data msg) :count (msg-size msg) :encoding encoding)
     (getsockopt socket :rcvmore))))

(defvar *default-context* nil
  "Implicit context from @fun{WITH-CONTEXT} for @fun{WITH-SOCKET}.")

(defmacro with-context (name-and-options &body body)
  "Initialize and destroy ZMQ context around body.

Use NIL for @em{anonymous context}, stored in @variable{*DEFAULT-CONTEXT*}.

Omit @fun{WITH-CONTEXT} altogether, and @fun{WITH-SOCKET} will establish it by itself.

Note: unwind-protected @fun{CTX-DESTROY} will not return until all governed sockets have sent all queued messages, unless they limited their wait time with :LINGER socket parameter.
@arg[name-and-options]{name | (name options)}
@arg[name]{name | NIL}
@arg[options]{:io-threads INT :max-sockets INT; as per @fun{CTX-SET}}"
  (let ((name name-and-options)
        (options nil))
    (when (listp name)
      (setf name (car name-and-options)
            options (cdr name-and-options)))
    `(let* ((*default-context* (ctx-new))
            ,@(if name `((,name *default-context*))))
       (unwind-protect
            (progn
              ,(when options
                 `(ctx-set *default-context* ,@options))
              ,@body)
         (ctx-destroy ,(or name '*default-context*))))))

(defmacro with-socket (name-and-context type-and-options &body body)
  "Initialize and close ZMQ socket around body.  Type is one of the types accepted by @fun{SOCKET}.  Options are passed to @fun{SETSOCKOPT} one by one.

When TYPE is :SUB, and :SUBSCRIBE is not given in OPTIONS, imply subscribe to all.  If this is undesirable, provide :SUBSCRIBE NIL.

When context is not specified, it either comes from surrounding @fun{WITH-CONTEXT} or @fun{WITH-SOCKET} in @variable{*DEFAULT-CONTEXT*}, or is established by this @fun{WITH-SOCKET} and stored in @variable{*DEFAULT-CONTEXT*} for the timespan of this block.
@arg[name-and-context]{name | (name context)}
@arg[type-and-options]{type | (type :option1 value1 :option2 value2 ...)} "
  (let* ((context-p (listp name-and-context))
         (options-p (listp type-and-options))
         (name (if context-p (car name-and-context) name-and-context))
         (context (if context-p (cadr name-and-context) '*default-context*))
         (type (if options-p (car type-and-options) type-and-options))
         (options (when options-p (cdr type-and-options)))
         (implicit-context (gensym (string '#:implicit-context))))
    (when (and (eq type :sub) (not (member :subscribe options)))
      (setf options (list* :subscribe "" options)))
    `(let ((,implicit-context (not (or ,context-p *default-context*))))
       (when ,implicit-context (setf *default-context* (ctx-new)))
       (unwind-protect
            (let ((,name (socket ,context ,type)))
              (unwind-protect
                   (progn
                     ,@(loop
                         for (option value) on options by #'cddr
                         collect `(setsockopt ,name ,option ,value))
                     ,@body)
                (close ,name)))
         (when ,implicit-context
           (ctx-destroy (prog1 *default-context*
                          (setf *default-context* nil))))))))

(defmacro with-sockets ((&rest socket-definitions) &body body)
  "Nest multiple sockets."
  (loop for definition in (reverse socket-definitions)
        for form = `(with-socket ,@definition ,@body)
        then `(with-socket ,@definition ,form)
        finally (return form)))

(defmacro with-poll-items (name (&rest items) &body body)
  "Prepare POLLITEM array in NAME.  Only ZeroMQ sockets are supported.

Without parrenthes, an item indicates subscription to all events.
@arg[items]{(item ...)}
@arg[item]{name | (name [:pollin] [:pollout])}"
  (let ((nitems (length items)))
    `(with-foreign-object (,name '(:struct pollitem) ,nitems)
       ,@(loop
           for item in items
           for offset from 0
           for (%socket . %events) = (if (atom item) (list item) item)
           when (zerop (length %events))
           do (setf %events (list :pollin :pollout))
           collect `(with-foreign-slots
                        ((socket events)
                         (mem-aptr ,name '(:struct pollitem) ,offset)
                         (:struct pollitem))
                      (setf socket ,%socket
                            events ',%events)))
       (let ((,name (cons ,name ,nitems)))
         ,@body))))

(defun revents (items subscript)
  "Return a list of events - :pollin, :pollout or both - that happened to an indicated item, counting from 0.
@return{([:pollin] [:pollout])}"
  (assert (< -1 subscript (cdr items)))
  (let ((item-ptr (mem-aptr (car items) '(:struct pollitem) subscript)))
    (foreign-slot-value item-ptr '(:struct pollitem) 'revents)))
