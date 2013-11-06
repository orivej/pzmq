(defpackage #:pzmq
  (:documentation "ZeroMQ 3.2+ bindings.

Consult @a[http://api.zeromq.org/3-3:zmq]{official C API reference} first.

@begin[Top-level API]{section}
@aboutfun{with-socket}
@aboutfun{with-sockets}
@aboutfun{bind}
@aboutfun{connect}
@aboutfun{recv-string}
@aboutfun{send}
@aboutfun{with-poll-items}
@aboutfun{poll}
@aboutfun{revents}
@end{section}

@begin[Middle-level API]{section}
@aboutfun{with-context}
@aboutfun{with-message}
@end{section}

@begin[Low-level API]{section}
@aboutfun{ctx-new}
@aboutfun{ctx-set}
@aboutfun{ctx-get}
@aboutfun{ctx-destroy}
@aboutfun{error}
@aboutfun{strerror}
@aboutfun{version}
@end{section}
@section[Other]{}")
  (:use #:cl #:cffi)
  (:shadow #:close)
  (:export
   ;; C API:
   ;; - misc
   #:version
   ;; - error
   #:errno
   #:strerror
   #:c-error
   #:libzmq-error
   #:eaddrinuse
   #:eaddrnotavail
   #:eagain
   #:efault
   #:efsm
   #:eintr
   #:einval
   #:emfile
   #:emthread
   #:enocompatprot
   #:enodev
   #:enomem
   #:enotsock
   #:enotsup
   #:eprotonosuppo
   #:eterm
   ;; - context
   #:ctx-new
   #:ctx-set
   #:ctx-get
   #:ctx-destroy
   ;; - message
   #:%msg
   #:msg-init
   #:msg-init-size
   #:msg-init-data
   #:msg-send
   #:msg-recv
   #:msg-close
   #:msg-data
   #:msg-size
   #:msg-more
   #:msg-get
   #:msg-set
   #:msg-copy
   #:msg-move
   ;; - socket
   #:socket
   #:close
   #:getsockopt
   #:setsockopt
   #:bind
   #:connect
   #:send
   #:poll
   ;; - device
   #:device

   ;; Lisp API:
   #:with-message
   #:*default-context*
   #:*restart-interrupted-calls*
   #:recv-string
   #:with-context
   #:with-socket
   #:with-sockets
   #:with-poll-items
   #:revents))
