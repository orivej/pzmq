(defpackage #:pzmq
  (:documentation "ZeroMQ 3.2+ bindings.

Consult @a[http://api.zeromq.org/4-0:zmq]{official C API reference} first.

@begin[Top-level API]{section}
@aboutfun{with-socket}
@aboutfun{with-sockets}
@aboutfun{bind}
@aboutfun{connect}
@aboutfun{recv-string}
@aboutfun{recv-octets}
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
   #:ehostunreach
   #:eintr
   #:einval
   #:emfile
   #:emthread
   #:enocompatproto
   #:enodev
   #:enoent
   #:enomem
   #:enotsock
   #:enotsup
   #:eprotonosupport
   #:eterm
   ;; - context
   #:ctx-new
   #:ctx-set
   #:ctx-get
   #:ctx-shutdown
   #:ctx-destroy #:ctx-term
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
   #:proxy

   ;; Lisp API:
   #:with-message
   #:with-messages
   #:*default-context*
   #:*restart-interrupted-calls*
   #:recv-string
   #:recv-octets
   #:with-context
   #:with-socket
   #:with-sockets
   #:with-poll-items
   #:revents))
