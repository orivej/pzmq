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
@end{section}

@begin[Middle-level API]{section}
@aboutfun{with-context}
@aboutfun{with-message}
@aboutfun{ctx-set-monitor}
@aboutfun{def-monitor-callback}
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
   ;; - context
   #:ctx-new
   #:ctx-set
   #:ctx-get
   #:ctx-destroy
   #:def-monitor-callback
   #:ctx-set-monitor
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
   ;; - device
   #:device

   ;; Lisp API:
   #:with-message
   #:*default-context*
   #:recv-string
   #:with-context
   #:with-socket
   #:with-sockets))
