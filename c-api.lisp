(in-package #:pzmq)

(define-foreign-library libzmq
  (unix (:or "libzmq.so.3.0.0" "libzmq.so.3" "libzmq"))
  (t (:default "libzmq")))

(use-foreign-library libzmq)

;;; Misc

(defcfun ("zmq_version" %version) :void
  "Report ØMQ library version. "
  (major :pointer)
  (minor :pointer)
  (patch :pointer))

(defun version ()
  "@return{(major minor patch)}

Report ØMQ library version."
  (with-foreign-objects ((major :int) (minor :int) (patch :int))
    (%version major minor patch)
    (mapcar (lambda (ptr) (mem-ref ptr :int))
            (list major minor patch))))

;;; Error

(defcfun ("zmq_errno" %errno) :int
  "Retrieve value of errno for the calling thread.")

;; TODO below is not portable, it only works on Linux due to GLIBC
;; hack to return thread local address from dlsym("errno"). Can return
;; global address on other systems. Should either use implementation
;; specific conditions like #+sbcl (sb-alien:get-errno), or use a
;; library like IOLIB or OSICAT for this

(defcvar errno :int)

(defvar *restart-interrupted-calls* t
  "When blocking ZMQ call returns with EINTR automatically retry it,
instead of signaling a condition.

Explanation: Every time garbage collection happens in implementation
that use signals to stop threads (like SBCL), every ZMQ blocking
ZMQ call, will error out with EINTR on every GC.

When this variable is non-NIL, PZMQ will retry the call, as if
you had selected CONTINUE restart.

Note that EINTR will also be returned by any other interruptions
such as attaching a debugger to a thread, or pressing Ctrl-C.

If you would like to terminate your ZMQ call in these cases, then
rebind *RESTART-INTERRUPTED-CALLS*, and have a HANDLER-BIND set it to
NIL on these special cases.

Then at a lower level, ignore EINTR errors. It is important to use
HANDLER-BIND and not HANDLER-CASE, because we want the ZMQ function
being interrupted to return EINTR, performing any necessary cleanups,
using HANDLER-CASE or using non-local exit from HANDLER-BIND will
accomplish its task, but without letting ZMQ call properly cleanup
after itself.")

(defun errno ()
  "Retrieve value of errno for the calling thread. @see{STRERROR}"
  errno)

(defcfun ("zmq_strerror" %strerror) :string
  "Get ØMQ error message string."
  (errnum :int))

(defun strerror (&optional (errnum (errno)))
  "Get ØMQ error message string."
  (%strerror errnum))

(defun c-error-keyword (errno)
  (foreign-enum-keyword 'c-errors errno :errorp nil))

(define-condition c-error (error)
  ((errno :initarg :errno :reader c-error-errno))
  (:report (lambda (c stream)
             (let* ((errno (c-error-errno c))
                    (error-name (c-error-keyword errno)))
               (format stream "C error ~:@(~a~): ~a."
                       (or error-name errno) (strerror errno))))))

(define-condition libzmq-error (c-error) ())

(defmacro define-conditions (parent &rest children)
  `(progn ,@(loop for child in children
                  collect `(define-condition ,child (,parent) ()))))
;;; Generate from "grovel.lisp", export in "package.lisp"
(define-conditions libzmq-error eaddrinuse eaddrnotavail eagain efault efsm eintr einval emfile emthread enocompatprot enodev enomem enotsock enotsup eprotonosuppo eterm)


(defun libzmq-error-condition (errno)
  (let ((error-keyword (c-error-keyword errno)))
    (if (not error-keyword)
        'libzmq-error
        (intern (symbol-name error-keyword) #.*package*))))

(defmacro with-c-error-check ((kind &optional allow-restart-p) &body body)
  (assert (member kind '(:int :pointer)))
  (let ((ret (gensym (symbol-name '#:ret)))
        (err (gensym (symbol-name '#:err))))
    `(loop
       (setf errno 0)
       (let ((,ret (progn ,@body)))
         (if ,(case kind
                (:int `(minusp (the fixnum ,ret)))
                (:pointer `(null-pointer-p ,ret)))
             (unless (and ,allow-restart-p *restart-interrupted-calls*
                          (= #.(foreign-enum-value 'c-errors :eintr) errno))
               (let ((,err (make-condition (libzmq-error-condition errno) :errno errno)))
                 ,(if allow-restart-p
                      `(cerror "Retry ZMQ call" ,err)
                      `(error ,err))))
             (return ,ret))))))

(defmacro defcfun* (name (return-type &optional allow-restart) &body args)
  "Simple wrapper for DEFCFUN and DEFUN around WITH-POSIX-ERROR-CHECK."
  (assert (stringp (car args))) ; required docstring
  (let ((internal (intern (concatenate 'string "%" (symbol-name name))))
        (c-name (substitute #\_ #\- (format nil "zmq_~(~a~)" name)))
        (lambda-list (loop for form in (cdr args) collect (car form)))
        (docstring (car args)))
    `(progn
       (defcfun (,c-name ,internal) ,return-type
         ,@args)
       (defun ,name ,lambda-list ,docstring
         (with-c-error-check (,return-type ,allow-restart)
           (,internal ,@lambda-list))))))

;;; Context

(defcfun ("zmq_ctx_new" ctx-new) :pointer
  "Create new ØMQ context.
@see{CTX-DESTROY}")

(defcfun ("zmq_ctx_set" %ctx-set) :int
  "Set context options."
  (context :pointer)
  (option-name :int)
  (option-value :int))

(defconstant +io-threads+ 1)
(defconstant +max-sockets+ 2)

(defun ctx-set (context &key (io-threads 1 io-threads-p) (max-sockets 1024 max-sockets-p))
  "Set context options."
  (when io-threads-p (assert (zerop (%ctx-set context +io-threads+ io-threads))))
  (when max-sockets-p (assert (zerop (%ctx-set context +max-sockets+ max-sockets)))))

(defcfun ("zmq_ctx_get" %ctx-get) :int
  "Get context options."
  (context :pointer)
  (option-name :int))

(defun ctx-get (context option-name)
  "@arg[option-name]{:io-threads | :max-threads} @see{CTX-SET}
@return{integer}
Get context options."
  (%ctx-get context
            (ccase option-name
              (:io-threads +io-threads+)
              (:max-sockets +max-sockets+))))

(defcfun* ctx-destroy (:int)
  "Destroy a ØMQ context."
  (context :pointer))

(defbitfield event
  "Socket events (for use with socket-monitor)."
  :connected
  :connect-delayed
  :connect-retried
  :listening
  :bind-failed
  :accepted
  :accept-failed
  :closed
  :close-failed
  :disconnected)

;;; Message

(defcstruct %msg
  "Concealed structure, defined only to know its size."
  (_ :unsigned-char :count 32))

(defcfun* msg-init (:int)
  "Initialise empty ØMQ message.

Low-level API. Consider using @fun{WITH-MESSAGE}.
@see{MSG-CLOSE}
@see{MSG-INIT-SIZE}
@see{MSG-INIT-DATA}"
  (msg :pointer))

(defcfun* msg-init-size (:int)
  "Initialise ØMQ message of a specified size.
@see{MSG-CLOSE}
@see{MSG-INIT}
@see{MSG-INIT-DATA}"
  (msg :pointer)
  (size size))

(defcfun ("zmq_msg_init_data" %msg-init-data) :int
  "Initialise ØMQ message from a supplied buffer. "
  (msg :pointer)
  (data :string)
  (size size)
  (ffn :pointer)
  (hint :pointer))

(defcallback free-fn :void ((data :pointer) (hint :pointer))
  (declare (ignorable hint))
  (foreign-string-free data))

(defun msg-init-data (msg data)
  "Initialise ØMQ message from a supplied buffer.
@see{MSG-CLOSE}
@see{MSG-INIT-SIZE}
@see{MSG-INIT-DATA}"
  (let ((ptr (foreign-string-alloc data)))
    (with-c-error-check (:int)
      (%msg-init-data msg ptr (length data) 'free-fn nil))))

(defbitfield send/recv-options
  :dontwait
  :sndmore)

(defcfun ("zmq_msg_send" %msg-send) :int
  "Send a message part on a socket."
  (msg :pointer)
  (socket :pointer)
  (flags :int))

(defun msg-send (msg socket &key dontwait sndmore)
  "Send a message part on a socket."
  (with-c-error-check (:int t)
    (%msg-send msg socket (+ (if dontwait 1 0) (if sndmore 2 0)))))

(defcfun ("zmq_msg_recv" %msg-recv) :int
  "Receive a message part from a socket."
  (msg :pointer)
  (socket :pointer)
  (flags :int))

(defun msg-recv (msg socket &key dontwait)
  "Receive a message part from a socket. "
  (with-c-error-check (:int t)
    (%msg-recv msg socket (if dontwait 1 0))))

(defcfun* msg-close (:int)
  "Release ØMQ message.

Low-level API. Consider using @fun{WITH-MESSAGE}."
  (msg :pointer))

(defcfun ("zmq_msg_data" msg-data) :pointer
  "Retrieve pointer to message content."
  (msg :pointer))

(defcfun ("zmq_msg_size" msg-size) size
  "Retrieve message content size in bytes."
  (msg :pointer))

(defcfun ("zmq_msg_more" %msg-more) :int
  "indicate if there are more message parts to receive."
  (msg :pointer))

(defun msg-more (msg)
  "Indicate if there are more message parts to receive."
  (if (zerop (%msg-more msg)) t nil))

(defcfun ("zmq_msg_get" %msg-get) :int
  "Get message property."
  (msg :pointer)
  (property :int))

(defconstant +more+ 1)

(defun msg-get (msg property)
  "Get message property.  The only defined property is :more; equivalent to @fun{MSG-MORE}.
@arg[property]{:more}"
  (assert (eq property :more))
  (with-c-error-check (:int)
    (%msg-get msg +more+)))

(defcfun* msg-set (:int)
  "Set message property.  No setable properties defined yet."
  (msg :pointer)
  (property :int)
  (value :int))

(defcfun* msg-copy (:int)
  "Copy content of a message to another message."
  (dest :pointer)
  (src :pointer))

(defcfun* msg-move (:int)
  "Move content of a message to another message."
  (dest :pointer)
  (src :pointer))

;;; Socket

(defcenum socket-type
  :pair
  :pub :sub
  :req :rep
  :dealer :router
  :pull :push
  :xpub :xsub)

(defcfun* socket (:pointer)
  "Create ØMQ socket.
@arg[type]{:pair | :pub | :sub | :req | :rep | :dealer | :router | :pull | :push | :xpub | :xsub}"
  (context :pointer)
  (type socket-type))

(defcfun* close (:int)
  "Close ØMQ socket."
  (socket :pointer))

(defcenum socket-options
  (:affinity 4)
  (:identity 5)
  (:subscribe 6)
  (:unsubscribe 7)
  (:rate 8)
  (:recovery-ivl 9)
  (:sndbuf 11)
  (:rcvbuf 12)
  (:rcvmore 13)
  (:fd 14)
  (:events 15)
  (:type 16)
  (:linger 17)
  (:reconnect-ivl 18)
  (:backlog 19)
  (:reconnect-ivl-max 21)
  (:maxmsgsize 22)
  (:sndhwm 23)
  (:rcvhwm 24)
  (:multicast-hops 25)
  (:rcvtimeo 27)
  (:sndtimeo 28)
  (:ipv4only 31)
  (:last-endpoint 32)
  (:router-behavior 33)
  (:tcp-keepalive 34)
  (:tcp-keepalive-cnt 35)
  (:tcp-keepalive-idle 36)
  (:tcp-keepalive-intvl 37)
  (:tcp-accept-filter 38)
  (:delay-attach-on-connect 39))

(defbitfield (events :short)
  :pollin
  :pollout
  :pollerr)

(defcfun ("zmq_getsockopt" %getsockopt) :int
  "Get ØMQ socket options."
  (socket :pointer)
  (option-name socket-options)
  (option-value :pointer)
  (len :pointer))

(defun getsockopt (socket option-name)
  "Get ØMQ socket options.
@arg[option-name]{keyword}
@return{integer, or string for :identity and :last-endpoint}"
  (with-foreign-object (len :uint)
    (flet ((call (val)
             (with-c-error-check (:int)
               (%getsockopt socket option-name val len))))
      (case option-name
        ;; uint64
        (:affinity
         (with-foreign-object (val :uint64)
           (setf (mem-ref len :uint) (foreign-type-size :uint64))
           (call val)
           (mem-ref val :uint64)))
        ;; int64
        (:maxmsgsize
         (with-foreign-object (val :int64)
           (setf (mem-ref len :uint) (foreign-type-size :int64))
           (call val)
           (mem-ref val :int64)))
        ;; binary 1..255
        ((:identity :last-endpoint)
         (with-foreign-pointer-as-string ((buf size) 256)
           (setf (mem-ref len :uint) (1- size))
           (call buf)))
        ;; int
        (t
         (with-foreign-object (val :int)
           (setf (mem-ref len :uint) (foreign-type-size :int))
           (call val)
           (let ((ret (mem-ref val :int)))
             (case option-name
               (:type (convert-from-foreign ret 'socket-type))
               ((:rcvmore :ipv4only :delay-attach-on-connect)
                (plusp ret))
               (:events (convert-from-foreign ret 'events))
               (t ret)))))))))

(defcfun ("zmq_setsockopt" %setsockopt) :int
  "Set ØMQ socket options."
  (socket :pointer)
  (option-name socket-options)
  (option-value :pointer)
  (option-len :uint))

(defun setsockopt (socket option-name option-value)
  "Set ØMQ socket options."
  (flet ((call (val size)
           (declare (type integer size))
           (with-c-error-check (:int)
             (%setsockopt socket option-name val size))))
    (case option-name
      ;; uint64
      (:affinity
       (with-foreign-object (val :uint64)
         (setf (mem-ref val :uint64) option-value)
         (call val (foreign-type-size :uint64))))
      ;; int64
      (:maxmsgsize
       (with-foreign-object (val :int64)
         (setf (mem-ref val :int64) option-value)
         (call val (foreign-type-size :int64))))
      ;; binary 1..255
      ((:subscribe :unsubscribe :identity :tcp-accept-filter)
       (if option-value
           (with-foreign-string ((buf size) option-value)
             (call buf (1- size)))
           (call (null-pointer) 0)))
      (t
       (with-foreign-object (val :int)
         (setf (mem-ref val :int)
               (case option-name
                 ((:ipv4only :delay-attach-on-connect :router-behavior)
                  (if option-value 1 0))
                 (t
                  option-value)))
         (call val (foreign-type-size :int)))))))

(defcfun* bind (:int)
  "Accept connections on a socket.

Only one socket may be bound to a particular endpoint.
Bound socket may receive messages sent before it was bound.
@arg[endpoint]{\"transport://address\"}"
  (socket :pointer)
  (endpoint :string))

(defcfun* connect (:int)
  "Connect a socket.

Many sockets may connect to the same endpoint.
Connected socket may not receive messages sent before it was bound.
@arg[endpoint]{\"transport://address\"}"
  (socket :pointer)
  (endpoint :string))

(defcfun ("zmq_send" %send) :int
  "Send a message part on a socket."
  (socket :pointer)
  (buf :pointer)
  (len size)
  (flags :int))

(defun send (socket buf &key len dontwait sndmore
                             (encoding cffi:*default-foreign-encoding*))
  (declare (optimize speed))
  "Send a message part on a socket.

@arg[buf]{string, or foreign byte array}
@arg[len]{ignored, or array size} "
  (let ((flags (+ (if dontwait 1 0) (if sndmore 2 0))))
    (if (stringp buf)
        (with-foreign-string ((buf len) (if len (subseq buf 0 len) buf)
                              :encoding encoding)
          (with-c-error-check (:int t)
            (locally (declare (type (integer 1 #.most-positive-fixnum)
                                    len))
              (%send socket buf (1- len) flags))))
        (with-c-error-check (:int t)
          (%send socket buf len flags)))))

(defcfun ("zmq_recv" %recv) :int
  "Receive a message part from a socket."
  (socket :pointer)
  (buf :string)
  (len size)
  (flags :int))

(defcstruct pollitem
  (socket :pointer)
  (fd :int)
  (events events)
  (revents events))

(defcfun ("zmq_poll" %poll) :int
  (items :pointer)
  (nitems :int)
  (timeout :long))

(defun poll (items &optional (timeout -1))
  "Input/output multiplexing on ZeroMQ sockets.
@arg[items]{Poll items prepared with @fun{WITH-POLL-ITEMS}}
@arg[timeout]{-1 : wait indefinitely; N : wait N milliseconds}
@return{The number of ready items.}"
  (with-c-error-check (:int t)
    (%poll (car items) (cdr items) timeout)))

;;; Device

(defcenum device
  (:streamer 1)
  :forwarder
  :queue)

(defcfun* device (:int)
  "Start built-in ØMQ device in the calling thread. Never returns unless interrupted.
@arg[device]{:streamer | :forwarder | :queue}
@arg[frontend, backend]{socket}"
  (device device)
  (frontend :pointer)
  (backend :pointer))
