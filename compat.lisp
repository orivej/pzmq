(in-package #:pzmq)

(defun sendmsg (socket msg &key dontwait sndmore)
  "Send a message part on a socket."
  (msg-send msg socket :dontwait dontwait :sndmore sndmore))

(defun recvmsg (socket msg &key dontwait)
  "Receive a message part from a socket."
  (msg-recv msg socket :dontwait dontwait))

(defun init (io-threads)
  "Initialise ØMQ context."
  (let ((context (ctx-new)))
    (ctx-set context :io-threads io-threads)
    context))

(setf (symbol-function 'term) (symbol-function 'ctx-destroy))
