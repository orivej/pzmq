(defpackage #:pzmq-test
  (:use #:cl #:5am #:let-plus))

(in-package #:pzmq-test)

(def-suite :pzmq)
(in-suite :pzmq)

(test supported-version
  (let+ (((major minor &ign) (pzmq:version)))
    (is (or (= major 4)
            (and (= major 3) (= minor 2))))))

(test request-response
  (pzmq:with-sockets ((sender :req) (receiver :rep))
    (pzmq:bind receiver "tcp://*:5555")
    (pzmq:connect sender "tcp://localhost:5555")
    (pzmq:send sender "")
    (is (string= "" (pzmq:recv-string receiver)))))

(test response-request
  (pzmq:with-sockets ((sender :req) (receiver :rep))
    (pzmq:bind receiver "tcp://*:5555")
    (pzmq:connect sender "tcp://localhost:5555")
    (bt:make-thread (lambda () (sleep 0.01) (pzmq:send sender "")))
    (is (string= "" (pzmq:recv-string receiver)))))

(defun accept-plain-auth (s)
  (pzmq:with-messages (ign id)
    (is (string= "1.0" (pzmq:recv-string s)))
    (pzmq:msg-recv id s)  ; request ID
    (is (string= "domain" (pzmq:recv-string s)))
    (pzmq:msg-recv ign s) ; client IP address
    (pzmq:msg-recv ign s) ; socket identity
    (is (string= "PLAIN" (pzmq:recv-string s)))
    (is (string= "user" (pzmq:recv-string s)))
    (is (string= "pass" (pzmq:recv-string s)))
    (is-false (pzmq:getsockopt s :rcvmore))
    (pzmq:send s "1.0" :sndmore t)
    (pzmq:msg-send id s :sndmore t)
    (pzmq:send s "200" :sndmore t)
    (pzmq:send s "Welcome" :sndmore t)
    (pzmq:send s "user" :sndmore t)
    (pzmq:send s nil)))

(test plain-auth
  (pzmq:with-sockets ((receiver (:rep :plain-server t
                                      :zap-domain "domain"))
                      (authenticator :rep)
                      (sender (:req :plain-username "user"
                                    :plain-password "pass")))
    (pzmq:bind receiver "tcp://*:5555")
    (pzmq:bind authenticator "inproc://zeromq.zap.01")
    (pzmq:connect sender "tcp://localhost:5555")
    (accept-plain-auth authenticator)
    (pzmq:send sender "")
    (is (string= "" (pzmq:recv-string receiver)))))
