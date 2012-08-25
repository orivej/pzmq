(defpackage #:pzmq-test
  (:use #:cl #:5am #:let-plus))

(in-package #:pzmq-test)

(def-suite :pzmq-test-suite)
(in-suite :pzmq-test-suite)

(test supported-version
  (let+ (((major minor nil) (pzmq:version)))
    (is (and (>= major 3) (>= minor 2)))))

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
