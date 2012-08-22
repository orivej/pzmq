(defpackage #:pzmq-test
  (:use #:cl #:5am #:let-plus)
  (:export #:pzmq-test-suite))

(in-package #:pzmq-test)

(def-suite :pzmq-test-suite)
(in-suite :pzmq-test-suite)

(test supported-version
  (let+ (((major minor nil) (pzmq:version)))
    (is (and (>= major 3) (>= minor 2)))))
