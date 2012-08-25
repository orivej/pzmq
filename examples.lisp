(defpackage #:pzmq-examples
  (:use #:cl #:split-sequence #:iterate #:local-time))

(in-package #:pzmq-examples)


;;; Simple publisher and subscriber

(defun sequential-publisher (&optional (listen-address "tcp://*:5556"))
  (pzmq:with-socket socket :pub
    (pzmq:bind socket listen-address)
    (loop for i from 1
          do (pzmq:send socket (princ-to-string i)))))

(defparameter *local-time-format* '((:hour 2) ":" (:min 2) ":" (:sec 2) "." (:msec 3) "  "))

(defun simple-subscriber (&key (server-address "tcp://127.0.0.1:5556") (burst 10000))
  (pzmq:with-socket socket (:sub :subscribe "")
    (pzmq:connect socket server-address)
    (loop for i from 1
          for reply = (pzmq:recv-string socket :encoding :ascii)
          when (> i burst)
          do (progn
               (setf i 1)
               (format-timestring t (now) :format *local-time-format*)
               (write-line reply)))))


;;; Educational requester and responder for Pieter Hintjens's guide at http://zguide.zeromq.org/page:all

(defun hwclient (&optional (server-address "tcp://localhost:5555"))
  "Translation of http://zguide.zeromq.org/c:hwclient updated for ZMQ 3.  Includes some parameters in with-* macros to demonstrate syntax."
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to hello world server...")
      (pzmq:connect requester server-address)
      (dotimes (i 10)
        (format t "Sending Hello ~d...~%" i)
        (pzmq:send requester "Hello")
        (write-string "Receiving... ")
        (write-line (pzmq:recv-string requester))))))

(defun hwserver (&optional (listen-address "tcp://*:5555"))
  "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
  (pzmq:with-context nil ; use *default-context*
    (pzmq:with-socket responder :rep
      (pzmq:bind responder listen-address)
      (loop
        (write-string "Waiting for a request... ")
        (write-line (pzmq:recv-string responder))
        (sleep 1)
        (pzmq:send responder "World")))))


;;; Educational subscriber and publisher for the guide
;;; NOTE: these wuclient and wuserver don't work together as concurrent threads

(defun wuclient (&key (server-address "tcp://localhost:5556")
                      (filter "1000") (repetitions 100))
  (pzmq:with-socket subscriber (:sub :subscribe filter :rcvbuf 1000000)
    (pzmq:connect subscriber server-address)
    (iter
      (repeat repetitions)
      (for reply = (pzmq:recv-string subscriber :encoding :ascii))
      (for temperature =
           (parse-integer (cadr (split-sequence #\  reply))))
      (format t "Got ~s.~%" reply)
      (sum temperature into total-temperature)
      (finally
       (format t "Average temperature for zipcode ~a was ~d.~%"
               filter (round total-temperature repetitions))))))

(defun wuserver (&optional (listen-address "tcp://*:5556"))
  (pzmq:with-socket publisher :pub
    (pzmq:bind publisher listen-address)
    (loop
      (let* ((zipcode (random 100000))
             (temperature (- (random 120) 45))
             (relhumidity (+ (random 50) 10))
             (broadcast (format nil "~5,'0d ~d ~d"
                                zipcode temperature relhumidity)))
        (pzmq:send publisher broadcast)))))


;;; Throughput performance tests

;;; Emacs: (put 'with-timing 'common-lisp-indent-function 2)
(defmacro with-timing ((&key run real) form &body body)
  `(let (,@(when run `((,run (get-internal-run-time))))
         ,@(when real `((,real (now)))))
     ,form
     (setf ,@(when run `(,run (/ (- (get-internal-run-time) ,run) internal-time-units-per-second)))
           ,@(when real `(,real (timestamp-difference (now) ,real))))
     ,@body))

(defun remote-thr (address message-size message-count)
  "Publisher immediately sending messages to ADDRESS."
  (declare (ignore message-count))
  (pzmq:with-context nil
    (pzmq:with-socket socket :pub
      (pzmq:connect socket address)
      (cffi:with-foreign-object (payload :uint8 message-size)
        (loop (pzmq:send socket payload :len message-size))))))

(defun local-thr (address message-size message-count)
  "Subscriber exiting after receiving MESSAGE-COUNT messages."
  (pzmq:with-context nil
    (pzmq:with-socket socket (:sub :subscribe "")
      (pzmq:bind socket address)
      (pzmq:with-message message
        (with-timing (:run run-time :real real-time)
            (loop for i from 1 to message-count
                  do (pzmq:msg-recv message socket nil)
                  do (assert (= (pzmq:msg-size message) message-size)))
          (let* ((msgs-throughput-run (/ message-count run-time))
                 (mbod-throughput-run (/ (* msgs-throughput-run message-size 8) (expt 10 6)))
                 (msgs-throughput-real (/ message-count real-time))
                 (mbod-throughput-real (/ (* msgs-throughput-real message-size 8) (expt 10 6))))
            (format t "message size: ~a bytes~%" message-size)
            (format t "message count: ~a~%" message-count)
            (format t "mean throughput ~a messages per second of run time~%" (round msgs-throughput-run))
            (format t "mean throughput ~,3f Mbit/s of run time~%" mbod-throughput-run)
            (format t "mean throughput ~a messages per second of real time~%" (round msgs-throughput-real))
            (format t "mean throughput ~,3f Mbit/s of real time~%" mbod-throughput-real)))))))


;;; Educational pushers and pullers for the guide

(defun taskvent (&key (listen-address "tcp://*:5557") (tasks 100) interactive)
  (pzmq:with-context nil
    (pzmq:with-socket sender :push
      (pzmq:bind sender listen-address)
      (case interactive
        ((t) (write-line "Press ENTER when workers are ready!") (read-line))
        ((nil) (sleep 0.1)))
      (write-line "Sending tasks to workers.")
      (pzmq:send sender "0")
      (loop repeat tasks
            for workload = (1+ (random 100))
            sum workload into total-msec
            do (pzmq:send sender (princ-to-string workload))
            finally (format t "Total expected cost: ~d msec.~%" total-msec)))))

(defun taskwork (&key (from "tcp://127.0.0.1:5557") (to "tcp://127.0.0.1:5558"))
  (pzmq:with-context nil
    (pzmq:with-sockets ((receiver :pull) (sender :push))
      (pzmq:connect receiver from)
      (pzmq:connect sender to)
      (loop (let ((reply (pzmq:recv-string receiver)))
              (sleep (/ (parse-integer reply) 1000))
              (pzmq:send sender ""))))))

(defun tasksink (&key (listen-address "tcp://*:5558") (tasks 100))
  (pzmq:with-context nil
    (pzmq:with-socket receiver :pull
      (pzmq:bind receiver listen-address)
      (pzmq:recv-string receiver)
      (with-timing (:real total-time)
          (dotimes (i tasks)
            (pzmq:recv-string receiver)
            (write-char (case (rem i 10) (4 #\/) (9 #\:) (t #\.))))
        (format t "~&Time spent in sink: ~d msec.~%" (round total-time 1/1000))))))

(defun launch-task-pipeline (&optional (workers 20) &aux (threads (list)))
  (with-timing (:real total-time)
      (unwind-protect
           (progn
             (dotimes (i workers)
               (push (bt:make-thread #'taskwork :name (format nil "taskwork~d" i)) threads))
             (taskvent)
             (tasksink))
        (map 'nil #'bt:destroy-thread threads))
    (format t "~&Real time spent: ~d msec.~%" (round total-time 1/1000))))

;;; DONTWAIT as a substitute for POLL

(defun msreader (&key (sender "tcp://localhost:5557")
                      (publisher "tcp://localhost:5556")
                      (filter "10001 "))
  (pzmq:with-sockets ((receiver :pull)
                      (subscriber (:sub :subscribe filter)))
    (pzmq:connect receiver sender)
    (pzmq:connect subscriber publisher)
    (loop
      (handler-case (loop (pzmq:recv-string receiver :dontwait t))
        (pzmq:eagain ()))
      (handler-case (loop (pzmq:recv-string subscriber :dontwait t))
        (pzmq:eagain ()))
      (sleep 1e-3))))

;;; now with POLL

(defun mspoller (&key (sender "tcp://localhost:5557")
                      (publisher "tcp://localhost:5556")
                      (filter "10001 "))
  (pzmq:with-sockets ((receiver :pull)
                      (subscriber (:sub :subscribe filter)))
    (pzmq:connect receiver sender)
    (pzmq:connect subscriber publisher)
    (pzmq:with-poll-items items (receiver subscriber)
      (loop
        (pzmq:poll items)
        (when (member :pollin (pzmq:revents items 0))
          (pzmq:recv-string receiver))
        (when (member :pollin (pzmq:revents items 1))
          (pzmq:recv-string subscriber))))))

;;; Educational pushers and pullers extended with a terminating publisher
(defun taskwork2 (&key (from "tcp://127.0.0.1:5557")
                       (to "tcp://127.0.0.1:5558")
                       (control "tcp://127.0.0.1:5559"))
  (pzmq:with-context nil
    (pzmq:with-sockets ((receiver :pull) (sender :push)
                        (controller (:sub :subscribe "")))
      (pzmq:connect receiver from)
      (pzmq:connect sender to)
      (pzmq:connect controller control)
      (pzmq:with-poll-items items (receiver controller)
        (loop
          do (pzmq:poll items)
          until (car (pzmq:revents items 1))
          do (let ((reply (pzmq:recv-string receiver)))
               (sleep (/ (parse-integer reply) 1000))
               (pzmq:send sender "")))))))

(defun tasksink2 (&key (receiver-address "tcp://*:5558")
                       (control-address "tcp://*:5559")
                       (tasks 100))
  (pzmq:with-context nil
    (pzmq:with-sockets ((receiver :pull) (control :pub))
      (pzmq:bind receiver receiver-address)
      (pzmq:bind control control-address)
      (pzmq:recv-string receiver)
      (with-timing (:real total-time)
          (progn
            (dotimes (i tasks)
              (pzmq:recv-string receiver)
              (write-char (case (rem i 10) (4 #\/) (9 #\:) (t #\.))))
            (pzmq:send control "KILL"))
        (format t "~&Time spent in sink: ~d msec.~%" (round total-time 1/1000))))))

(defun launch-task2-pipeline (&optional (workers 20))
  (with-timing (:real total-time)
      (progn
        (dotimes (i workers)
          (bt:make-thread #'taskwork2 :name (format nil "taskwork~d" i)))
        (taskvent)
        (tasksink2))
    (format t "~&Real time spent: ~d msec.~%" (round total-time 1/1000))))
