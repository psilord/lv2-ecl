(defun doit (foo bar)
  (format t "DOIT: Got arguments ~A ~A~%" foo bar)
  (+ foo bar))

(defclass lv2-descriptor ()
  ((%lv2-uri :initarg :lv2-uri
             :initform nil
             :reader lv2-uri)
   (%lv2-instantiate :initarg :lv2-instantiate
                     :initform nil
                     :reader lv2-instantiate)
   (%lv2-connect-port :initarg :lv2-connect-port
                      :initform nil
                      :reader lv2-connect-port)
   (%lv2-activate :initarg :lv2-activate
                  :initform nil
                  :reader lv2-activate)
   (%lv2-run :initarg :lv2-run
             :initform nil
             :reader lv2-run)
   (%lv2-deactivate :initarg :lv2-deactivate
                    :initform nil
                    :reader lv2-deactivate)
   (%lv2-cleanup :initarg :lv2-cleanup
                 :initform nil
                 :reader lv2-cleanup)
   (%lv2-extension-data :initarg :lv2-extension-data
                        :initform nil
                        :reader lv2-extension-data)))

(defun make-lv2-descriptor (&rest args)
  (apply #'make-instance 'lv2-descriptor args))

