(defun logit (fmt &rest args)
  (apply #'format t (concatenate 'string "LISP Plugin: " fmt) args)
  (finish-output))

(defun plugin1-instantiate (descriptor rate bundle_path features)
  (logit "plugin1-instantiate: ~A ~A ~A ~A~%"
         descriptor rate bundle_path features))

(defun plugin1-connect-port (instance port data)
  (logit "plugin1-connect-port: ~A ~A ~A~%" instance port data))

(defun plugin1-activate (instance)
  (logit "plugin1-activate: ~A~%" instance))

(defun plugin1-run (instance num_samples)
  (logit "plugin1-run: ~A ~A~%" instance num_samples))

(defun plugin1-deactivate (instance)
  (logit "plugin1-deactivate: ~A~%" instance))

(defun plugin1-cleanup (instance)
  (logit "plugin1-cleanup: ~A~%" instance))

(defun plugin1-extension-data (uri)
  (logit "plugin1-extension-data: ~A~%" uri))




(defun plugin2-instantiate (descriptor rate bundle_path features)
  (logit "plugin2-instantiate: ~A ~A ~A ~A~%"
         descriptor rate bundle_path features))

(defun plugin2-connect-port (instance port data)
  (logit "plugin2-connect-port: ~A ~A ~A~%" instance port data))

(defun plugin2-activate (instance)
  (logit "plugin2-activate: ~A~%" instance))

(defun plugin2-run (instance num_samples)
  (logit "plugin2-run: ~A ~A~%" instance num_samples))

(defun plugin2-deactivate (instance)
  (logit "plugin2-deactivate: ~A~%" instance))

(defun plugin2-cleanup (instance)
  (logit "plugin2-cleanup: ~A~%" instance))

(defun plugin2-extension-data (uri)
  (logit "plugin2-extension-data: ~A~%" uri))


(defparameter *plugin0*
  (make-lv2-descriptor
   :lv2-uri "http://lv2plug.in/plugins/eg-amp-a"
   :lv2-instantiate #'plugin1-instantiate
   :lv2-connect-port #'plugin1-connect-port
   :lv2-activate #'plugin1-activate
   :lv2-run #'plugin1-run
   :lv2-deactivate #'plugin1-deactivate
   :lv2-cleanup #'plugin1-cleanup
   :lv2-extension-data #'plugin1-extension-data))

(defparameter *plugin1*
  (make-lv2-descriptor
   :lv2-uri "http://lv2plug.in/plugins/eg-amp-b"
   :lv2-instantiate #'plugin2-instantiate
   :lv2-connect-port #'plugin2-connect-port
   :lv2-activate #'plugin2-activate
   :lv2-run #'plugin2-run
   :lv2-deactivate #'plugin2-deactivate
   :lv2-cleanup #'plugin2-cleanup
   :lv2-extension-data #'plugin2-extension-data))

(defun lv2-descriptor (index)
  (format t "lv2-descriptor called with argument: ~D~%" index)
  (finish-output)
  (cond
    ((= index 0)
     *plugin0*)
    ((= index 1)
     *plugin1*)
    (t
     nil)))

