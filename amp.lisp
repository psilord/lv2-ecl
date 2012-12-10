(defun instantiate (descriptor rate bundle_path features)
  0)

(defun connect-port (instance port data)
  nil)

(defun activate (instance)
  nil)

(defun run (instance num_samples)
  nil)

(defun deactivate (instance)
  nil)

(defun cleanup (instance)
  nil)

(defun extension-data (uri)
  nil)

(defparameter *descriptor*
  (make-lv2-descriptor
   :lv2-uri "http://lv2plug.in/plugins/eg-amp"
   :lv2-instantiate #'instantiate
   :lv2-connect-port #'connect-port
   :lv2-activate #'activate
   :lv2-run #'run
   :lv2-deactivate #'deactivate
   :lv2-cleanup #'cleanup
   :lv2-extension-data #'extension-data))

(defun lv2-descriptor (index)
  (cond
    ((= index 0)
     *descriptor*)
    (t
     nil)))

