(in-package cl-user)

(ql:quickload :py4cl2)

(defparameter *src-dir* (merge-pathnames "runtime/" (asdf:system-source-directory :nlpedit)))
(change-directory *src-dir*)

#-windows
(setf (environment-variable "PYTHONHOME") (namestring (truename "./py/"))
      (environment-variable "PYTHONPATH") (namestring (truename "./py/")))

(setf (py4cl2:config-var 'py4cl2:pycmd) (namestring (truename "./py/bin/python3.10")))

(py4cl2:defpymodule "pip")
(pip:main :args #("install" "stanza"
                  "--target" (namestring (merge-pathnames "./py/lib/python3.10/site-packages/" *src-dir*))))
#+macosx
(pip:main :args #("install" "numpy==1.25.2"
                  "--target" (namestring (merge-pathnames "./py/lib/python3.10/site-packages/" *src-dir*))
                  "--upgrade"))
#+macosx
(dolist (d (directory (merge-pathnames "./py/lib/python3.10/site-packages/numpy-2*" *src-dir*)))
  (uiop:delete-directory-tree d :validate t))

(py4cl2:defpymodule "stanza" t)
(stanza:download :lang "en" :model-dir "./stanza")
