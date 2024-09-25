(defpackage nlpedit
  (:use :cl :hcl :lw :py4cl2))
(in-package nlpedit)

(unless (delivered-image-p)
  (cd "~/common-lisp/nlpedit/runtime/"))

#-win32
(setf (environment-variable "PYTHONHOME") (truename "./py/")
      (environment-variable "PYTHONPATH") (truename "./py/"))

(setf (py4cl2:config-var 'py4cl2:pycmd) (namestring (truename #+win32 "./py/python.exe"
                                                              #+unix "./py/bin/python3.10")))

(export 'nlpedit)
