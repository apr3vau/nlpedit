(in-package cl-user)

(load-all-patches)

(require "asdf")

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system :nlpedit)

(deliver #'nlpedit:main "~/common-lisp/nlpedit/runtime/nlpedit" 0
         :interface :capi
         :console :input)