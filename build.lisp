(in-package cl-user)

(load-all-patches)

(require "asdf")

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(set-default-character-element-type 'character)
(pushnew :utf-8 system:*specific-valid-file-encodings*)

(asdf:load-system :nlpedit)

(deliver #'nlpedit:main
         #+mswindows "~/common-lisp/nlpedit/nlpedit.exe"
         #+darwin (create-macos-application-bundle "~/common-lisp/nlpedit/nlpedit.app")
         0
         :interface :capi
         :console :input)

#|(deliver #'nlpedit:main "~/common-lisp/nlpedit/nlpedit" 0
         :interface :capi
         :console :input
         :split nil)|#