(defsystem nlpedit
  :author "April Lu"
  :license "0BSD"
  :version "0.1.0"
  :depends-on (py4cl2 dexador cl-json cl-ppcre)
  :components ((:file "package")
               (:file "util")
               (:file "python")
               (:file "resources")
               (:file "generic")
               (:file "stanza")
               (:file "interface")
               (:file "highlight")))
