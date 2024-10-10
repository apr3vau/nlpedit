(defsystem nlpedit
  :author "April Lu"
  :license "0BSD"
  :version "0.2.2"
  :depends-on (py4cl2 dexador cl-json cl-ppcre alexandria)
  :components ((:file "package")
               (:file "util")
               (:file "python")
               (:file "resources")
               (:file "generic")
               (:file "stanza")
               (:file "installer")
               (:file "interface")
               (:file "highlight")))
