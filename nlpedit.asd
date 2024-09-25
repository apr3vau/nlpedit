(defsystem nlpedit
  :author "April Lu"
  :license "0BSD"
  :depends-on (py4cl2)
  :components ((:file "package")
               (:file "util")
               (:file "editor-patch")
               (:file "py4cl2-patch")
               (:file "resources")
               (:file "generic")
               (:file "stanza")
               (:file "highlight")
               (:file "interface")))
