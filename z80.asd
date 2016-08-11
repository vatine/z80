(asdf:defsystem "z80"
  :description "Assembler for Z80 assembler in a lispy notation"
  :version "0.1"
  :components ((:file "packages")
	       (:file "globals" :depends-on ("packages"))
	       (:file "classes" :depends-on ("globals" "packages"))
	       (:file "asm" :depends-on ("classes" "globals" "packages"))
	       (:file "output" :depends-on ("asm" "globals" "packages"))
	       (:file "definstr" :depends-on ("asm" "packages" "classes" "globals"))))
