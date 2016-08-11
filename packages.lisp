(defpackage "Z80-ASM"
  (:shadow "POP" "PUSH" "BIT" "SET" "OR" "AND")
  (:use "COMMON-LISP")
  (:export "ASSEMBLE" "I" "HL-" "DE-" "BC-" "AF-" "HL" "DE"
	   "BC" "IX" "IY" "INTR" "R" "AF" "SP" "INTMODE" "BCOL"))
