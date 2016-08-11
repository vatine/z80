;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Z80 assembler, targetting SNA snap files
;;;
;;; Written 2004 by Ingvar Mattsson <ingvar@hexapodia.net>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "Z80-ASM")

(defvar *defined-classes* nil "List of mmnemonics classes defined so far")
(defvar *current-position* 0)
(defvar *program* nil)
(defvar *labels* (make-hash-table :test #'equal))
(defvar *current-label* nil)
(defvar *default-output-style* :sna)

