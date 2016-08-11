;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Z80 assembler
;;;
;;; This file is in the public domain
;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "Z80-ASM")
;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains the output routines. For a new routine,
;;; three methods need to be implemented. One to return the
;;; relevant file extension and one to take a 64 KB (array (integer -1 255)
;;; and output that. Bytes that have not been filled by the assembler will
;;; have the value -1 and suitable default values should be substituted
;;; One method to return the "default starting point".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric assembler-extension (outfile-format)
  (:documentation "EQL-specialized defmethods returning the string that should
be sued as file extension for the output file of a given format.

The input data is a keyword and should (ideally) correspond to the
file format name."))

(defgeneric assembler-output (array outstream outfile-format extras)
  (:documentation "ARRAY is a 64 KB of (integer -1 255)
OUTSTREAM is an open output stream
OUTFILE-FORMAT is an EQL specialiser for the output file format keyword"))

(defgeneric assembler-start (outfile-format))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SNA file format, Ingvar Mattsson
;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod assembler-extension ((type (eql :sna)))
  "sna")
(defmethod assembler-output (array stream (type (eql :sna)) extras)
  (let ((i 0) (hl- 0)
	(de- 0) (bc- 0) (af- 0) (hl 0) (de 0)
	(bc 0) (ix 0) (iy 0) (intr 0) (r 0)
	(af 0) (sp 0) (intmode 0) (bcol 0))
    (when (member 'i extras :key #'car)
      (setf i (cdr (assoc 'i extras))))
    (when (member 'hl- extras :key #'car)
      (setf hl- (cdr (assoc 'hl- extras))))
    (when (member 'de- extras :key #'car)
      (setf de- (cdr (assoc 'de- extras))))
    (when (member 'bc- extras :key #'car)
      (setf bc- (cdr (assoc 'bc- extras))))
    (when (member 'af- extras :key #'car)
      (setf af- (cdr (assoc 'af- extras))))
    (when (member 'hl extras :key #'car)
      (setf hl (cdr (assoc 'hl extras))))
    (when (member 'de extras :key #'car)
      (setf de (cdr (assoc 'de extras))))
    (when (member 'bc extras :key #'car)
      (setf bc (cdr (assoc 'bc extras))))
    (when (member 'ix extras :key #'car)
      (setf ix (cdr (assoc 'ix extras))))
    (when (member 'iy extras :key #'car)
      (setf iy (cdr (assoc 'iy extras))))
    (when (member 'intr extras :key #'car)
      (setf intr (cdr (assoc 'intr extras))))
    (when (member 'r extras :key #'car)
      (setf r (cdr (assoc 'r extras))))
    (when (member 'af extras :key #'car)
      (setf af (cdr (assoc 'af extras))))
    (when (member 'sp extras :key #'car)
      (setf sp (cdr (assoc 'sp extras))))
    (when (member 'intmode extras :key #'car)
      (setf intmode (cdr (assoc 'intmode extras))))
    (when (member 'bcol extras :key #'car)
      (setf bcol (cdr (assoc 'bcol extras))))
    (write-byte (low-byte i) stream)
    (write-byte (low-byte hl-) stream) (write-byte (high-byte hl-) stream)
    (write-byte (low-byte de-) stream) (write-byte (high-byte de-) stream)
    (write-byte (low-byte bc-) stream) (write-byte (high-byte bc-) stream)
    (write-byte (low-byte af-) stream) (write-byte (high-byte af-) stream)
    (write-byte (low-byte hl) stream) (write-byte (high-byte hl) stream)
    (write-byte (low-byte de) stream) (write-byte (high-byte de) stream)
    (write-byte (low-byte bc) stream) (write-byte (high-byte bc) stream)
    (write-byte (low-byte ix) stream) (write-byte (high-byte ix) stream)
    (write-byte (low-byte iy) stream) (write-byte (high-byte iy) stream)
    (write-byte (low-byte intr) stream) 
    (write-byte (low-byte r) stream) 
    (write-byte (low-byte af) stream) (write-byte (high-byte af) stream)
    (write-byte (low-byte sp) stream) (write-byte (high-byte sp) stream)
    (write-byte (low-byte intmode) stream)
    (write-byte (low-byte bcol) stream))

  (loop for offset from (assembler-start :sna) to 65535
	do (let ((byte (aref array offset)))
	     (write-byte (if (= byte -1) 0 byte) stream)))
  'done)
(defmethod assembler-start ((type (eql :sna)))
  16384)
