;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Z80 assembler, targetting SNA snap files
;;;
;;; Written 2004 by Ingvar Mattsson <ingvar@hexapodia.net>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "Z80-ASM")

(defgeneric minval (x))
(defgeneric maxval (x))

(defun copy-range (range)
  (make-instance 'address-range :minval (minval range) :maxval (maxval range)))

(defclass asmlabel ()
  ((dest :accessor dest :initarg :dest)
   (name :reader name :initarg :name)))

(defclass asmdata ()
  ((startpos :reader startpos :initarg :startpos))
  (:default-initargs :startpos (copy-range *current-position*)))

(defclass asminstr (asmdata)
  ((instr :reader instr :initarg :instr)
   (source :reader source :initarg :source)
   (target :reader target :initarg :target)
   (flags :reader flags :initarg :flags :initform nil) 
   ))
(defclass asmvirtinstr (asminstr) ())

(defclass asmplace () ())
(defclass memory-direct (asmplace)
  ((address :accessor address :initarg :address)))
(defclass memory-indirect (asmplace)
  ((register :reader register :initarg :register)))
(defclass immediate (asmplace)
  ((value :reader value :initarg :value)))
(defclass byte-immediate (immediate) ())
(defclass word-immediate (immediate) ())
(defclass register (asmplace)
  ((register :reader register :initarg :register)
   (size :reader size :initarg :size)))
(defclass aux-register (register)
  ()
  (:default-initargs :size 16))
(defclass memory-indirect-indexed (memory-indirect immediate) ())
(defclass memory-indirect-immediate (immediate memory-indirect) ())
(defclass bitpos (asmplace)
  ((pos :reader pos :initarg :pos)))
(defmethod size ((reg memory-indirect))
  (declare (ignore reg))
  16)

(defmethod minval ((x integer))
  x)
(defmethod maxval ((x integer))
  x)

(defclass address-range ()
  ((minval :accessor minval :initarg :minval)
   (maxval :accessor maxval :initarg :maxval)))

(defclass data (asmdata)
  ((datavec :reader datavec :initarg :datavec)))

(defgeneric incf-range (drange srange))
(defmethod incf-range ((drange address-range) (srange address-range))
  (incf (minval drange) (minval srange))
  (incf (maxval drange) (maxval srange))
  drange)
(defmethod incf-range ((drange address-range) (num fixnum))
  (incf (minval drange) num)
  (incf (maxval drange) num)
  drange)
(defmethod incf-range ((num fixnum) drange)
  (let ((range (make-instance 'address-range :minval num :maxval num)))
    (incf-range range drange)))
(defmethod print-object ((range address-range) stream)
  (format stream "#<ADDRESS-RANGE ~d ~d>" (minval range) (maxval range)))

(defgeneric decf-range (drange srange))
(defmethod decf-range ((drange address-range) (srange address-range))
  (decf (minval drange) (minval srange))
  (decf (maxval drange) (maxval srange))
  drange)
(defmethod decf-range ((drange address-range) (num fixnum))
  (decf (minval drange) num)
  (decf (maxval drange) num)
  drange)
(defmethod decf-range ((num fixnum) drange)
  (let ((range (make-instance 'address-range :minval num :maxval num)))
    (decf-range range drange)))

(defun range< (a b)
  (< (minval a) (minval b)))
(defun range> (a b)
  (> (maxval a) (maxval b)))
(defun range= (a b)
  (cl:and (= (minval a) (minval b))
	  (= (maxval a) (maxval b))))

;;; Debug aids
(defmethod print-object ((obj asminstr) stream)
  (format stream "#<ASMINSTR ~a ~a ~a>" (instr obj) (target obj) (source obj)))

(defmethod print-object ((obj immediate) stream)
  (format stream "#<IMMEDIATE ~a>" (value obj)))

(defmethod print-object ((obj memory-direct) stream)
  (format stream "#<MEMORY-DIRECT ~a>" (address obj)))

(defmethod print-object ((obj memory-indirect) stream)
  (format stream "#<MEMORY-INDIRECT (~a)>" (register obj)))

(defmethod print-object ((obj memory-indirect-indexed) stream)
  (format stream "#<MEMORY-INDIRECT-INDEXED (+ ~a ~a)>" (register obj) (value obj)))

(defmethod print-object ((obj register) stream)
  (format stream "#<REGISTER ~a>" (register obj)))

(defmethod print-object ((obj asmlabel) stream)
  (format stream "#<ASMLABEL ~a>" (name obj)))
