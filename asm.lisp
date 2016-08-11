2;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Z80 assembler, targetting SNA snap files
;;;
;;; Written 2004 by Ingvar Mattsson <ingvar@hexapodia.net>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "Z80-ASM")

(define-condition multiply-defined-error (simple-error) ())
(define-condition incorrect-flags-error (simple-error) ())
(define-condition incorrect-register (simple-error) ())
(define-condition not-label-ref-error (simple-error) ())

;;; Finish-off helper functions
(defun sort-program ()
  (setf *program* (sort (nreverse *program*) #'range< :key #'startpos)))

(defun needs-relax (i)
  (let ((sp (startpos i)))
    (/= (minval sp) (maxval sp))))

(defun needs-fix (i)
  (typep i 'asmvirtinstr))

(defun fixup-startpos (prog len)
  (loop for i in prog
	do (if (needs-relax i)
	       (progn (incf (minval (startpos i)) (1- len))
		      (decf (maxval (startpos i)) (- 3 len)))
	     (return))))

(defun relax ()
  (sort-program)
  (loop while (some #'needs-fix *program*)
	do (try-relax)))

(defun try-relax (&optional (prog *program*))
  (when prog
    (let ((head (car prog)))
      (if (typep head 'asmvirtinstr)
	  (case (decide-jump-type head)
	    ((jr jp)
	     (fix-jump head)
	     (let ((len (length (emit head))))
	       (fixup-startpos (cdr prog) len)))
	    (t nil))))
    (try-relax (cdr prog))))

;;; Main function

(defun assemble (infile-designator &key
				   (filetype *default-output-style*)
				   output-extras)
  (let ((output-ext (assembler-extension filetype))
	(infile-path (pathname infile-designator)))
    (let ((outfile-path (merge-pathnames (make-pathname :type output-ext) infile-path))
	  (*default-pathname-defaults* (merge-pathnames (make-pathname :type "l80") *default-pathname-defaults*))
	  (target-array (make-array '(65536) :element-type '(integer -1 255) :initial-element -1)))
      (with-open-file (output outfile-path :direction :output
			      :element-type '(integer 0 255)
			      :if-exists :overwrite
			      :if-does-not-exist :create)
	(let ((*package* (find-package "Z80-ASM")))
	  (setf *program* nil)
	  (setf *current-label* nil)
	  (setf *current-position* (copy-range (assembler-start filetype)))
	  (load infile-designator)
	  (relax)
	  (loop for instr in *program*
		do
		(let ((base (minval (startpos instr)))
		      (data (emit instr)))
		  (loop for n from 0 to (1- (length data))
			do (let ((byte (aref data n))) 
			     (setf (aref target-array (+ base n)) byte)))))
	  (assembler-output target-array output filetype output-extras))))))

;;; Instruction set-up
(defmacro definstr (mmn flagsense)
  (when (member mmn *defined-classes*)
    (error 'multiply-defined-error :format-control "Assembler mnemonic ~A already defined" :format-arguments (list mmn)))
  `(progn
;;;     (defclass ,(cl:or classname mmn) (asminstr) ())
     (cl:push ',mmn *defined-classes*)
     (defmacro ,mmn (&rest args)
       `(asm-fun ',',mmn ,',flagsense ',args))))

;;; Instruction helpers
(defun make-label (string)
  (let ((string (string string)))
    (cl:or (gethash string *labels* nil)
	   (setf (gethash string *labels*)
		 (make-instance 'asmlabel :name string :dest nil)))))

(defun register-helper (reg)
  (case reg
    ((a b c d e f h l i r) (make-instance 'register
				      :register reg
				      :size 8))
    ((af bc de hl sp ix iy) (make-instance 'register
					   :register reg
					   :size 16))
    ((:af :bc :de :hl :sp :ix :iy) (make-instance 'aux-register
						  :register (find-symbol (symbol-name reg) "Z80-ASM")))
    (t (make-label (string reg)))))

(defun indirect-helper (ix)
  (case (car ix)
    ((+) (if (symbolp (cadr ix))
	     (make-instance 'memory-indirect-indexed
			    :register (cadr ix) :value (caddr ix))
	   (make-instance 'memory-indirect-indexed
			  :register (caddr ix) :value (cadr ix))))
    ((hl de sp c) (make-instance 'memory-indirect :register (car ix)))
    (t (if (typep (car ix) 'fixnum)
	   (make-instance 'memory-direct :address (car ix))))))

(defun asm-fun (instr flagsense args)
  (let ((source (cadr args))
	(target (car args))
	(flags nil))
    (when flagsense
      (if (cl:and (listp target)
		  (eql (car target) 'flags))
	  (progn (setf flags (car args)) (setf target nil))
	(shiftf source target nil)))

    (when (null source)
      (shiftf source target nil))
    ;; Inspect source
    (setf source
	  (cond ((symbolp source)
		 (register-helper source))
		((integerp source)
		 (make-instance 'immediate :value source))
		((stringp source)
		 (make-label source))
		((listp source) ;; Indirect 
		 (indirect-helper source))
		(t (format t "Fell through trying for source ~A~%" source))))
    (when target
      (setf target (typecase target
		     ((cl:or string symbol) (register-helper target))
		     (list (indirect-helper target))
		     (integer (make-instance 'immediate :value target)))))
    (let ((instr (make-instance 'asminstr
				:instr instr :source source
				:target target :flags flags)))
      (when *current-label*
	(setf (dest *current-label*) instr)
	(setf *current-label* nil))
      (cl:push instr *program*)
      (setf *current-position*
	    (incf-range *current-position* (length (emit instr)))))))

;;; Hand-coded "virtual instructions"
(defmacro org (addr)
  `(let ((addr ,addr))
     (setf (minval *current-position*) addr)
     (setf (maxval *current-position*) addr)))

(defmacro label (str)
  `(setf *current-label*
	 (make-label (string ,str))))

(defun dstr (str)
  (let ((datalen (length str)))
    (let ((dvec (make-array (list datalen)
			    :initial-contents (map 'list #'char-code str))))
      (incf-range *current-position* datalen)
      (cl:push (make-instance 'data :datavec dvec) *program*))))

(defun db (&rest bytes)
  (let ((datalen (length bytes)))
    (let ((dvec (make-array (list datalen)
			    :initial-contents
			    (map 'list #'(lambda (v) (logand v 255)) bytes))))
      (incf-range *current-position* datalen)
      (cl:push (make-instance 'data :datavec dvec) *program*))))

(defmacro jmp (&rest args)
  (let ((conditional nil)
	(source (car (last args))))
    (when (cl:and (listp (car args))
		  (eql (caar args) 'flags))
      (setf conditional (car args)))
    (unless (cl:or (stringp source) (symbolp source))
      (error 'not-label-ref-error
	     :format-control "Not a label reference."
	     :format-arguments (list source)))
    `(let ((instr (make-instance 'asmvirtinstr
				 :instr 'jmp :source (make-label ,source)
				 :target nil :flags ',conditional)))
       (when *current-label*
	 (setf (dest *current-label*) instr)
	 (setf *current-label* nil))
       (cl:push instr *program*)
       (setf *current-position*
	     (incf-range
	      *current-position*
	      (make-instance 'address-range :minval 1 :maxval 3))))))

(defun decide-jump-type (instr)
  (let ((startrange (startpos instr))
	(landrange (copy-range (startpos (dest (source instr))))))
    (decf-range landrange startrange)
    (cond ((cl:and (<= -126 (minval landrange) 129)
		   (<= -126 (maxval landrange) 129))
	   'jr)
	  ((cl:or (< (maxval landrange) -126)
		  (> (minval landrange) 129))
	   'jp)
	  (t 'jmp))))

(defun fix-jump (instr)
  (let ((jumptype (decide-jump-type instr))
	(tinstr (dest (source instr))))
    (case jumptype
      (jr (setf (slot-value instr 'source)
		(make-instance 'byte-immediate :value (- (minval (startpos tinstr)) (+ 2 (minval (startpos instr))))))
	  (setf (slot-value instr 'instr) 'jr))
      (jp (setf (slot-value instr 'instr) 'jp)))
    (unless (eq (instr instr) 'jmp)
      (change-class instr 'asminstr))))

;;; Machine-code generation helpers      
(defgeneric low-byte (data))
(defgeneric high-byte (data))

(defmethod low-byte ((data integer))
  (ldb (byte 8 0) data))
(defmethod low-byte ((data immediate))
  (ldb (byte 8 0) (value data)))
(defmethod low-byte ((data memory-direct))
  (low-byte (address data)))
(defmethod low-byte ((data asmlabel))
  (let ((dest (dest data)))
    (let ((low (minval (startpos dest)))
	  (high (maxval (startpos dest))))
      (if (= low high)
	  (ldb (byte 8 0) low)
	(error "Label not sufficiently fixed")))))
(defmethod high-byte ((data integer))
  (ldb (byte 8 8) data))
(defmethod high-byte ((data memory-direct))
  (high-byte (address data)))
(defmethod high-byte ((data immediate))
  (ldb (byte 8 8) (value data)))
(defmethod high-byte ((data asmlabel))
  (let ((dest (dest data)))
    (let ((low (minval (startpos dest)))
	  (high (maxval (startpos dest))))
      (if (= low high)
	  (ldb (byte 8 8) low)
	(error "Label not sufficiently fixed")))))

(defun compute-flags (flag-spec)
  (unless (eq (car flag-spec) 'flags)
    (error 'incorrect-flags-error :format-control "Assembler flag specification ~%~A~% incorrect, does not start with FLAGS" :format-arguments (list flag-spec)))
  (case (cadr flag-spec)
    ((:z z)   #b00001000)
    ((:po po) #b00100000)
    ((:pe pe) #b00101000)
    ((:p p)   #b00110000)
    ((:nz nz) #b00000000)
    ((:nc nc) #b00010000)
    ((:m m)   #b00111000)
    ((:c c)   #b00011000)
    ((nil) nil)))

(defun compute-register (register &optional source)
  (let ((rval 
	 (case (size register)
	   (8 (cl:or
	       (cdr
		(assoc (register register)
		       '((b . #b000)
			 (c . #b001)
			 (d . #b010)
			 (e . #b011)
			 (h . #b100)
			 (l . #b101)
			 (f . #b110)
			 (a . #b111)
			 (i . #b000))))
	       (error 'incorrect-register
		      :format-control "Incorrect byte register ~a"
		      :format-arguments (list (register register)))))
	   (16 (cl:or
		(cdr
		 (assoc (register register)
			'((bc . #b00)
			  (de . #b01)
			  (hl . #b10)
			  (ix . #b10)
			  (iy . #b10)
			  (af . #b11)
			  (sp . #b11))))
		(error 'incorrect-register
		       :format-control "Incorrect word register ~a"
		       :format-arguments (list (register register)))))
	   (t 0))))
    (if source
	rval
      (ash rval 3))))

(defgeneric prefix (place))
(defmethod prefix (asmplace) nil)
(defmethod prefix ((reg memory-indirect-indexed))
  (case (register reg)
    ((ix :ix) #xDD)
    ((iy :iy) #xFD)))
(defmethod prefix ((reg register))
  (case (register reg)
    ((ix :ix) #xDD)
    ((iy :iy) #xFD)
    (t nil)))

;;; Machine-code generation
(defgeneric emit (data))
(defgeneric emit-instruction (data instruction))
(defmethod emit ((data data))
  (datavec data))
(defmethod emit ((data asminstr))
  (emit-instruction data (instr data)))

;;; Machine-code generation per function
(defmethod emit-instruction (instr (i (eql 'adc)))
  (let ((source (source instr))
	(target (target instr)))
    (case (size target)
      (8 (let ((base #x88))
	   (typecase source
	     (memory-indirect
	      (let ((prefix (prefix source)))
		(if prefix
		    (vector prefix (logior base #x06) (low-byte source)))))
	     (immediate (vector (logior base #x46) (low-byte source)))
	     (register (vector (logior base (compute-register source t)))))))
      (16 (let ((base #x4A)
		(prefix (prefix target))
		(reg (ash (compute-register source t) 4)))
	    (if prefix
		(vector prefix #xED (logior base reg))
	      (vector #xED (logior base reg))))))))

(defmethod emit-instruction (instr (i (eql 'add)))
  (let ((source (source instr))
	(target (target instr)))
    (case (size target)
      (8 (let ((base #x80))
	   (typecase source
	     (memory-indirect
	      (let ((prefix (prefix source)))
		(if prefix
		    (vector prefix (logior base #x06) (low-byte source)))))
	     (immediate (vector (logior base #x46) (low-byte source)))
	     (register (vector (logior base (compute-register source t)))))))
      (16 (let ((base #x09)
		(prefix (prefix target))
		(reg (ash (compute-register source t) 4)))
	    (if prefix
		(vector prefix #xED (logior base reg))
	      (vector #xED (logior base reg))))))))


(defmethod emit-instruction (instr (i (eql 'and)))
  )

(defmethod emit-instruction (instr (i (eql 'bit)))
  (let ((pos (value (target instr)))
	(prefix (prefix (source instr)))
	(offset (compute-register (source instr) t))
	(source (source instr)))
    (when (cl:or (typep (source instr) 'memory-indirect)
		 (typep (source instr) 'memory-indirect-indexed))
      (setf offset (logior offset (ash offset 1))))
    (let ((op (+ #x40 (* pos 8) offset)))
      (if prefix
	  (vector prefix #xCB (low-byte source) op)
	(vector #xCB op)))))

(defmethod emit-instruction (instr (i (eql 'call)))
  (let ((base 205)
	(target (source instr)))
    (when (flags instr)
      (setf base (logior (logand base #b11000110)
			 (compute-flags (flags instr)))))
    (vector base (low-byte target) (high-byte target))))

(defmethod emit-instruction (instr (i (eql 'ccf)))
  (vector #x3F))

(defmethod emit-instruction (instr (i (eql 'cp)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((prefix (prefix source))
			     (base #xBE))
			 (if prefix
			     (vector prefix base (low-byte source))
			   (vector base))))
      (immediate (vector #xFE (low-byte source)))
      (t (let ((base #xB8)
	       (reg (compute-register source t)))
	   (vector (logior base reg)))))))

(defmethod emit-instruction (instr (i (eql 'cpd)))
  (vector #xED #xA9))

(defmethod emit-instruction (instr (i (eql 'cpdr)))
  (vector #xED #xB9))

(defmethod emit-instruction (instr (i (eql 'cpi)))
  (vector #xED #xA1))

(defmethod emit-instruction (instr (i (eql 'cpir)))
  (vector #xED #xB1))

(defmethod emit-instruction (instr (i (eql 'cpl)))
  (vector #x2F))

(defmethod emit-instruction (instr (i (eql 'daa)))
  (vector #x27))

(defmethod emit-instruction (instr (i (eql 'dec)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((prefix (prefix source))
			     (base #x35))
			 (if prefix
			     (vector prefix base (low-byte source))
			   (vector base))))
      (register (case (size source)
		  (8 (let ((base #x05)
			   (reg (compute-register source)))
		       (vector (logior reg base))))
		  (16 (let ((base #x0B)
			    (prefix (prefix source))
			    (reg (ash (compute-register source t) 4)))
			(if prefix
			    (vector prefix (logior base reg))
			  (vector (logior base reg))))))))))

(defmethod emit-instruction (instr (i (eql 'di)))
  (vector #xF3))

(defmethod emit-instruction (instr (i (eql 'djnz)))
  (let ((source (source instr)))
    (typecase source
      (asmlabel (setf source (-
			      (minval
			       (decf-range (incf-range 2 (startpos instr))
					   (startpos (dest source)))))))) 
    (vector #x10 (low-byte source))))

(defmethod emit-instruction (instr (i (eql 'ei)))
  (vector #xFB))

(defmethod emit-instruction (instr (i (eql 'ex)))
  (typecase (target instr)
    (memory-indirect (let ((base #xE3)
			   (prefix (prefix (target instr))))
		       (if prefix (vector prefix base) (vector base))))
    (register (let ((reg (register (target instr))))
		(case reg
		  (af (vector #x08))
		  ((de hl) (vector #xEB)))))))

(defmethod emit-instruction (instr (i (eql 'exx)))
  (vector #xD9))

(defmethod emit-instruction (instr (i (eql 'halt)))
  (vector #x76))

(defmethod emit-instruction (instr (i (eql 'im)))
  (unless (typep (target instr) 'immediate)
    (error 'incorrect-register
	   :format-control "Mnemonic IM requires an immediate, not ~A"
	   :format-arguments (list (target instr))))
  (case (value (target instr))
    ((0 1 2) (vector #xED (aref #(#x46 #x56 #x5E) (value (target instr)))))
    (t (error 'incorrect-register
	   :format-control "Mnemonic IM expects values 0, 1 or 2 not ~A"
	   :format-arguments (list (value (target instr)))))))

(defmethod emit-instruction (instr (i (eql 'in)))
  (let ((source (source instr)))
    (typecase source
      (immediate (vector #xDB (low-byte source)))
      (memory-indirect
       (vector #xED (+ #x40 (* (compute-register (target instr) t) 8)))))))

(defmethod emit-instruction (instr (i (eql 'ind)))
  (vector #xED #xAA))

(defmethod emit-instruction (instr (i (eql 'indr)))
  (vector #xED #xBA))

(defmethod emit-instruction (instr (i (eql 'ini)))
  (vector #xED #xA2))

(defmethod emit-instruction (instr (i (eql 'inir)))
  (vector #xED #xB2))

(defmethod emit-instruction (instr (i (eql 'inc)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((prefix (prefix source))
			     (base #x34))
			 (if prefix
			     (vector prefix base (low-byte source))
			   (vector base))))
      (register (case (size source)
		  (8 (let ((base #x04)
			   (reg (compute-register source)))
		       (vector (logior reg base))))
		  (16 (let ((base #x03)
			    (prefix (prefix source))
			    (reg (ash (compute-register source t) 4)))
			(if prefix
			    (vector prefix (logior base reg))
			  (vector (logior base reg))))))))))

(defmethod emit-instruction (instr (i (eql 'jp)))
  (let ((base 195)
	(target (source instr)))
    (when (flags instr)
      (setf base (logior (logxor base 1) (compute-flags (flags instr)))))
    (if (typep target 'memory-indirect)
	(if (prefix target)
	    (vector (prefix target) #xE9)
	  (vector #xE9))
      (vector base (low-byte target) (high-byte target)))))

(defmethod emit-instruction (instr (i (eql 'jr)))
  (let ((base 24)
	(target (low-byte (source instr))))
    (when (flags instr)
      (setf base (logior #b00100000 (compute-flags (flags instr)))))
    (vector base target)))

(defmethod emit-instruction (instr (i (eql 'ld)))
  (let ((source (source instr))
	(target (target instr)))
    (typecase target
      (memory-direct 
       (case (register source)
	 (a (vector #x32 (low-byte target) (high-byte target)))
	 ((cl:or bc de sp)
	  (let ((base #x43)
		(reg (ash (compute-register source t) 4)))
	    (vector #xED (logior base reg) (low-byte target) (high-byte target))))
	 (t (let ((prefix (prefix source)))
	      (if prefix
		  (vector prefix #x22 (low-byte target) (high-byte target))
		(vector #x22 (low-byte target) (high-byte target)))))))
      
      (memory-indirect
       (case (register target)
	 (bc (vector #x02))
	 (de (vector #x12))
	 (t (let ((prefix (prefix target)))
	      (typecase source
		(immediate
		 (if prefix
		     (vector prefix #x36 (low-byte target) (low-byte source))
		   (vector #x36 (low-byte source)))) 
		(register
		 (let ((base #x70)
		       (reg (compute-register source t)))
		   (if prefix
		       (vector prefix (logior base reg) (low-byte target))
		     (vector (logior base reg)))))))))) 
      (register 
       (case (size target)
	 (8 
	  (let ((base #x40)
		  (treg (compute-register target))
		  (prefix (prefix source)) 
		  rval)
	    (when (eq (register target) 'a)
	      (cond ((typep source 'memory-direct)
		     (setf rval
			   (vector #x3a (low-byte source) (high-byte source))))
		    ((cl:and (typep source 'memory-indirect)
			     (member (register source) '(bc de)))
		     (setf
		      rval
		      (vector
		       (logior #x0a (ash (compute-register source t) 4)))))
		    ((typep source 'register)
		     (case (register source)
		       (i (setf rval (vector #xED #x57)))
		       (r (setf rval (vector #xED #x5E)))))))
	      (if rval
		  rval
		(let ((sreg (compute-register source t)))
		  (typecase source
		    (memory-indirect
		     (if prefix
			 (vector prefix (logior #x46 treg) (low-byte source))
		       (vector (logior #x46 treg))))
		    (immediate (vector (logior #x06 treg (low-byte source)))) 
		    (register (vector (logior base treg sreg))))))))
	 (16 
	  (case (register target)
	       ((cl:or bc de) 
		(typecase source
		  ((cl:or asmlabel immediate)
		   (let ((base #x01)
			 (reg (ash (compute-register target t) 4)))
		     (vector (logior reg base)
			     (low-byte source) (high-byte source))))
		  (t (let ((base #x4B)
			   (reg (ash (compute-register source t) 4)))
		       (vector #xED (logior base reg)
			       (low-byte target) (high-byte target))))))
	       (t 
		(let ((base (typecase source
			      ((cl:or asmlabel immediate) #x21)
			      (t #x2A)))
		      (prefix (prefix target)))
		  (if prefix
		      (vector prefix base (low-byte source) (high-byte source))
		    (vector base (low-byte source) (high-byte source))))))))))))

(defmethod emit-instruction (instr (i (eql 'ldd)))
  (vector #xED #xA8))
(defmethod emit-instruction (instr (i (eql 'lddr)))
  (vector #xED #xB8))
(defmethod emit-instruction (instr (i (eql 'ldi)))
  (vector #xED #xA0))
(defmethod emit-instruction (instr (i (eql 'ldir)))
  (vector #xED #xB0))
(defmethod emit-instruction (instr (i (eql 'neg)))
  (vector #xED #x44))
(defmethod emit-instruction (instr (i (eql 'nop)))
  (vector #x00))
(defmethod emit-instruction (instr (i (eql 'or)))
  (typecase (source instr)
    (memory-indirect (let ((base #xB6)
			   (prefix (prefix (source instr))))
		       (if prefix
			   (vector prefix base (low-byte (source instr)))
			 (vector base))))
    (immediate (vector #xF6 (low-byte (source instr))))
    (register (let ((base #xB0)
		    (reg (compute-register (source instr) t)))
		(vector (logior base reg))))))
(defmethod emit-instruction (instr (i (eql 'otdr)))
  (vector #xED #xBB))
(defmethod emit-instruction (instr (i (eql 'otir)))
  (vector #xED #xB3))
(defmethod emit-instruction (instr (i (eql 'out)))
  (case (target instr)
    (memory-indirect-immediate (vector #xD3 (low-byte (target instr))))
    (memory-indirect (let ((reg (compute-register (source instr) t))
			   (base #x41))
		       (vector #xED (+ (* 8 reg) base))))))
(defmethod emit-instruction (instr (i (eql 'outd)))
  (vector #xED #xAB))
(defmethod emit-instruction (instr (i (eql 'outi)))
  (vector #xED #xA3))
(defmethod emit-instruction (instr (i (eql 'pop)))
  (let ((base #xC1)
	(reg (ash (compute-register (target instr) t) 4))
	(prefix (prefix (target instr))))
    (if prefix
	(vector prefix (logior base reg) )
      (vector (logior base reg)))))
(defmethod emit-instruction (instr (i (eql 'push)))
  (let ((base #xC5)
	(reg (ash (compute-register (target instr) t) 4))
	(prefix (prefix (target instr))))
    (if prefix
	(vector prefix (logior base reg) )
      (vector (logior base reg)))))
(defmethod emit-instruction (instr (i (eql 'res)))
  (let ((pos (value (target instr)))
	(prefix (prefix (source instr)))
	(offset (compute-register (source instr) t))
	(source (source instr)))
    (when (cl:or (typep (source instr) 'memory-indirect)
		 (typep (source instr) 'memory-indirect-indexed))
      (setf offset (logior offset (ash offset 1))))
    (let ((op (+ #x80 (* pos 8) offset)))
      (if prefix
	  (vector prefix #xCB (low-byte source) op)
	(vector #xCB op)))))
(defmethod emit-instruction (instr (i (eql 'ret)))
  (if (flags instr)
      (let ((base #xC0)
	    (flags (compute-flags (flags instr))))
	(vector (logior base flags))) 
    (vector #xc9)))
(defmethod emit-instruction (instr (i (eql 'reti)))
  (vector #xED #x4D))
(defmethod emit-instruction (instr (i (eql 'retn)))
  (vector #xED #x45))
(defmethod emit-instruction (instr (i (eql 'rl)))
  (typecase (source instr)
    (memory-indirect (let ((prefix (source instr)))
		       (if prefix
			   (vector prefix #xCB (low-byte (source instr)) #x16)
			 (vector #xCB #x16))))
    (register (let ((base #x10)
		    (reg (compute-register (source instr) t)))
		(vector #xCB (logior base reg))))))
(defmethod emit-instruction (instr (i (eql 'rla)))
  (vector #x17))
(defmethod emit-instruction (instr (i (eql 'rlc)))
  (typecase (source instr)
    (memory-indirect (let ((prefix (source instr)))
		       (if prefix
			   (vector prefix #xCB (low-byte (source instr)) #x06)
			 (vector #xCB #x06))))
    (register (let ((base #x00)
		    (reg (compute-register (source instr) t)))
		(vector #xCB (logior base reg))))))
(defmethod emit-instruction (instr (i (eql 'rlca)))
  (vector #xCB #x07))
(defmethod emit-instruction (instr (i (eql 'rld)))
  (vector #xED #x6F))
(defmethod emit-instruction (instr (i (eql 'rr)))
  (typecase (source instr)
    (memory-indirect (let ((prefix (source instr)))
		       (if prefix
			   (vector prefix #xCB (low-byte (source instr)) #x1E)
			 (vector #xCB #x1E))))
    (register (let ((base #x18)
		    (reg (compute-register (source instr) t)))
		(vector #xCB (logior base reg))))))
(defmethod emit-instruction (instr (i (eql 'rra)))
  (vector #x1F))
(defmethod emit-instruction (instr (i (eql 'rrc)))
  (typecase (source instr)
    (memory-indirect (let ((prefix (source instr)))
		       (if prefix
			   (vector prefix #xCB (low-byte (source instr)) #x0E)
			 (vector #xCB #x0E))))
    (register (let ((base #x08)
		    (reg (compute-register (source instr) t)))
		(vector #xCB (logior base reg))))))
(defmethod emit-instruction (instr (i (eql 'rrca)))
  (vector #x0F))
(defmethod emit-instruction (instr (i (eql 'rrd)))
  (vector #xED #x67))
(defmethod emit-instruction (instr (i (eql 'rst)))
  (unless (typep (source instr) 'immediate)
    (error 'incorrect-register :format-control "Expected immediate value, not ~a" :format-arguments (list (source instr))))
  (let ((v (logand (value (source instr)) #x38))
	(base #xC7))
    (vector (+ base v))))

(defmethod emit-instruction (instr (i (eql 'sdc)))
  (let ((source (source instr))
	(target (target instr)))
    (case (size target)
      (8 (let ((base #x98))
	   (typecase source
	     (memory-indirect
	      (let ((prefix (prefix source)))
		(if prefix
		    (vector prefix (logior base #x06) (low-byte source)))))
	     (immediate (vector (logior base #x46) (low-byte source)))
	     (register (vector (logior base (compute-register source t)))))))
      (16 (let ((base #x42)
		(prefix (prefix target))
		(reg (ash (compute-register source t) 4)))
	    (if prefix
		(vector prefix #xED (logior base reg))
	      (vector #xED (logior base reg))))))))

(defmethod emit-instruction (instr (i (eql 'scf)))
 (vector #x37))

(defmethod emit-instruction (instr (i (eql 'set)))
  (let ((pos (value (target instr)))
	(prefix (prefix (source instr)))
	(offset (compute-register (source instr) t))
	(source (source instr)))
    (when (cl:or (typep (source instr) 'memory-indirect)
		 (typep (source instr) 'memory-indirect-indexed))
      (setf offset (logior offset (ash offset 1))))
    (let ((op (+ #xC0 (* pos 8) offset)))
      (if prefix
	  (vector prefix #xCB (low-byte source) op)
	(vector #xCB op)))))
(defmethod emit-instruction (instr (i (eql 'sla)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((base #x26)
			     (prefix (prefix source)))
			 (if prefix
			     (vector prefix #xCB (low-byte source) base)
			   (vector #xCB base))))
      (register (let ((base #x20)
		      (reg (compute-register source t)))
		  (vector #xCB (logior base reg)))))))
(defmethod emit-instruction (instr (i (eql 'sra)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((base #x2E)
			     (prefix (prefix source)))
			 (if prefix
			     (vector prefix #xCB (low-byte source) base)
			   (vector #xCB base))))
      (register (let ((base #x28)
		      (reg (compute-register source t)))
		  (vector #xCB (logior base reg)))))))
(defmethod emit-instruction (instr (i (eql 'srl)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((base #x3E)
			     (prefix (prefix source)))
			 (if prefix
			     (vector prefix #xCB (low-byte source) base)
			   (vector #xCB base))))
      (register (let ((base #x38)
		      (reg (compute-register source t)))
		  (vector #xCB (logior base reg)))))))

(defmethod emit-instruction (instr (i (eql 'sub)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((prefix (prefix source))
			     (base #x96)) 
			 (if prefix
			     (vector prefix base (low-byte source))
			   (vector base))))
      (immediate (vector #xD6 (low-byte source)))      
      (register (let ((reg (compute-register source t))
		      (base #x90))
		  (vector (logior base reg)))))))

(defmethod emit-instruction (instr (i (eql 'xor)))
  (let ((source (source instr)))
    (typecase source
      (memory-indirect (let ((prefix (prefix source))
			     (base #xAE)) 
			 (if prefix
			     (vector prefix base (low-byte source))
			   (vector base))))
      (immediate (vector #xEE (low-byte source)))      
      (register (let ((reg (compute-register source t))
		      (base #xA8))
		  (vector (logior base reg)))))))
