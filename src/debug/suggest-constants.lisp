;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-DEBUG; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(in-package "GARNET-DEBUG")

(eval-when (eval load compile)
  (export '(record-from-now suggest-constants find-formulas explain-formulas)))



;;; Start up the application, then call (RECORD-FROM-NOW) when the application
;;; is in a steady state (e.g., after loading and typing do-go for a demo).
;;; Syntax:
;;; (RECORD-FROM-NOW)
;;;
;;; After this, the main function is called SUGGEST-CONSTANTS:
;;; (SUGGEST-CONSTANTS object &key max (recompute-p T) (level 1))
;;; This functions prints all the schema/slot pairs which, if made constant,
;;; would eliminate some formulas.
;;; Normally, you only need to specify the <object>, which is typically the
;;; top-level aggregate of your application's window.
;;;
;;; Specify the <max> if you want to stop printing after that many pairs
;;; (the default is to print all pairs).
;;; Set <recompute-p> to NIL if you do not need to reexamine all the objects
;;; and you trust what was computed earlier.
;;; The default value of <level> (level = 1) causes the function to print only
;;; pairs which would, by themselves, eliminate some formula.  If <level> is
;;; made higher, pairs will be printed if the dependent formulas have a total
;;; number of depended pairs which is less than or equal to the <level>.
;;;
;;;
;;;
;;; A function that also might be useful is:
;;; (FIND-FORMULAS  aggregate  &optional  only-totals-p  limit  from)
;;; - The <aggregate> is usually a window aggregate.  All formulas in it and
;;;   all of its (recursive) components are examined.
;;; - If <only-totals-p> is non-nil, the function simply prints out the total
;;;   number of formulas that have not been reevaluated.  If it is nil,
;;;   the names of all the formulas are printed.  Note that formulas that
;;;   were NEVER evaluated are always printed by name, independent of the value
;;;   of this parameter.
;;; - If <limit> is specified and only-totals-p is nil, at most <limit>
;;;   formula names are printed.  If <limit> is nil, all formula names are
;;;   printed.  The default value is 50.
;;; - You will seldom need to specify the <from> parameter.  This allows you
;;;   to print out formulas that have been unevaluated since <from>.  The
;;;   default value is the number returned by the last call to RECORD-FROM-NOW;
;;;   specifying a smaller number reduces the number of formulas that are
;;;   printed out, since formulas that were evaluated earlier are discarded.
;;;



(defparameter *start-recording* 0
  "Sweep-mark counter at the beginning of the current evaluation")


(defparameter *counter* 0
  "Counter for formulas that were unevaluated since the last time.")
(defparameter *evaluated-counter* 0)


;;;; RECORD-FROM-NOW
;;;
(defun record-from-now ()
  (setf *start-recording* kr::*sweep-mark*)
  (format t "Now exercise the interface.  When you are done, call the function~%" )
  (format t "  (SUGGEST-CONSTANTS  agg  &key  max)~%")
  (format t "to print out what formulas would be eliminated if certain slots were~%")
  (format t "declared constants.~%")
  (format t "~%")
  (format t "You may also want to use the reverse function,~%")
  (format t "  (EXPLAIN-FORMULAS  agg  &optional  limit  eliminate-useless-p),~%")
  (format t "to find out why formulas that were not evaluated did not come up as constant.~%"))



;;;; FIND-FORMULAS
;;; 
;;; ONLY-TOTALS-P: print only number of formulas, instead of individual
;;; formula names.
;;;
(defun find-formulas (object &optional (only-totals-p T)
			     (limit 50)
			     (from *start-recording*) recursive-p)
  (unless recursive-p
    (setf *counter* 0)
    (setf *evaluated-counter* 0))
  (unless limit
    (setf limit most-positive-fixnum))
  (doslots (slot object T)
    (let ((value (get-value object slot)))
      (if (formula-p value)
	(let ((number (g-formula-value value :number)))
	  (if (< number from)
	    (progn
	      (incf *counter*)
	      (if (<= *counter* limit)
		(cond ((zerop number)
		       (format t "unevaluated:  ~S~%" value))
		      ((not only-totals-p)
		       (format t "  ~S" value)
		       (when (zerop (mod *counter* 4))
			 (terpri))))
		(if (and (not only-totals-p) (= *counter* limit))
		  (format t " ... more formulas omitted~%"))))
	    (incf *evaluated-counter*))))))
  (dolist (child (g-local-value object :components))
    (find-formulas child only-totals-p limit from T))
  (unless recursive-p
    (let ((when (if (= from *start-recording*)
		  "the last call to RECORD-FROM-NOW"
		  from)))
      (format t "~%Total of ~S formulas unevaluated since ~A.~%"
	      *counter* when)
      (format t "~%Total of ~S formulas evaluated since ~A.~%"
	      *evaluated-counter* when))))


(defun count-all-formulas (object)
  (let ((number 0))
    (doslots (slot object T)
      (let ((value (get-value object slot)))
	(if (formula-p value)
	  (incf number))))
    (dolist (child (g-local-value object :components))
      (incf number (count-all-formulas child)))
    number))



;;; This is used to make a formula go away, if everything it depends on is
;;; now constant.
;;;
(defun make-go-away (formula)
  (setf (kr::a-formula-number formula) 0)
  (kr::re-evaluate-formula (g-formula-value formula :schema)
			   (g-formula-value formula :slot)
			   formula
			   NIL))


(defparameter *formulas-on-why-not* (make-hash-table))


(defun find-key (dep slot list)
  (dolist (entry list)
    (if (and (eq (car entry) dep) (eq (cdr entry) slot))
      (return-from find-key T))))


(defun process-unevaluated (schema the-slot formula make-go-away print-p)    
  (let ((reasons nil)
	(repeated nil)
	(working-on (gethash formula *formulas-on-why-not*)))
    (unless (and (numberp working-on) (plusp working-on))
      ;; Do nothing if we are already processing this formula!
      (dolist (schema-slot (kr::i-depend-on schema the-slot))
	(let ((dep (car schema-slot))
	      (slot (cdr schema-slot)))
	  ;; The <formula> depends on this slot.
	  (unless (slot-constant-p dep slot)
	    (let ((value (get-value dep slot)))
	      ;; The <slot> in schema <dep> is not constant.
	      (if (and (formula-p value)
		       (gethash value *formulas-on-why-not*))
		  (progn
		    (if print-p
			(format t "  formula ~S (listed above) on ~S ~S~%"
				value dep slot))
		    (setf repeated (cons dep slot)))
		  (let (sub-reasons)
		    (unless (find-key dep slot reasons)
		      ;; Avoid infinite loops!
		      (setf sub-reasons
			    (why-not-as-list dep slot make-go-away print-p)))
		    (if (and sub-reasons (listp sub-reasons))
			(setf reasons (union sub-reasons reasons
					     :test #'equal))
			(unless (find-key dep slot reasons)
			  (push (cons dep slot) reasons))))))))))
    (unless (or reasons repeated)
      (if make-go-away
	(progn
	  (make-go-away formula)
	  (when print-p
	    (unless (formula-p (get-value schema the-slot))
	      (format t "  ** eliminated formula on ~S.~S.~%"
		      schema the-slot))))
	(if print-p
	  (format t "~%  *** ~S ~S should go away if reevaluated~%"
		  schema the-slot))))
    (when repeated
      (push repeated reasons))
    reasons))



;;; Like GD:WHY-NOT-CONSTANT, but returns the results in a list.  Also,
;;; recurses until it finds rock-bottom, i.e., propagates to the most distant
;;; reaches of the dependency graph.
;;;
(defun why-not-as-list (schema the-slot make-go-away print-p)
  (let ((formula (get-value schema the-slot))
	(is-constant (slot-constant-p schema the-slot)))
    (if (formula-p formula)
      ;; A formula
      (if is-constant
	(return-from why-not-as-list :CONSTANT)
	(if (zerop (g-formula-value formula :number))
	  ;; Not yet evaluated.
	  (return-from why-not-as-list :UNEVALUATED)
	  ;; Evaluated but not constant.
	  (process-unevaluated schema the-slot formula make-go-away print-p)))
      ;; Not a formula
      (if is-constant
	:CONSTANT
	:VALUE))))



;;;; EXPLAIN-FORMULAS
;;;
(defun explain-formulas (object &optional (print-limit 50) make-go-away recursive-p)
  (unless recursive-p
    (setf *counter* 0)
    (setf *evaluated-counter* 0)
    (clrhash *formulas-on-why-not*))
  (unless print-limit
    (setf print-limit most-positive-fixnum))
  (doslots (slot object T)
    (let ((value (get-value object slot)))
      (if (formula-p value)
	(let ((number (g-formula-value value :number))
	      (schema (g-formula-value value :schema))
	      (slot (g-formula-value value :slot)))
	  (if (< number *start-recording*)
	    (progn
	      (incf *counter*)
	      (let* ((print-p (<= *counter* print-limit))
		     (unevaluated-p (zerop number))
		     reasons)
		(if unevaluated-p
		  (if print-p
		    (format t "~%unevaluated formula:  ~S  on ~S ~S~%" value object slot))
		  (progn
		    (if print-p
		      (format t "~%~S  (~(~S~) ~S) not constant because of:~%" value schema slot))
		    (setf (gethash value *formulas-on-why-not*) 0) ; working on
		    (if (or print-p make-go-away)
		      (setf reasons (why-not-as-list schema slot make-go-away print-p)))
		    (when (listp reasons)
		      (dolist (reason reasons)
			(let ((r-schema (car reason))
			      (r-slot (cdr reason)))
			  (when print-p
			    (if (member r-slot (g-value r-schema :MAYBE-CONSTANT))
			      (format t "  [slot is in :MAYBE-CONSTANT]"))
			    (format t "  ~(~S~) ~S~%" r-schema r-slot)))))
		    (setf (gethash value *formulas-on-why-not*) 1)
		    (when (= *counter* print-limit)
		      (format t " ... more formulas omitted~%"))))))
	    (incf *evaluated-counter*))))))
  (dolist (child (g-local-value object :components))
    (explain-formulas child print-limit make-go-away T))
  (unless recursive-p
    (format
     t
     "Total of ~D formulas unevaluated since the last call to RECORD-FROM-NOW.~%" *counter*)
    (format
     t
     "Total of ~D formulas evaluated since the last call to RECORD-FROM-NOW.~%"
     *evaluated-counter*)))



(defun explain-formula (formula &optional make-go-away)
  (let ((number (g-formula-value formula :number))
	(schema (g-formula-value formula :schema))
	(slot (g-formula-value formula :slot)))
    (when (< number *start-recording*)
      (let* ((unevaluated-p (zerop number))
	     (reasons (unless unevaluated-p
			(why-not-as-list schema slot make-go-away T))))
	(if unevaluated-p
	  (format t "~%unevaluated formula:  ~S~%~%" formula)
	  (progn
	    (format t "~%~S  (~S ~S) not constant because of:~%" formula schema slot)
	    (format t "~{  ~(~S~)~%~}" reasons)
	    (setf (gethash formula *formulas-on-why-not*) formula)))))))



(defvar *inverted-table* (make-hash-table :test #'equal))


(defun what-if-constant (schema slot)
  (when (slot-constant-p schema slot)
    (format t "Slot ~S in ~S is already constant!~%" slot schema)
    (return-from what-if-constant NIL))
  (let ((formulas (gethash (cons schema slot) *inverted-table*)))
    (if formulas
      (let ((may nil))
	(setf formulas (sort formulas #'(lambda (a1 a2) (<= (cdr a1) (cdr a2)))))
	;; Test for circularities here.
	(dolist (formula formulas)
	  (setf formula (car formula))
	  (when (and (eq (g-formula-value formula :SCHEMA) schema)
		     (eq (g-formula-value formula :SLOT) slot))
	    (format t "Slot  ~S  ~S  has a formula which depends on itself.~%"
		    schema slot)
	    (return-from what-if-constant NIL)))
	(format t "Making  ~S  ~S  constant would eliminate:~%"
		schema slot)
	(dolist (s '(:MAYBE-CONSTANT :PARAMETERS :OUTPUT))
	  (if (member slot (g-value schema s))
	    (format t "      (this slot is in the ~S slot)~%" s)))
	(dolist (entry formulas)
	  (if (= (cdr entry) 1)
	    (format t "  ~S  (on ~(~S~) ~S)~%" (car entry)
		    (g-formula-value (car entry) :SCHEMA)
		    (g-formula-value (car entry) :SLOT))))
	(dolist (entry formulas)
	  (when (> (cdr entry) 1)
	    (unless may
	      (format t "    This slot is also used by the following formulas:~%")
	      (setf may T))
	    (format t "    ~S (which depends on ~D other slots)~%"
		    (car entry) (1- (cdr entry))))))
      (format t "No formulas would be eliminated by setting ~S ~S to constant.~%"
	      schema slot))))



(defun compute-inverted (object &optional recursive-p)
  (unless recursive-p
    (setf *counter* 0)
    (setf *evaluated-counter* 0)
    (clrhash *inverted-table*)
    (clrhash *formulas-on-why-not*))
  (doslots (slot object T)
    (let ((value (get-value object slot)))
      (if (formula-p value)
	(let ((number (g-formula-value value :number))
	      (schema (g-formula-value value :schema))
	      (slot (g-formula-value value :slot)))
	  (if (< number *start-recording*)
	    (progn
	      (incf *counter*)
	      (setf (gethash value *formulas-on-why-not*) 0) ; working on
	      (let ((reasons (why-not-as-list schema slot T NIL)))
		(when (listp reasons)
		  (let* ((how-many-reasons (length reasons))
			 (marker (if reasons (cons value how-many-reasons))))
		    (dolist (reason reasons)
		      (push marker (gethash reason *inverted-table*)))))
		(setf (gethash value *formulas-on-why-not*) 1)))
	    (incf *evaluated-counter*))))))
  (dolist (child (g-local-value object :components))
    (compute-inverted child T))
  (unless recursive-p
    (format t "Total of ~D formulas unevaluated since last RECORD-FROM-NOW.~%~%"
	    *counter*)
    (format t "Total of ~D formulas evaluated since last RECORD-FROM-NOW.~%~%"
	    *evaluated-counter*)))



;;;; SUGGEST-CONSTANTS
;;; Print all the schema/slot pairs that, if made constant, would eliminate some
;;; formulas.  The default (level = 1) is to print only pairs that would, by
;;; themselves, eliminate some formula.  If <level> is made higher, pairs will
;;; be printed if the dependent formulas have a total number of depended pairs
;;; that is less than or equal to the <level>.
;;; Specify <max> if you want to stop printing after that many pairs.
;;; Set <recompute-p> to NIL if you do not need to reexamine all the objects
;;; and you trust what was computed earlier.
;;;
(defun suggest-constants (object &key (max most-positive-fixnum)
				 (recompute-p T) (level 1))
  (when recompute-p
    (format t "Analyzing objects...~%")
    (compute-inverted object))
  (let ((counter max))
    (maphash #'(lambda (key value)
		 (setf (gethash key *inverted-table*)
		       (setf value (sort value #'(lambda (a1 a2)
						   (<= (cdr a1) (cdr a2))))))
		 (when (<= (cdr (first value)) level)
		   (what-if-constant (car key) (cdr key))
		   (terpri)
		   (when (minusp (decf counter))
		     ;; Stop printing after <max> pairs.
		     (format t "... ~D more slots omitted~%"
			     (- (hash-table-count *inverted-table*) max))
		     (return-from suggest-constants NIL))))
	     *inverted-table*)))
