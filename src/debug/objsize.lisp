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


;;; objsize.lisp -- compute the storage of a kr object including
;;; constraints.  Suggested use:
;;;     Type (objbytes <objname>) to see how big one object is.
;;;     Type (aggbytes <aggname>) to see how big an aggregate and
;;; components are, and also get statistics.
;;;     The aggbytes function will also take a list of aggregates,
;;; a window, or a list of windows.  For example, type
;;; (aggbytes editor::*lapidary-subwindows*) to get lapidary measurements.
;;;     Set *avoid-shared-values* to t to use hashing.
;;;     Set *avoid-equal-values* to t to measure *potential* sharing.  This
;;; will count distinct values using #'equal to do hashing
;;;     Set *count-symbols* to t to measure space taken by schema names.

(in-package "GARNET-DEBUG")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(OBJBYTES AGGBYTES INTERBYTES
	    *avoid-shared-values* *avoid-equal-values* *count-symbols*
	    COUNT-FORMULAS WHY-NOT-CONSTANT)))

;; The schema structure size is 2 fields * 4 bytes + 4 bytes type info.
;; Formulas have 12 fields * 4 bytes + 4 bytes.
;;
(defparameter schema-size 12)
(defparameter slot-size 16 "Size in bytes of a short slot structure")
(defparameter formula-size 64)
(defparameter cons-size 8)

;; the size of a cmu common lisp symbol in bytes:
(defun symbol-size (symbol)
  (+ 28 (* 4 (ceiling (length (symbol-name symbol)) 4))))

(defparameter *avoid-shared-values* nil)
(defparameter *avoid-equal-values* nil)
(defparameter *count-symbols* nil)

(defvar value-hash nil)

(defvar num-objs)
(defvar num-aggs)
(defvar num-inter)
(defvar num-forms)
(defvar num-isaforms)
(defvar siz-objs)
(defvar siz-aggs)
(defvar siz-inter)
(defvar siz-forms)
(defvar siz-isaforms)
(defvar siz-update)
(defvar biggest-obj)
(defvar biggest-siz)
(defvar symbol-bytes)

(defvar type-hash nil)

(defun datasize (data formula-slot slot-name)
  (cond ((null data) 0)
	(formula-slot (treesize data))  ;don't recurse on formulas
	((not (listp data))
	 (if (formula-p data)
	     (objbytes data t)
	     (if (opal::update-info-p data)
		 (* 4 (1+ opal::number-of-slots-of-update-info-struct))
		 (if (eq slot-name :update-slots-values)
		     (* 4 (length data))
		     0))))
	((formula-p (car data))
	 (+ (objbytes (car data) t) (length data)))
	(t (treesize data))))


;;; DZG - fixed to hash properly for new-style IS-A slots.
;;;
(defun objbytes (obj &optional (is-formula (formula-p obj)))
  "Calculate the size in bytes of an object."
  (if is-formula
    (formula-bytes obj)
    (let ((size (+ schema-size
		   (* 4 (1+
			 #+GARNET-BINS (array-dimension (kr::schema-bins obj) 0)
			 #-GARNET-BINS (hash-table-count (kr::schema-bins obj))
			 ))))
	  obj-isa stats)
      (kr::iterate-slot-value (obj T NIL NIL)
	(let ((entry kr::iterate-slot-value-entry))
	  (incf size slot-size)		; count storage for slot
	  (if (kr::full-sl-p entry)
	    (incf size 4))		; slot with dependents - 1 more word
	  (let ((s (datasize kr::value nil kr::slot)))
	    (incf size s))
	  (if (kr::full-sl-p entry)
	    (let ((dep (kr::full-sl-dependents entry)))
	      (if (listp dep)
		(incf size (* cons-size (length dep))))))))
      ;; the name slot is not a normal slot
      (if (and *count-symbols* (symbolp (kr::schema-name obj)))
	(let ((sym (symbol-size (kr::schema-name obj))))
	  (incf symbol-bytes sym)
	  (incf size sym)))
      ;; compute statistics if we're called from aggbytes or interbytes
      (when type-hash
	(cond ((is-a-p obj opal:aggregate)
	       (incf num-aggs)
	       (incf siz-aggs size))
	      ((is-a-p obj inter:interactor)
	       (incf num-inter)
	       (incf siz-inter size))
	      (t
	       (incf num-objs)
	       (incf siz-objs size)))
	;; compute detailed statistics
	(setf obj-isa (first (get-value obj :is-a)))
	(setf stats (gethash obj-isa type-hash))
	(cond (stats
	       (incf (car stats))
	       (incf (cdr stats) size))
	      (t
	       (setf (gethash obj-isa type-hash) (cons 1 size))))	   
	(cond ((> size biggest-siz)
	       (setf biggest-obj obj)
	       (setf biggest-siz size))))
      size)))


(defun formula-bytes (obj)
  (let* ((depends-on (g-formula-value obj :DEPENDS-ON))
	 (size (+ formula-size
		 (* cons-size (if (listp depends-on)
				  (length depends-on)
				  1))
		 (* cons-size (length (g-formula-value obj :PATH)))
		 (datasize (g-formula-value obj :CACHED-VALUE) t nil)
		 (if (g-formula-value obj :IS-A)
		     0			; inherited formula
		     ;; local formula - add lambda, function
		     (datasize (g-formula-value obj :LAMBDA) T nil)))))
    (if type-hash
	(if (g-formula-value obj :IS-A)
	    (progn
	      (incf num-isaforms)
	      (incf siz-isaforms size))
	    (progn
	      (incf num-forms)
	      (incf siz-forms size))))
    size))

    
(defun treesize (data)
  (cond ((null data) 0)
	((consp data)
	        ;; see if we have entered this value in the hash table
	 (cond ((and value-hash (gethash data value-hash)) 0)
	       ;; closures can have nasty things inside like the special
	       ;; internal marker that makes variables be unbound --
	       ;; if 'data got bound to this, we'd be in big trouble
	 #+cmu ((eq (car data) 'LISP::%LEXICAL-CLOSURE%) 0)
     #+allegro ((eq (car data) 'EXCL::.LEXICAL-CLOSURE%) 0)
	       (t
		;; save the value in the hash table if there is one
		(if value-hash (setf (gethash data value-hash) t))
		(+ cons-size (treesize (car data)) (treesize (cdr data))))))
	(t 0))) ; do not count non-lists, e.g. symbols and structures


;; get ready to keep statistics: this is shared by aggbytes and interbytes
;;
(defun statistics-setup ()
  (setf num-objs 0)
  (setf num-aggs 0)
  (setf num-inter 0)
  (setf num-forms 0)
  (setf num-isaforms 0)
  (setf siz-objs 0)
  (setf siz-aggs 0)
  (setf siz-inter 0)
  (setf siz-forms 0)
  (setf siz-isaforms 0)
  (setf symbol-bytes 0)
  (setf biggest-obj nil)
  (setf biggest-siz -1)
  (if *avoid-shared-values*
      (setf value-hash (make-hash-table :test #'eq :size 10000)))
  (if *avoid-equal-values*
      (setf value-hash (make-hash-table :test #'equal :size 10000)))
  (setf type-hash (make-hash-table :test #'eq :size 100)))


(defun stats-helper (name size how-many)
  (format t "~%  ~4D ~32A" how-many name)
  (when (> how-many 0)
    (format t " ~5D bytes " size)
    (if (> how-many 1)
	(format t " (average ~5D)" (round size how-many)))))



;; print out statistics -- called by aggbytes and interbytes
;;
(defun statistics-wrapup (size verbose)
  (format t "There were a total of ~A objects and ~A bytes:"
	  (+ num-objs num-aggs num-inter num-forms num-isaforms) size)
  (stats-helper "aggregates" siz-aggs num-aggs)
  (stats-helper "interactors" siz-inter num-inter)
  (stats-helper "formulas" siz-forms num-forms)
  (stats-helper "inherited formulas" siz-isaforms num-isaforms)
  (stats-helper "other objects" siz-objs num-objs)

  (cond ((null *count-symbols*)
	 (format t "~%Schema name storage was not counted"))
	((> symbol-bytes 0)
	 (format t "~%Schema names use ~5D bytes" symbol-bytes)))
  (when verbose
    (format t "~%~%Detailed breakdown by object type:")
    (maphash #'(lambda (key value)
		 (stats-helper key (cdr value) (car value))
		 #+TEST
		 (format t "~4D ~40A  (average ~5D)~%"
			 (car value) key (round (cdr value) (car value))))
	     type-hash)
    (format t "~%Biggest object: ~A (~D bytes).~%"
	    biggest-obj biggest-siz)
    (setf value-hash nil)
    (setf type-hash nil)))

  
(defun aggbytes (agg &optional (verbose t))
  "Count bytes in an aggregate and print a report."
  (let ((size 0) components)
    (if (listp agg)
	(setf components (find-components agg))
	(setf components (find-components (list agg))))
    (statistics-setup)
    (dolist (comp components)
      (incf size (objbytes comp)))
    (statistics-wrapup size verbose)
    size))


(defun interbytes (&optional windows (verbose t))
  "Count the size of interactors and print a report.  Specify the window to
  get only interactors for a given window."
  (let ((size 0)
	inter)
    ;; since in interactors T means "all windows", allow T here as well.
    (cond ((or (eq windows t)(null windows))
	   (setq windows inter::all-inter-windows))
	  ((schema-p windows)(setq windows (list windows))))
    (statistics-setup)
    (dolist (win windows)
      (when (schema-p win)
	(dolist (localassoc (g-value win :priority-level-assoc))
	  (dotimes (i (length (cdr localassoc)))
	    (setq inter (aref (cdr localassoc) i))
	    (incf size (objbytes inter))))))
    (statistics-wrapup size verbose)
    size))

;;; find all components of a gadget
;;;
(defun find-components (gadgets)
  (set-transitive-closure gadgets '(:components :children :aggregate)))


;;; find the transitive closure of a set of relations starting from schema.
;;; the results are added to components, which is needed for recursion.
;;;
(defun transitive-closure (schema relations &optional components)
  (pushnew schema components)
  (dolist (relation relations)
    ;; DZG - 4-7-1993    Using g-value-no-copy to avoid creating these slots
    ;; if they are not present in the object.
    (let ((values (kr::g-value-no-copy schema relation)))
      ; amickish - 5-11-94  when schema = WIN and relation = :components,
      ; we get kr::*no-value* for the variable VALUES
      (if (eq values kr::*no-value*) (setf values NIL))
      (if (listp values)
	  (dolist (s values)
	    (if (not (member s components))
		(setf components
		      (transitive-closure s relations components))))
	  (if (not (member values components))
		(setf components
		      (transitive-closure values relations components))))))
  components)


;;; find transitive closure of a set of relations starting from a set of
;;; schemata.
;;;
(defun set-transitive-closure (schemata relations)
  (let (components)
    (dolist (schema schemata)
      (setf components (transitive-closure schema relations components)))
    components))



;;; --------------------------------------------------


;;; Call this with the top aggregate of a window.  It will print out
;;; all remaining (i.e., non-constant) formulas, divided by schema.
;;;
;;; Example:
;;;  (count-formulas (g-value garnet-gadgets::gauge-win :aggregate))
;;;
(defun count-formulas (o &optional (total 0) (uneval 0) (top T))
  (let ((output nil))
    (kr::iterate-slot-value (o T NIL NIL)
      (when (formula-p kr::value)
	(unless output
	  (format t "~%object ~S:~%" o)
	  (setf output t))
	(if (zerop (g-formula-value kr::value :NUMBER))
	  (progn
	    (incf uneval)
	    ;; turn this on to get printing for unevaluated formulas.
	    (format t "  ~S(unevaluated)" kr::slot)
	    )
	  (format t "  ~S" kr::slot))
	(incf total))))
  (dolist (child (g-local-value o :components))
    (multiple-value-setq (total uneval)
      (count-formulas child total uneval nil)))
  (if top
    (format t "~%~%  ~D formula~:P (of which ~D are unevaluated).~%"
	    total uneval)
    (values total uneval)))


;;; Given a schema and a slot, tells you why the formula in the slot was
;;; not marked constant.  Prints the name of each schema / slot which is
;;; depended on by the formula, and which is not marked constant.
;;;
;;; Example:
;;; (why-not-constant MARK-13643 :left)
;;;   MARK-1281 :LINE-STYLE
;;;   OPAL:DEFAULT-LINE-STYLE :CAPS-STYLE
;;;   OPAL:DEFAULT-LINE-STYLE :LINE-THICKNESS
;;;
(defun why-not-constant (schema the-slot)
  (if (numberp schema)
    (setf schema (kr::s schema)))
  (let ((formula (get-value schema the-slot))
	(is-constant (slot-constant-p schema the-slot)))
    (if (formula-p formula)
      ;; A formula
      (if is-constant
	(format t "  The formula IS constant!~%")
	(if (zerop (g-formula-value formula :NUMBER))
	  ;; Not yet evaluated.
	  (format t "  The formula has not yet been evaluated.~%")
	  ;; Evaluated but not constant.
	  (kr::do-one-or-list (dep (g-formula-value formula :DEPENDS-ON))
	    (doslots (slot dep T)
	      (let ((entry (kr::slot-accessor dep slot)))
		(kr::do-one-or-list (f (kr::slot-dependents entry))
		  (if (eq formula f)
		    (unless (kr::is-constant (kr::sl-bits entry))
		      (format t "  ~S  ~S  is not constant~%" dep slot)))
		  (return)))))))
      ;; Not a formula
      (if is-constant
	(format t "  The slot IS constant!~%")
	(format t "  The slot does not contain a formula, and was not declared constant.~%")))))


