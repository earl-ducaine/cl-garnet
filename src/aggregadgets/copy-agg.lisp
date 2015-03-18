;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;


;;; Copy an aggregadget.
;;; 
;;; Roger B. Dannenberg, 1990
;;; Implementation details:
;;
;;  Copy works much like saving, except rather than writing out the structure,
;;  a duplicate structure is built.  The copy operation does not create
;;  instances because an instance of an aggregadget's parent would create parts
;;  that might not match the parts of the aggregate.  


(in-package "OPAL")
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(copy-gadget)))

(defvar *standard-element-slots*)


;;;  :INITIALIZE-COPY methods
;;   Required because you don't always want to call the :initialize method
;;   on the new copy.

(defmacro initialize-copy (orig copy)
  `(let ((the-orig ,orig)
	 (the-copy ,copy))
    (if (g-value the-copy :initialize-copy)
	(kr-send the-copy :initialize-copy the-orig the-copy)
	(kr-send the-copy :initialize the-copy))))

;; start at aggregate level to avoid copying components (which is what
;; initialize-method-aggregadget does).
(define-method :initialize-copy opal:aggregadget (orig copy)
  (declare (ignore orig))
  (kr-send opal:aggregate :initialize copy))

;; Same as aggregadget's :initialize-copy method, but install
;; update-slots-array for :fix-update-method (which would have
;; been done in initialize-method-aggrelist)
(define-method :initialize-copy opal:aggrelist (orig copy)
  (declare (ignore orig))
  (s-value copy :update-slots-values
	   (make-array (length (g-value copy :update-slots))))
  (kr-send opal:aggregate :initialize copy))



;;;----------------------------------------------------------------------------

(defun copy-gadget (agget name &optional new-parent)
  (let* ((copy (if name (create-schema name) (create-schema nil)))
	 (known-as (g-value agget :known-as))
	 (parent (g-value agget :parent))
	 (parent-proto (if parent (car (g-local-value parent :is-a))))
	 (normal-proto (if (and known-as parent-proto)
			   (g-local-value parent-proto known-as)))
	 (components (g-local-value agget :components))
	 (behaviors (g-local-value agget :behaviors)))
    (copy-slots copy agget normal-proto components behaviors new-parent)
    copy))


;; Copy the type declarations from the original object, unless types are
;; currently disabled.  Does nothing if the original slot had no type
;; declaration.
;;
(defun set-slot-type (copy agget slot)
  (when kr::*types-enabled*
    (let ((type (g-type agget slot)))
      (when type
	(s-type copy slot type)))))


(defun copy-slots (copy agget normal-proto components behaviors new-parent)
  (let ((proto (car (g-local-value agget :is-a)))
	;; dzg - use this call, which does not actually inherit the slot.
	(standard-slots (kr::g-value-no-copy agget :DO-NOT-DUMP-SLOTS))
	value values item-prototype-object)

    ;; set the IS-A slot
    (with-demons-disabled
      (let ((kr::*schema-is-new* t))
	(set-slot-type copy agget :is-a)
	(s-value copy :is-a (list proto))))

    (doslots (slot agget)

      ;; don't copy automatically generated slots
      (cond ((eq slot :known-as)
	     ;; copy :known-as even if it is a standard slot
	     (let ((value (g-value agget :known-as)))
	       (s-value copy :known-as value)
	       (with-constants-disabled
		 (set-slot-type copy agget :known-as))))

	    ;; there are certain slots we don't want to copy		   
	    ((or (member slot standard-slots)
		 (member slot *standard-element-slots*)))

	    ;; don't copy parts or behaviors (yet)
	    ((progn
	       (setf values (get-local-value agget slot))
	       (setf value (if (consp values) (car values) values))
	       (and values value (schema-p value)
		    (or (member value components)
			(member value behaviors))))
	     (when (not (eq slot (g-value value :known-as)))
		 (format *error-output*
			 "Warning: slot ~S of ~S: ~S not copied.~%"
			 slot agget value)))

	    ;; don't copy inherited formulas:
	    ((slot-has-an-inherited-formula slot value proto))

	    ;; test to see if this is an :inherit formula:
	    ((is-an-inherit-formula slot value normal-proto)
	     (with-constants-disabled
	       (s-value copy slot (get-inherited-value normal-proto slot))))

	    ;; special copy for formulas
	    ((formula-p value)
	     (with-constants-disabled
	       (s-value copy slot (copy-formula value)))
	     (set-slot-type copy agget slot))

	    ;; ordinary copy of anything that's left:
	    (t
	     ;; rather than doing a full copy-tree of the value,
	     ;;  just copy the top-most list and, if the first
	     ;;  element is a list, copy that one too
	     (when (consp values)
	       (setf values (copy-list values)))
	     (when (consp value)	; recall that value is (car values)
	       (setf (car values) (copy-list value)))
	     (with-constants-disabled
	       ;; Maintain constant bit information, if present.
	       (if (slot-constant-p agget slot)
		   ;; Keep constant
		   (kr::set-slot-accessor copy slot values
					  kr::*constant-mask* NIL)
		   (s-value copy slot values))
	       (set-slot-type copy agget slot)))))

    (when new-parent
      (with-constants-disabled
	(add-local-component new-parent copy)))

    (kr::process-constant-slots copy (list proto)
				(g-local-value proto :constant) NIL)

    (initialize-copy agget copy)
       
    ;; for an itemized aggrelist
    (setf item-prototype-object (g-local-value agget :item-prototype-object))
    (when (and item-prototype-object
	       (g-local-value agget :item-prototype))
      (s-value copy :item-prototype-object (copy-gadget 
					    item-prototype-object nil)))

    ;; If there are components, add them as they are copied
    (dolist (comp components)
      (copy-gadget comp NIL copy))
    
    (when behaviors
      (s-value copy :behaviors
	       (mapcar #'(lambda (inter)
			   (let ((inter-copy (copy-gadget inter nil)))
			     (s-value inter-copy :operates-on copy)
			     (s-value copy (g-value inter-copy :known-as)
				      inter-copy)
			     inter-copy))
		       behaviors)))))



;;; DZG - test code for COPY-GADGET.

#||
(progn
  (create-schema 'a (:left 10) (:top (o-formula (+ 5 (gvl :left))))
		 :declare ((:constant :left)
			   (:type (:integer :left :top))))
  (create-instance 'b a)
  (format t "(g-value b :left) is         ~3S (expected 10)~%"
	  (g-value b :left))
  (format t "(g-value b :top) is          ~3S (expected 15)~%"
	  (g-value b :top))

  (setf c (copy-gadget b NIL))

  (format t "(slot-constant-p c :left) is ~3S (expected T)~%"
	  (slot-constant-p c :left))
  (format t "(g-value c :top) is          ~3S (expected 15)~%"
	  (g-value c :top))
  (format t "(g-value c :left) is         ~3S (expected 15)~%"
	  (g-value c :left))
  (format t "(slot-constant-p c :top) is  ~3S (expected T)~%"
	  (slot-constant-p c :top))
  (format t "(g-value c :is-a) is         ~3S (expected (A))~%"
	  (g-value c :is-a))
  (format t "(g-type c :left) is          ~3S (expected :INTEGER)~%"
	  (g-type c :left))
  (format t "(g-type c :top) is          ~3S (expected NIL)~%"
	  (g-type c :top))
)
||#
