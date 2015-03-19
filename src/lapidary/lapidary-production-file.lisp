;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; This file contains definitions of lapidary objects and variables
;;; that production programs must use

(in-package "LAPIDARY")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(lapidary-agg editor-menu load-instances)))

(defvar *CREATED-INSTANCES* NIL
  "list of objects created by loading a lapidary-generated file")

;;; controls visibility slot of feedback objects. In production programs,
;;; feedback objects are always in "test" mode

(defvar editor-menu nil)
(defvar *custom-constraint* nil)

(defun dependency-any-slot-p (schema obj slot)
  (let ((formula (get-value schema slot)))
    (when (formula-p formula)
      (dolist (dependencies (get-values obj :depended-slots))
	(when (member formula (cdr dependencies))
	  (return-from dependency-any-slot-p t))))
    nil))

(defun depends-on-p (p-selection slot s-selection link)

  ;; determine if the formula depends on the secondary selection
  (if (is-a-p (get-value p-selection slot) *custom-constraint*)
      ;; if this is a custom formula, determine whether the
      ;; slot in the primary selection depends on any of the
      ;; slots in the secondary selection
      (dependency-any-slot-p p-selection s-selection slot)
      
      ;; if this is a lapidary formula, determine whether the
      ;; slot in the primary selection depends on the same slot
      ;; in the secondary selection
      (eq (g-value p-selection link) s-selection)))

(defun is-a-line-p (obj) nil)

(create-schema 'editor-menu
  (:build-p nil))


;;; load a file of gadgets into memory and return the list of created objects

(defun load-instances (loader-fct required-objs created-objs)
  (let (file created-instances) 
    (dolist (item required-objs) 
      (when (not (boundp item)) 
	(format t "~% please enter the name of a file that contains ~S: " 
		(symbol-name item))
	(setf file (read-line)) 
	(if (read-from-string file) ; if the user actually entered something 
	    (progn 
	      (load file) 
	      ; the user could demur in defining objects necessary for 
	      ; the definition of item so we must make sure that item 
	      ; was actually created	
	      (when (not (boundp item)) 
		(format t "~% aborting load ~%") 
		(setf *created-instances* nil)
		(return-from load-instances))
	      
	      ; add the created instances to the created-instances list
	      (setf created-instances (append created-instances 
					      *created-instances*)))
	    ; else user entered nothing so we better not create the 
	    ; aggregadgets. 
	    (progn 
	      (format t "~% aborting load ~%")
	      (setf *created-instances* nil) 
	      (return-from load-instances)))))

    ;; now create the objects
    (funcall loader-fct)
    
    ; add the created instances to the created-instances list
    (setf created-instances (append created-instances 
				    (mapcar #'eval created-objs)))

    (setf *created-instances* created-instances)
    created-instances))
