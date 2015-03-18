;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;  These functions are used to generate labels for the gadgets.  They are
;;  defined in this aggregadget file because they are used in several different
;;  gadget files.
;;;
;;; $Id::                                                             $	


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Single-Button-Get-Label Panel-Get-Label)))

;;
;; These functions return objects that will be used directly
;;
(defun Single-Button-Get-Label (agg)
  (let ((item (g-value agg :string))
	(text-label-prototype (g-value agg :text-label-prototype))) 
    (cond
      ((schema-p item)
       (let ((new-label (if (g-value item :parent)
			    ;; The item has been used already --
			    ;; Use it as a prototype
			    (create-instance NIL item)
			    ;; Use the item itself
			    item))
	     (leftform (get-value text-label-prototype :left))
	     (topform (get-value text-label-prototype :top)))
	 ;; Automatically set the :left and :top of the label
	 (unless (is-a-p (get-local-value item :left) leftform)
	   (s-value new-label :left (formula leftform)))
	 (unless (is-a-p (get-local-value item :top) topform)
	   (s-value new-label :top (formula topform)))
	 new-label))
      (t (create-instance NIL text-label-prototype)))))

(defun Panel-Get-Label (agg)
  (let ((alist (g-value agg :parent)))
    (if alist  ;; Must check because the item-prototype
               ;; has no parent!
	(let* ((item (nth (g-value agg :rank)
			  (g-value alist :items)))
	       (text-label-prototype (g-value alist :parent
					      :text-label-prototype)))
	  ;; Don't forget that item functions are allowed!
	  (if (consp item) (setq item (first item)))
	  ;; Don't forget that menus have item conversion functions!
	  (if (g-value alist :item-to-string-function)
	      (setf item (kr-send alist :item-to-string-function item)))
	  (cond
	    ((schema-p item)
	     (let ((new-label (if (g-value item :parent)
				  ;; The item has been used already --
				  ;; Use it as a prototype
				  (create-instance NIL item)
				  ;; Use the item itself
				  item))
		   (leftform (get-value text-label-prototype :left))
		   (topform (get-value text-label-prototype :top)))
	       ;; Automatically set the :left and :top of the label
	       (unless (is-a-p (get-local-value item :left) leftform)
		 (s-value new-label :left (formula leftform)))
	       (unless (is-a-p (get-local-value item :top) topform)
		 (s-value new-label :top (formula topform)))
	       new-label))
	    (t (create-instance NIL text-label-prototype))))
	;; Give the item-prototype a bogus part
	(create-instance NIL opal:null-object))))

