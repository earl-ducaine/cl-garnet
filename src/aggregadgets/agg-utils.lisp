;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -* ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Aggregadgets and Aggrelist utilities
;;;
;;; $Id::                                                             $


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(no-func)))

;;; No-function: used as default value for the matching function.
(defun no-func (x) x)


;;;--------------------------------------------------------------------------
;;;
;;;    Get-Wheres  -  This function is used to analyze the &rest part of the
;;;                   parameters to add-local-item methods.  It returns three
;;;                   (multiple) values.
;;;
;;;--------------------------------------------------------------------------

(defun Get-Wheres (args)
  (let (where locator key)
    (cond ((eq (first args) :where)
	   (setq where (second args))
	   (case where
	     ((:front :back :head :tail)
	      (setq locator NIL)
	      (setq key (if (eq (third args) :key)
			    (fourth args)
			    #'opal:no-func)))
	     (t (setq locator (third args))
		(setq key (if (eq (fourth args) :key)
			      (fifth args)
			      #'opal:no-func)))))
	  ((first args)
	   (setq where (first args))
	   (case where
	     ((:font :back :head :tail)
	      (setq locator NIL)
	      (setq key (if (eq (second args) :key)
			    (third args)
			    #'opal:no-func)))
	     (t (setq locator (second args))
		(setq key (if (eq (third args) :key)
			      (fourth args)
			      #'opal:no-func)))))
	  (t (setq where :front)
	     (setq locator NIL)
	     (setq key #'no-func)))
    (values where locator key)))


;;;--------------------------------------------------------------------------
;;;
;;;    Insert-Item  -  This function returns the items list with the new item
;;;                    inserted in the proper place.  Called by Add-Local-Item
;;;                    methods.
;;;
;;;--------------------------------------------------------------------------

(defun Insert-Item (item items where locator key)
  (cond ((null item)  ;; No specific item supplied -> just add an instance
	              ;; of :prototype-item in the components
	 (if (numberp items)
	     (1+ items)
	     (add-last nil items)))
	((or (eq where :front) (eq where :tail))
	 (add-last item items))
	((or (eq where :back) (eq where :head))
	 (push item items))
	((or (eq where :behind) (eq where :before))
	 (add-before item locator items key))
	((eq where :at)
	 (add-at item locator items))
	((or (eq where :in-front) (eq where :after))
	 (add-after item locator items key))
	(t (warn "***Illegal :where ('~S') ~%" where)
	   (warn "***  Defaulting, setting :where to :front~%") 
	   (add-last item items))))

;;; Adds elt at the end of list.
(defun add-last (elt list)
  (nreverse (cons elt (reverse list))))

;;; Adds elt at the index position of list.
(defun add-at (elt index list)
  (let ((new-list NIL) (cptr 0))
    (dolist (current-elt list)
      (when (eq cptr index)
	(push elt new-list))
      (push current-elt new-list)
      (incf cptr))
    (when (>= index cptr)
      (push elt new-list))
    (nreverse new-list)))

;;; Adds elt before here in list, using key to do the matching.
(defun add-before (elt here list key)
  (let ((new-list NIL) (found NIL))
    (dolist (current-elt list)
      (if (and (null found)
	       (or (equal here (funcall key current-elt))
		   (and (listp here) (equal (funcall key here)
					    (funcall key current-elt)))))
	  (progn
	    (push elt new-list)
	    (push current-elt new-list)
	    (setf found T))
	  (push current-elt new-list)))
    (if (null found)
	(setf new-list (cons elt (nreverse new-list)))
	(setf new-list (nreverse new-list)))
    new-list))

;;; Adds elt after here in list, using key to do the matching.
(defun add-after (elt here list key)
  (let ((new-list NIL) (found NIL))
    (dolist (current-elt list)
      (if (and (null found)
	       (or (equal here (funcall key current-elt))
		   (and (listp here) (equal (funcall key here)
					    (funcall key current-elt)))))
	  (progn
	    (push current-elt new-list)
	    (push elt new-list)
	    (setf found T))
	  (push current-elt new-list)))
    (when (null found)
      (push elt new-list))
    (nreverse new-list)))


;;;--------------------------------------------------------------------------
;;;
;;;    Delete-Elt  -  Removes elt from list, using key to do the matching.
;;;
;;;--------------------------------------------------------------------------

(defun delete-elt (elt list key)
  (let ((new-list NIL) (found NIL))
    (dolist (current-elt list)
      (if (and (null found)
	       (or (equal elt (funcall key current-elt))
		   (and (listp elt) (equal (funcall key elt)
					   (funcall key current-elt)))))
	  (setf found T)
	  (push current-elt new-list)))
    (nreverse new-list)))



