;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  This file contains functions for constructing a path through the
;;;  aggregate hierarchy between two objects.
;;;

(in-package "GILT")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Make-Path)))

;;; indicates whether an object has been marked
(defmacro marked-p (obj)
  `(g-local-value ,obj :mark))

;;; marks or unmarks an object
(defmacro set-marked-bit (obj value)
  (if value
      `(s-value ,obj :mark ,value)
      `(destroy-slot ,obj :mark)))

(defun generate-link-name (obj)
  (let ((counter 0)
	(link :link-0))
    (loop
     (when (not (has-slot-p obj link))
       (return))
     (incf counter)
     (setf link (read-from-string (format nil ":link-~A" counter))))
    link))

;;; =================================================================
;;; constructs a path that gvl can use to get from the source object
;;; to the destination object. this is done in three stages. first
;;; all objects on the path from the source object to the top-level 
;;; aggregate are marked. second all objects on the path from the 
;;; destination object to the common ancestor of the source and 
;;; destination objects (this will be the first marked object) are 
;;; visited and their links are pushed onto the path. third, all objects
;;; on the path from the source object to the common ancestor are visited
;;; and :parent links are pushed onto the path. this third pass continues
;;; up to the top-level aggregate and unmarks all objects on the way up
;;; =================================================================

(defun make-path (src dest)
  (let (parent path parent-path)
    ;; check if the src and dest are the same
    (when (eq src dest)
      (return-from make-path nil))
    ;; optimize for the case where the source and destination objects
    ;; are siblings (i.e., have the same parent)
    (if (and (eq (g-value src :parent) (g-value dest :parent))
	     (g-value dest :known-as))
	(setf path (list :parent (g-value dest :known-as)))
	(progn
	  ;; first pass--mark all objects between source object and top
	  ;; level aggregate
	  (setf parent src)
	  (loop 
	    (set-marked-bit parent t)
	    (setf parent (or (g-value parent :parent) 
			     (g-value parent :operates-on)))
	    (when (null parent)
	      (return)))

	  ;; second pass--push the names of all objects between the destination
	  ;; object and the source and destination objects' common ancestor
	  ;; onto the path
	  (setf parent dest)
	  (loop
	    ;; common ancestor is first marked object
	    (when (marked-p parent)
	      ;; common ancestor is made unmarked
	      (set-marked-bit parent nil)
	      (return))
	    ;; if parent is an aggrelist, or this component is not named with
	    ;; a :known-as slot, the parent will not have a pointer
	    ;; to this object. Create a link that will point
	    ;; to this object and push the generated link name onto the path
	    (if (or (is-a-p (g-value parent :parent) opal:aggrelist)
		    ; amickish - add link if component is unnamed
		    (null (g-value parent :known-as)))
		(let* ((true-parent (g-value parent :parent))
		       link
		       (index (position parent 
					(g-value true-parent :components))))
		  ;; try to find a link in parent's parent that points to
		  ;; it. if none do, generate a new link
		  (setf link (dolist (l (g-value true-parent :links))
				     (when (eq (g-value true-parent l) parent)
					   (return l))))
		  (when (null link)
			(setf link (generate-link-name true-parent)))
		  (push link path)
		  (pushnew link (g-value true-parent :links))
		  (s-value true-parent link 
			   (eval `(o-formula (nth ,index (gvl :components))))))
	        (push (g-value parent :known-as) path))
	    (setf parent (g-value parent :parent)))

	  ;; third pass--push :parent links onto the path for all objects
	  ;; between the source object and the common ancestor. then
	  ;; continue to the top-level aggregate and unmark all objects
	  ;; on the way
	  (setf parent src)
	  (loop
	    ;; common ancestor is first unmarked object
	    (when (not (marked-p parent)) 
	      (loop
		(setf parent (or (g-value parent :parent)
				 (g-value parent :operates-on)))
		(when (null parent)
		  (return))
		(set-marked-bit parent nil))
	      (return))
	    (if (g-value parent :parent)
		(push :parent parent-path)
	        (push :operates-on parent-path))
	    (set-marked-bit parent nil)
	    (setf parent (or (g-value parent :parent)
			     (g-value parent :operates-on))))
	  (append (reverse parent-path) path)))))
