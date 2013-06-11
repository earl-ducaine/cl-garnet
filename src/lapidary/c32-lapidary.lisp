;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; This file contains a few interface functions that are needed for Lapidary.


(in-package "C32")

;; custom function to be called when the user is finished editing a formula
(defvar *custom-function* nil)

;; top level aggregate to be used in get-root. Sometimes the user does not
;; want get-root to search all the way to the window aggregate. For example,
;; when there is both an editor aggregate and a feedback aggregate, the
;; search should stop at the editor aggregate.
(defvar *top-level-agg* nil)

;;;
(defun c32-ok-function ()
  ;; allow no windows to be selected by the obj-find interactor
;  (s-value (g-value c32::ask-object :obj-find) :window nil)
  (setf lapidary-p nil)
  (dolist (win *All-windows*)
    (if (schema-p win)
      (s-value win :visible nil)))
  (dolist (panel (copy-list (g-value *Current-Panel-Set* :aggrel :components)))
	  ;; do not remove the last panel which is the "other name" panel
	  (when (g-value panel :obj)
		(remove-panel panel))))



(defun lapidary-QuitFunc (gadget sel)
  (declare (ignore gadget sel))
  (declare (special lapidary-p))
  (if lapidary-p
    (c32-ok-function)
    (progn
      (do-stop)
      #-cmu (inter:exit-main-event-loop))))



;;; --------------------------------------------------

(defun generate-link-name (obj)
  (let ((counter 0)
	(link :link-0))
    (loop
     (when (not (has-slot-p obj link))
       (return))
     (incf counter)
     (setf link (read-from-string (format nil ":link-~A" counter))))
    link))


(defun Get-Reference-For (to-obj to-slot from-obj from-slot)
  (declare (ignore from-slot))
  (let ((win *current-formula-win*)
	link ref)
    ;; see if the from-obj and to-obj are the same and avoid the messy
    ;; generating of links if they are
    (cond ((eq to-obj from-obj)
	   (setf ref (if (or (null to-slot) (eq to-slot T)) 
		       ;; if a reference to the object itself, return SELF
		       "(gv :SELF)" 
		       ;; else use to-slot of object	     
		       (prin1-to-string `(gvl ,to-slot)))))
	  (t
	   ;; see if a link for this object already exists
	   (dolist (slot (g-value from-obj :links))
	     (when (eq to-obj (g-value from-obj slot))
	       (setf link slot)
	       ;; need to keep track of existing as well as new links,
	       ;; so that all links can be stored in the new formula's
	       ;; meta-information--we cannot add this link to the :links
	       ;; slot, because remove-temporary-links would not know whether
	       ;; this was a pre-existing link or not
	       (pushnew link (g-value win :existing-links))
	       (return)))

	   ;; also check the links generated for this formula.
	   ;; the links are in a list of the form ((link obj) ... (link obj))
	   (unless link
	     (setf link (member to-obj (g-value win :links) :key #'cdr))
	     (if link (setf link (caar link))))

	   ;; if a link couldn't be found, generate a new link. Start 
	   ;; generating link names and see if they're already in use. 
	   ;; Start with :link-0 and work up. Since there are unlikely 
	   ;; to be too many link names, this is not that wasteful.
	   (unless link
	     (setf link (generate-link-name from-obj))
	     ;; temporarily install the link so that the formula
	     ;; can be tested--it will be removed or given the
	     ;; appropriate path later
	     (s-value from-obj link to-obj)
	     (push (cons link to-obj) (g-value win :links)))

	   ;; create the reference that will be returned
	   (if (or (null to-slot) (eq to-slot T)) ; reference to the object itself
	     (setf ref (prin1-to-string `(gvl ,link)))
	     (setf ref (prin1-to-string `(gvl ,link ,to-slot))))))

    ref))




;;;
;;; find the root aggregadget for an object, assuming that the
;;; root is not the top-level aggregadget but an aggregadget in between
;;;
#|
(defun get-root (obj)
  (let ((parent (or (g-value obj :parent)
		    (g-value obj :operates-on))))
    (if (is-a-p obj inter:interactor)
	(if parent
	    (get-root parent)
	    obj)
        (if (eq (g-value obj :parent)
		(g-value obj :window :editor-agg))
	    (return-from get-root obj)
	    (get-root parent)))))
|#

(defun get-root (obj)
  (let ((parent (or (g-value obj :parent)
		    (g-value obj :operates-on)))
	(top-agg (when (not (is-a-p obj inter:interactor))
		       (or *top-level-agg*
			   (g-value obj :window :aggregate)))))
    (if (is-a-p obj inter:interactor)
	(if parent
	    (get-root parent)
	    obj)
        (if (eq (g-value obj :parent)
		top-agg)
	    (return-from get-root obj)
	    (get-root parent)))))

;;; find the common ancestral aggregadget of obj1 and obj2
;;;
(defun common-ancestor-p (obj1 obj2)
  (let ((parent1 (get-root obj1))
	(parent2 (get-root obj2)))
    (eq parent1 parent2)))



;;; install links that will be used in a formula. if the object referenced
;;; by the link and the link belong to a common aggregadget, create a formula 
;;; that traverses the aggregate hierarchy to get the to-obj; otherwise use 
;;; a direct reference
;;;
(defun install-links (win from-obj)
  (let (link to-obj)
    (dolist (link-obj (if (formula-p win) 
			  (g-formula-value win :links-and-objs)
			  (g-value win :links)))
      (setf link (car link-obj))
      (setf to-obj (cdr link-obj))
      (if (common-ancestor-p from-obj to-obj)
	(s-value from-obj link 
		 (eval `(o-formula
			 (gvl ,@(gilt::make-path from-obj to-obj)))))
	(s-value from-obj link to-obj))
      ;; remember that link is a link
      (pushnew link (g-value from-obj :links)))
    ;; reset the window's links slot to nil
    (if (not (formula-p win))
	(progn
	  (s-value win :links nil)
	  (s-value win :existing-links nil))
        (s-formula-value win :links-and-objs nil))))



;;; get rid of the temporarily installed links and reset the links list
;;; to nil
;;;
(defun remove-temporary-links (obj win)
  (dolist (link-obj (g-value win :links))
    (destroy-slot obj (car link-obj)))
  (s-value win :links nil)
  (s-value win :existing-links nil))




(create-instance 'direct-ref-query-gadget garnet-gadgets:query-gadget
  (:modal-p t)
  (:button-names '("YES" "NO")))


(defun check-for-direct-ref (expr)
  (cond ((listp expr)
	 (dolist (element expr)
	   (if (string-equal (check-for-direct-ref element) "YES")
	       (return-from check-for-direct-ref t)))
	 nil)
	(t
	 (if (and (symbolp expr) (boundp expr)
		  (is-a-p (symbol-value expr) opal:view-object))
	     (garnet-gadgets:display-query-and-wait
	      direct-ref-query-gadget
	      (format nil "The formula contains a direct reference to ~S.
It may not be possible to generalize this
formula properly if the direct reference
should be a parameter. If the direct reference
should be a parameter, please edit the
formula and use either 'Insert Ref From Spread...'
or 'Insert Ref from Mouse' to insert the reference.
Do you want to edit the formula?" expr))

	     ;; else the expr is not a view-object, so return nil
	     nil))))

    

(defun lapidary-Do-Form-Cancel (gadget item)
  (declare (Ignore item))
  (let ((win (g-value gadget :window)))
    (setq *Current-Formula-Win* NIL)
    ;; get rid of the links that were temporarily installed so that
    ;; the formula could be safely evaluated
    (remove-temporary-links (g-value win :c32-obj) win)
    (s-value win :visible NIL)
    (push win Formula-Wins-Available)
    ;; if this formula was requested by some property sheet, make c32 
    ;; invisible
    (when (g-value win :disappear-p)
	  ;; restore these slots to default settings
	  (s-value win :disappear-p nil)
	  (c32-ok-function)
	  )))


(defun lapidary-Do-Form-Ok (gadget item)
  (declare (Ignore item))
  (let* ((win (g-value gadget :window))
	 (obj (g-value win :c32-obj))
	 (slot (g-value win :c32-slot))
	 (item (g-value win :c32-item))
	 (valstr (opal:get-string (g-value win :edit-string)))
	 (*current-formula-obj* obj)
	 (*current-formula-slot* slot)
	 result)
    (setq *Current-Formula-Win* NIL)
    (multiple-value-bind (val ok-p)
	(Convert-Str-To-Formula valstr)
      (case ok-p
	((t)
	 ;; place the formula on an interactor queue if a queue is provided
	 (if *custom-function*
	     (progn
	       ;; install meta-information in the formula
	       (s-formula-value val :menu-item :customize)
	       (s-formula-value val :links 
				(append (mapcar 'car (g-value win :links))
					(g-value win :existing-links)))
	       (s-formula-value val :links-and-objs 
				(g-value win :links))
	       ;; get rid of the links that were temporarily installed so that
	       ;; the formula could be safely evaluated
	       (remove-temporary-links obj win)
	       ;; call the custom formula
	       (funcall *custom-function* obj slot val))

	     ;; else, install the formula
	     (progn
	       ;; install meta-information in the formula
	       (s-formula-value val :menu-item :customize)
	       (s-formula-value val :links 
				(append (mapcar 'car (g-value win :links))
					(g-value win :existing-links)))
	       ;; install links
	       (install-links win obj)
	       (s-value obj slot val)))	; formula
	 ;; if formula added or removed, won't necessarily notice
	 (kr:recompute-formula item :formula-p))
	(:val
	 (destroy-constraint obj slot) ; regular value
	 (s-value obj slot val))
	(:empty
	 (destroy-constraint obj slot)) ; empty value, use old value
	((NIL)
	 (return-from lapidary-Do-Form-Ok))) ; error already reported
      (s-value win :visible NIL)
      (push win Formula-Wins-Available)

    ;; if this formula was requested by some property sheet, make c32 
    ;; invisible
    (when (g-value win :disappear-p)
	  ;; restore these slots to default settings
	  (s-value win :disappear-p nil)
	  (c32-ok-function))
)))



