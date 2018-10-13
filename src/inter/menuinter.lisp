;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.


;;; This file contains the mouse interactors to handle menus.  It
;;; should be loaded after Interactor and after MoveGrowInter
;;;
;;; Designed and implemented by Brad A. Myers


(in-package "INTERACTORS")


;;; Utility procedures
;;; parts of :slots-to-set list

(defun check-slots-to-set (inter)
  (let ((slots (g-value inter :slots-to-set)))
    (unless (and (listp slots)
		 (= 3 (length slots))
		 (or (null (first slots)) (keywordp (first slots)))
		 (or (null (second slots)) (keywordp (second slots)))
		 (or (null (third slots)) (keywordp (third slots))))
      (error "the :slots-to-set of ~s must be a list of three nils or keywords"
	     inter))))

(defun interim-sel-slot (inter)
  (first (g-value inter :slots-to-set)))

(defun obj-sel-slot (inter)
  (second (g-value inter :slots-to-set)))

;; there always must be an aggregate selected slot because used for
;; final-feedback
(defun agg-sel-slot (inter)
  (or (third (g-value inter :slots-to-set))
      :*inter-agg-selected*))

;;; final feedback objects
(defun clear-finals (an-interactor feedback-objs-in-use)
  (dolist (f feedback-objs-in-use)
    (dbprint-feed :obj-over f nil an-interactor)
    (s-value f :obj-over nil)))

;; this clears the final feedback objects and resets the list in the
;; interactor.
(defun clear-finals-and-set (an-interactor feedback-objs-in-use)
  (clear-finals an-interactor feedback-objs-in-use)
  (if-debug an-interactor (format t "clearing interactor final feedback slots~%"))
  (s-value an-interactor :final-feed-avail
	   (append (g-value an-interactor :final-feed-avail)
		   feedback-objs-in-use))
  (s-value an-interactor :final-feed-inuse nil))

;;; destroys any objects created to be extra final feedback objects.  this
;;; is called when the interactor is destroyed.
(defun destroy-extra-final-feedback-objs (an-interactor erase)
  (if-debug an-interactor
	    (format t "destroying extra final feedback objects~%"))
  (let ((final-feedback-protos
	 (get-local-value an-interactor :final-feedback-protos)))
    (when final-feedback-protos
      (dolist (obj (get-local-value an-interactor :final-feed-avail))
	(when (schema-p obj)
	  (unless (member obj final-feedback-protos)
	    (with-constants-disabled
		(opal:destroy obj erase)))))
      (s-value an-interactor :final-feed-avail nil)
      (dolist (obj (get-local-value an-interactor :final-feed-inuse))
	(when (schema-p obj)
	  (unless (member obj final-feedback-protos)
	    (with-constants-disabled
		(opal:destroy obj erase)))))
      (s-value an-interactor :final-feed-inuse nil))))

;;; Sets the final feedback object to be only new-sel-obj, or if
;;; new-sel-obj is nil, then sets there to be no final-feedback-objs.
;;; There always must be at least one final-feedback-obj (the
;;; prototype itself), so this procedure does not need to worry about
;;; allocating any.
(defun one-final-feedback-obj (an-interactor new-sel-obj)
  (let ((final-feedback-proto (g-value an-interactor :final-feedback-obj)))
    (when final-feedback-proto ; otherwise, just exit
      (let ((feedback-objs-in-use
	     (get-local-value an-interactor :final-feed-inuse)))
	(clear-finals-and-set an-interactor feedback-objs-in-use)
	(when new-sel-obj
	  ;; set feedback obj to it
	  (let* ((final-feedback (find-final-feedback-obj an-interactor
					     final-feedback-proto)))
	    (dbprint-feed :obj-over final-feedback new-sel-obj an-interactor)
	    (s-value final-feedback :obj-over new-sel-obj)))))))

;;; Adds (if add-p is t) or removes (if add-p is nil) any final-feedback
;;; objs refering to newval, but leaves rest of the final feedback
;;; objects alone
(defun list-final-feedback-obj (an-interactor newval add-p)
  (let ((final-feedback-proto (g-value an-interactor :final-feedback-obj)))
    (when final-feedback-proto ; otherwise, just exit
      (let ((feedback-objs-avail
	     (get-local-value an-interactor :final-feed-avail))
	    (feedback-objs-in-use
	     (get-local-value an-interactor :final-feed-inuse))
	    feed-for-newval)
	(dolist (f feedback-objs-in-use)
	  (when (eq newval (g-value f :obj-over))
	    (setq feed-for-newval f)
	    (return)))
	(if add-p
	    ; add new feedback obj unless one is there
	    (unless feed-for-newval
	      ; get a feedback obj to use
	      (setq feed-for-newval
		    (find-final-feedback-obj an-interactor
					      final-feedback-proto))
	      (dbprint-feed :obj-over feed-for-newval newval an-interactor)
	      (s-value feed-for-newval :obj-over newval))
	    ; else remove the feedback obj
	    (when feed-for-newval
	      (dbprint-feed :obj-over feed-for-newval nil an-interactor)
	      (s-value feed-for-newval :obj-over nil)
	      (s-value an-interactor :final-feed-avail
		       (cons feed-for-newval feedback-objs-avail))
	      (s-value an-interactor :final-feed-inuse
		       (delete feed-for-newval feedback-objs-in-use))))))))

;;; Create an instance of final-feedback-proto unless it is a constant
(defun get-new-feedback-obj (final-feedback-proto)
  (let ((new-obj (create-instance nil final-feedback-proto)))
    (with-constants-disabled
      (opal:add-component (g-value final-feedback-proto :parent)
			  new-obj))
    new-obj))

;;; Find an instance of final-feedback-proto in the available list of
;;; final feedback objects. This allows the prototype to change,
;;; e.g. because there is a formula in the :final-feedback-obj slots
;;; which might depend on the object that the mouse is over. This
;;; routine maintains the slots :final-feed-avail, :final-feed-inuse
;;; and :final-feedback-proto, so don't set the slots after calling
;;; this function.
(defun find-final-feedback-obj (inter final-feedback-proto)
  (let ((available-objs (get-local-value inter :final-feed-avail))
	final-feedback-obj final-feedback-protos)
    ;; search if one of the current type is availble
    (setf final-feedback-obj (car (member final-feedback-proto available-objs
					 :test #'(lambda (item list-obj)
						   (is-a-p list-obj item)))))
    (cond (final-feedback-obj)
	  ;; If there is no final feedback object of the desired type
	  ;; available, check to see if the the prototype final
	  ;; feedback has been added to the :final-feedback-protos
	  ;; list. If it has not, add it to this list and make the
	  ;; final feedback object be the prototype.
	  ((not (member final-feedback-proto
		   (setf final-feedback-protos
			 (get-local-value inter :final-feedback-protos))))
	   (setf final-feedback-obj final-feedback-proto)
	   (s-value inter :final-feedback-protos
		    (push final-feedback-proto final-feedback-protos)))
	  ;; Create an instance of the final feedback prototype
	  (t (setf final-feedback-obj
		   (get-new-feedback-obj final-feedback-proto))
	     (if-debug inter
		       (format t "----allocating final feedback obj:~s~%"
			       final-feedback-obj))))
    ;; now have a final-feedback-obj
    (s-value inter :final-feed-avail  ;; no-op if not there
	     (delete final-feedback-obj available-objs))
    (s-value inter :final-feed-inuse
	     (cons final-feedback-obj
		   (get-local-value inter :final-feed-inuse)))
    final-feedback-obj))

;;; Initialize the final-feedback internal slots if necessary --
;;; should not be necessary in new version. Keep around just in case
;;; code is needed.
(defun check-start-final-feedback-obj (an-interactor)
  (declare (ignore an-interactor)))

;; (when (and (g-value an-interactor :final-feedback-obj)
;; 	     (null (get-local-value an-interactor :final-feed-avail))
;; 	     (null (get-local-value an-interactor :final-feed-inuse)))
;;   (s-value an-interactor :final-feed-avail
;; 	     (list (g-value an-interactor :final-feedback-obj)))))


;;; Exported "useful" functions
(defun return-final-selection-objs (an-interactor)
  "returns a list of all the final-feedback objects currently in use
   by the interactor.  This can be used to have another interactor
   operate on the final feedback objects (e.g., moving from the
   selection handles)."
  (when (g-value an-interactor :final-feedback-obj)
    (copy-list (get-local-value an-interactor :final-feed-inuse))))

(defun deselectobj (an-interactor obj)
  "Cause obj to no longer be selected. Turns off the final-feedback
   objects and clears the various selected slots appropriately. If obj
   is not selected, this does nothing."
  (let ((how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(obj-sel-slot (obj-sel-slot an-interactor)))
    (check-start-final-feedback-obj an-interactor)
    (when (null main-agg) ; hasn't been run yet
      (setq main-agg
	    (s-value an-interactor :main-aggregate
		   (get-gob-of-where (g-value an-interactor :start-where)))))
    (setq how-set
	  (case how-set
	    ((:list-add :list-remove :list-toggle) :list-remove)
	    ((:set :clear :toggle) :clear)))
    ; First do object itself
    (when obj-sel-slot
      (dbprint-sel obj-sel-slot obj nil an-interactor)
      (s-value obj obj-sel-slot nil))
    ; Now do aggregate
    (if (eq main-agg obj)
	;; If no aggregate, then just clear any final-feedbacks
	(clear-finals-and-set an-interactor
		      (get-local-value an-interactor :final-feed-inuse))
	;; Otherwise, do the aggregate and any final-feedback objects
	(calc-set-agg-slot an-interactor main-agg obj how-set))
    obj))

(defun selectobj (an-interactor obj)
  "cause obj to be selected.  turns on the final-feedback objects and
   sets the various selected slots appropriately.  does not check
   whether obj is part of the domain of an-interactor (in
   start-where)."
  (let ((how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(agg-selected-slot (agg-sel-slot an-interactor)))
    (check-start-final-feedback-obj an-interactor)
    ;; Hasn't been run yet
    (when (null main-agg)
      (setq main-agg
	    (s-value an-interactor :main-aggregate
		   (get-gob-of-where (g-value an-interactor :start-where)))))
    (setq how-set
	  (case how-set
	    ((:list-add :list-remove :list-toggle) :list-add)
	    ((:set :clear :toggle) :set)))
    ;; First do object itself
    (calc-set-obj-slot an-interactor obj how-set
			 (if (eq obj main-agg)
			     nil
			     (g-value main-agg agg-selected-slot)))
    ; Now do aggregate
    (if (eq main-agg obj)
	;; if no aggregate, then just set the final-feedback, if any
	(one-final-feedback-obj an-interactor obj)
	;; otherwise, do the aggregate and any final-feedback objects
	(calc-set-agg-slot an-interactor main-agg obj how-set))
    obj))

;;; Calculating how to set the selected slots

(defun how-set-error (how-set)
  (error
  "** bad how-set: ~s.  options are :set :clear :toggle
   :list-add :list-remove :list-toggle <num> (<num> <num>)" how-set))

;;; Sets the selected slot of the object according to how-set.
;;; other-obj contains the other objects that may need
;;; to be cleared because this one was set.
;;; Does handle when obj or other-obj are not schemas.
(defun calc-set-obj-slot (an-interactor obj how-set other-obj)
  (if-debug an-interactor (format t "how-set=~s~%" how-set))
  ;; first clear other object, if necessary and if not same as the main obj
  (let ((obj-sel-slot (obj-sel-slot an-interactor)))
    (when obj-sel-slot
      (if (and other-obj (not (eq other-obj obj)))
	(case how-set
	  ((:list-add :list-remove :list-toggle)) ; do nothing for these
	  ((:set :clear :toggle) (if (listp other-obj)
				     ;; then assume that used to be a list and
				     ;; now isn't, so undo each element
				     (dolist (o other-obj)
				       (when (and (schema-p o)
						  (not (eq o obj)))
					 (dbprint-sel obj-sel-slot o nil
						      an-interactor)
					 (s-value o obj-sel-slot nil)))
				     ;; otherwise, only one object to de-select
				     (when (schema-p other-obj)
				       (dbprint-sel obj-sel-slot other-obj nil
						    an-interactor)
				       (s-value other-obj obj-sel-slot nil))))
	  (otherwise)))			; is a number so do nothing
  ;; now set the selected slot of the new object
    (let (val)
      (when (schema-p obj);; otherwise, can't set its selected slot!
	(case how-set
	  ((:set :list-add) (dbprint-sel obj-sel-slot obj t an-interactor)
	   (s-value obj obj-sel-slot t))
	  ((:clear :list-remove) (dbprint-sel obj-sel-slot obj nil an-interactor)
	   (s-value obj obj-sel-slot nil))
	  ((:toggle :list-toggle)
	   (setq val (if (g-value obj obj-sel-slot) nil t))
	   (dbprint-sel obj-sel-slot obj val an-interactor)
	   (s-value obj obj-sel-slot val))
	  (otherwise (cond ((numberp how-set)
			    (incf (g-value obj obj-sel-slot) how-set)
			    (dbprint-sel obj-sel-slot obj
					 (g-value obj obj-sel-slot)
					 an-interactor))
			   ((and (listp how-set)(numberp (first how-set))
				 (numberp (second how-set))) ; mod
			    (setq val (mod (+ (g-value obj obj-sel-slot)
					      (first how-set))
					   (second how-set)))
			    (dbprint-sel obj-sel-slot obj val an-interactor)
			    (s-value obj obj-sel-slot val))
			   (t (how-set-error how-set))))))))))

;; Used when the new values is :none, this clears out all the
;; selections from the aggregate and the final-feedback-objects
(defun clear-all-selected (an-interactor main-agg)
  (if-debug an-interactor (format t "clearing all selections from ~s~%"
				  main-agg))
  (when (schema-p main-agg)
    (let* ((agg-sel-slot (agg-sel-slot an-interactor))
	   (obj-sel-slot (obj-sel-slot an-interactor))
	   (other-obj (g-value main-agg agg-sel-slot)))
      ;; first, clear the selected slots of any other objects
      (when obj-sel-slot
	(if (listp other-obj)
	  ; undo each element
	  (dolist (o other-obj)
	    (when (and (schema-p o) (kr::schema-name o))
	      (dbprint-sel obj-sel-slot o nil an-interactor)
	      (s-value o obj-sel-slot nil)))
	  ; otherwise, only one object to de-select
	  (when (and (schema-p other-obj) (kr::schema-name other-obj))
	    (dbprint-sel obj-sel-slot other-obj nil an-interactor)
	    (s-value other-obj obj-sel-slot nil))))
      ;; then clear out the aggregate's slot
      (s-value main-agg agg-sel-slot nil)))
  (when (g-value an-interactor :final-feedback-obj) ; then we are doing final
						    ; feedback objects
    (clear-finals-and-set an-interactor
			  (get-local-value an-interactor :final-feed-inuse))))

;;; Sets the selected slot of the aggregate that the selected object
;;; is in according to how-set.  newval is the new object selected.
(defun calc-set-agg-slot (an-interactor agg newval how-set)
  (let* ((agg-sel-slot (agg-sel-slot an-interactor))
	 (old-sel (g-value agg agg-sel-slot))
	 val)
    (when (schema-p agg)
      (case how-set
	(:set (dbprint-sel agg-sel-slot agg newval an-interactor)
	      (s-value agg agg-sel-slot newval)
	      (one-final-feedback-obj an-interactor newval))
	(:clear (dbprint-sel agg-sel-slot agg nil an-interactor)
		(s-value agg agg-sel-slot nil)
		(one-final-feedback-obj an-interactor nil))
	(:toggle (setq val
		       (if (listp old-sel)
			   ;; Then converting from a list to a single
			   ;; value
			   (if (member newval old-sel)
			       ;; if used to be selected, then clear
			       nil
			       ;; else select just this one
			       newval)
			   ;; Otherwise, just check the old single
			   ;; value
			   (if (eq old-sel newval)
			       ;; if used to be selected, then clear
			       nil
			       ;; else select this one
			       newval)))
		 (dbprint-sel agg-sel-slot agg val an-interactor)
		 (s-value agg agg-sel-slot val)
		 (one-final-feedback-obj an-interactor val))
	(:list-add
	 (cond ((listp old-sel)
		(pushnew newval (g-value agg agg-sel-slot))
		(dbprint-sel agg-sel-slot
			     agg (g-value agg agg-sel-slot) an-interactor))
	       ((schema-p old-sel)
		;; Make it into a list (in case how-set changed
		;; from single-selectable to be multiple selectable).
		;; if new obj same as old, only include once, however.
		(setq val (if (eq newval old-sel)
			      (list newval)
			      (list newval old-sel)))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val))
	       ;; otherwise, throw away old value
	       (t (setq val (list newval))
		  (dbprint-sel agg-sel-slot agg val an-interactor)
		  (s-value agg agg-sel-slot val)))
	 (list-final-feedback-obj an-interactor newval t)) ; add newval
	(:list-remove
	 (cond ((listp old-sel)
		(setq val (delete newval (g-value agg agg-sel-slot)))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)
		(mark-as-changed agg agg-sel-slot)) ;; s-value may not cause
					;slot to be marked since its value is
					;a list which will be
					;destructively modified by delete
	       ;; else convert to a list
	       ((eq old-sel newval) (dbprint-sel agg-sel-slot agg nil
						 an-interactor)
		(s-value agg agg-sel-slot nil)) ;remove old
	       ((schema-p old-sel) (setq val (list old-sel))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)) ;keep old
	       (t (dbprint-sel agg-sel-slot agg nil an-interactor)
		  (s-value agg agg-sel-slot nil))) ; bad old value, remove it
	 (list-final-feedback-obj an-interactor newval nil)) ; remove newval
	(:list-toggle
	 (cond ((listp old-sel)
		(if (member newval old-sel)
		    (progn
		      (setq val (delete newval old-sel))
		      (dbprint-sel agg-sel-slot agg val an-interactor)
		      (s-value agg agg-sel-slot val)
		      ;; s-value may not cause slot to be marked since
		      ;; its value is a list which will be
		      ;; destructively modified by delete
		      (mark-as-changed agg agg-sel-slot)
		      (list-final-feedback-obj an-interactor newval nil)) ;remove
		    (progn
		      (push newval (g-value agg agg-sel-slot))
		      (dbprint-sel agg-sel-slot agg (g-value agg agg-sel-slot)
				   an-interactor)
		      (list-final-feedback-obj an-interactor newval t)))) ;add
					;; Otherwise, if was the old value, now none
	       ((eq old-sel newval) (dbprint-sel agg-sel-slot agg nil
						 an-interactor)
		(s-value agg agg-sel-slot nil)
		(list-final-feedback-obj an-interactor newval nil)) ;remove
					;; If was a different object, use both
	       ((schema-p old-sel)
		(setq val (list newval old-sel))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)
		;; add
		(list-final-feedback-obj an-interactor newval t))
	       ;; bad old val, remove it
	       (t (setq val (list newval))
		  (dbprint-sel agg-sel-slot agg val an-interactor)
		  (s-value agg agg-sel-slot val)
		  (list-final-feedback-obj an-interactor newval t)))) ;add
	;; is a number, already incremented object's selected slot,
	(otherwise
	 ;; here just note newval
	 (dbprint-sel agg-sel-slot agg newval an-interactor)
	 (s-value agg agg-sel-slot newval)
	 (list-final-feedback-obj an-interactor newval t)))))) ;add


;;; Menu interactors
;;; default procedures to go into the slots

(declaim (special menu-interactor))

(defun menu-interactor-initialize (new-menu-schema)
  (if-debug new-menu-schema (format t "menu initialize ~s~%" new-menu-schema))
  (check-interactor-type new-menu-schema inter:menu-interactor)
  (check-required-slots new-menu-schema)
  (check-slots-to-set new-menu-schema)
  (set-up-defaults new-menu-schema)
  (s-value new-menu-schema :remembered-last-object nil) ; this slot must be local
  ) ;end initialize procedure

(defun menu-int-running-action (an-interactor prev-obj-over new-obj-over)
  (if-debug an-interactor (format t "menu int-running, old = ~s, new= ~s~%"
				  prev-obj-over new-obj-over))
  (unless (eq prev-obj-over new-obj-over)
    (let ((interim-sel-slot (interim-sel-slot an-interactor))
	  (feedbackobj (g-value an-interactor :feedback-obj)))
      (when feedbackobj
	(dbprint-feed :obj-over feedbackobj new-obj-over an-interactor)
	(s-value feedbackobj :obj-over new-obj-over))
      (when (and interim-sel-slot prev-obj-over
		 (schema-p prev-obj-over))
	(dbprint interim-sel-slot prev-obj-over nil an-interactor)
	(s-value prev-obj-over interim-sel-slot nil))
      (when (and interim-sel-slot
		 new-obj-over (schema-p new-obj-over))
	(dbprint interim-sel-slot new-obj-over t an-interactor)
	(s-value new-obj-over interim-sel-slot t)))))

(defun menu-int-start-action (an-interactor obj-under-mouse)
  (if-debug an-interactor (format t "menu int-start over ~s~%" obj-under-mouse))
  (kr-send an-interactor :running-action
	   an-interactor nil obj-under-mouse))  ;turn on feedback

(defun menu-int-outside-action (an-interactor outside-control prev-obj-over)
  (if-debug an-interactor (format t "menu int-outside, old = ~s~%" prev-obj-over))
  (unless (eq :last outside-control)
    (kr-send an-interactor :running-action
	   an-interactor prev-obj-over nil)))

(defun menu-int-back-inside-action (an-interactor outside-control
					       prev-obj-over new-obj-over)
  (if-debug an-interactor (format t "menu int-back-inside, old = ~s, new= ~s~%"
				  prev-obj-over new-obj-over))
  (kr-send an-interactor :running-action an-interactor
	   (if (eq :last outside-control) prev-obj-over nil)
	   new-obj-over))

(defun menu-int-stop-action (an-interactor final-obj-over)
  (if-debug an-interactor (format t "menu int-stop over ~s~%" final-obj-over))
  (let ((feedbackobj (g-value an-interactor :feedback-obj))
	(how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(interim-sel-slot (interim-sel-slot an-interactor))
	(agg-sel-slot (agg-sel-slot an-interactor))
	(obj-sel-slot (obj-sel-slot an-interactor)))
    (when feedbackobj
      (dbprint-feed :obj-over feedbackobj nil an-interactor)
      (s-value feedbackobj :obj-over nil))
    (when final-obj-over
      (when (and interim-sel-slot (schema-p final-obj-over))
	(dbprint interim-sel-slot final-obj-over nil an-interactor)
	(s-value final-obj-over interim-sel-slot nil))
      (when (schema-p main-agg)
	(calc-set-obj-slot an-interactor
			 final-obj-over how-set
			 ; old-object is the one that used to be selected,
			 ; and get it from the aggregate, if any
			 (if (eq final-obj-over main-agg)
			     nil
			     (g-value main-agg agg-sel-slot)))))
    (if (eq :none final-obj-over)
	(clear-all-selected an-interactor main-agg)
	; else handle the new object normally
	(when (and main-agg (schema-p main-agg))
	  (if (eq final-obj-over main-agg) ;; if eq, then selected already set,
				    ; but still need to do final-feedback-obj
	      (one-final-feedback-obj an-interactor
			       (if (and obj-sel-slot
					(g-value final-obj-over obj-sel-slot))
				   final-obj-over nil))
	      ;; else set the selected slot of the main-agg.  this procedure
	      ;; will also handle the final-feedback-obj
	      (calc-set-agg-slot an-interactor main-agg
				 final-obj-over how-set))))
    (kr-send an-interactor :final-function an-interactor final-obj-over)))

(defun menu-int-abort-action (an-interactor final-obj-over)
  (if-debug an-interactor (format t "menu int-abort over ~s~%" final-obj-over))
  (kr-send an-interactor :running-action an-interactor
	   final-obj-over nil))

;;; go procedure utilities

;;; remove from running level, put on start level, change state to
;;; start, call abort procedure.  become-inactive ignored because :active
;;; set before this is called
(defun menu-do-abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format t "menu aborting~%"))
  (gotostartstate an-interactor t)
  (kr-send an-interactor :abort-action an-interactor
	       (get-local-value an-interactor :remembered-last-object)))

;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running)
;;; save object over, call start procedure.
(defun menu-do-start (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format t "menu starting over ~s~%" new-obj-over))

  (s-value an-interactor :remembered-last-object new-obj-over)
  (fix-running-where an-interactor new-obj-over)
  (s-value an-interactor :main-aggregate
           (get-gob-of-where (get-running-where an-interactor)))
  (check-start-final-feedback-obj an-interactor)
  (if (g-value an-interactor :continuous)  ;then will go to running state
    (progn
      (gotorunningstate an-interactor t)
      (kr-send an-interactor :start-action an-interactor new-obj-over))
    ;else call stop-action
    (progn
      (gotostartstate an-interactor nil)
      (kr-send an-interactor :stop-action an-interactor new-obj-over))))


;;; call outside procedure, clear saved obj, change state to outside
(defun menu-do-outside (an-interactor)
  (if-debug an-interactor (format t "menu outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :outside)
	       (g-value an-interactor :remembered-last-object))
  (unless (eq :last (g-value an-interactor :outside))
    (s-value an-interactor :remembered-last-object nil)))

;;;check to see if need to stop or abort based on whether :outside = :last
(defun menu-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format t "menu stop outside~%"))
  (if (eq :last (g-value an-interactor :outside))
      (menu-do-stop an-interactor (g-value an-interactor
					:remembered-last-object) event)
      (menu-do-abort an-interactor nil event)))

;;; call back-inside procedure, change state to running
(defun menu-do-back-inside (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format t "menu back-inside over ~s~%" new-obj-over))
  (s-value an-interactor :current-state :running)
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (kr-send an-interactor :back-inside-action an-interactor
		 (g-value an-interactor :outside) prev-obj-over new-obj-over)
    (s-value an-interactor :remembered-last-object new-obj-over)))

;;;if new object is different from old one, call running-procedure
(defun menu-do-running (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format t "menu running over ~s~%" new-obj-over))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (unless (eq prev-obj-over new-obj-over)
      (kr-send an-interactor :running-action an-interactor prev-obj-over
	       new-obj-over)
      (s-value an-interactor :remembered-last-object new-obj-over))))

;;; If new-obj-over not equal to :remembered-last-object, then call
;;; running-action on :remembered-last-object so its interim-feedback can
;;; be removed.  then, remove from running level, add to start level
;;; change state to start, call stop procedure
(defun menu-do-stop (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format t "menu stop over ~s~%" new-obj-over))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (unless (eq prev-obj-over new-obj-over)
      (kr-send an-interactor :running-action an-interactor prev-obj-over
		   new-obj-over)
      (s-value an-interactor :remembered-last-object new-obj-over)))
  (gotostartstate an-interactor t)
  (kr-send an-interactor :stop-action an-interactor new-obj-over))

;;; This is used if explicitly call stop-interactor.  it uses the last
;;; selected object
(defun menu-explicit-stop (an-interactor)
  (if-debug an-interactor (format t "menu explicit stop~%"))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (if prev-obj-over
	(progn
	  (gotostartstate an-interactor t)
	  (kr-send an-interactor :stop-action an-interactor prev-obj-over))
	(menu-do-abort an-interactor nil nil))))

;;; menu schema
(create-schema 'inter:menu-interactor
	       (:is-a inter:interactor)
	       (:name :first-menu-interactor)
	       (:start-action 'menu-int-start-action)
	       (:running-action 'menu-int-running-action)
	       (:stop-action 'menu-int-stop-action)
	       (:abort-action 'menu-int-abort-action)
	       (:outside-action 'menu-int-outside-action)
	       (:back-inside-action 'menu-int-back-inside-action)
	       (:how-set :set)
	       (:slots-to-set '(:interim-selected :selected :selected))
	       ;; slots: interim, in object, in aggregate
	       (:remembered-last-object nil)
	       (:main-aggregate nil)
	       (:go 'general-go)
	       ;; Proc executed when events happen. These are called
	       ;; by go to do for stop-interactor the real work.  they
	       ;; call the appropriate action procedures
	       (:do-start 'menu-do-start)
	       (:do-running 'menu-do-running)
	       (:do-stop 'menu-do-stop)
	       (:do-explicit-stop 'menu-explicit-stop)
	       (:do-abort 'menu-do-abort)
	       (:do-outside 'menu-do-outside)
	       (:do-back-inside 'menu-do-back-inside)
	       (:do-outside-stop 'menu-do-outside-stop)
	       ;; proc to call when created
	       (:initialize 'menu-interactor-initialize))

;;; need special destroy to remove the extra final feedback objects
;;; that may have been allocated.
(define-method :destroy-me
    menu-interactor (an-interactor &optional (erase t))
    (if-debug
     an-interactor
     (format t "menu special destroy ~s erase=~s~%" an-interactor erase))
    (destroy-extra-final-feedback-objs an-interactor erase)
    (call-prototype-method an-interactor erase))
