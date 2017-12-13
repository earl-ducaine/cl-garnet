;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $Id$
;;;
;;; This file contains the mouse interactors to handle menus.
;;; It should be loaded after Interactor and after MoveGrowInter
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
       9/8/93 Brad Myers - fixed bug with final-feed-avail (thanks to Almond)
       7/13/93 Brad Myers - allow formula in :final-feedback-obj (BVZ)
       7/30/92 Brad Myers - make explicit-stop more robust (for Lars Smit)
       7/16/92 Brad Myers - fixed small bug where :continuous NIL
                   meant couldn't destroy in final-function: Robert Schnelbach
       4/4/92  Brad Myers - Bound kr::*constants-disabled* in destroy also
       4/3/92  Andrew Mickish - Bound kr::*constants-disabled* in
                                List-Final-Feedback-Obj; removed test for
                                constant :parent in Get-New-Feedback-Obj.
       4/1/92  Andrew Mickish - kr::*debug-switch* ---> #-garnet-debug
       2/27/92 Andrew Mickish - Check for destroyed object in Calc-Set-Obj-Slot
                                by calling schema-p
       2/7/92  Brad Myers - test final-feedback is constant part of aggregadget
                          - more robust destroy
       1/28/92 Brad Myers - make :start-where T work.
                          - added :slots-to-set parameter
       1/21/92 Ed Pervin - In Clear-All-Selected, check that selected
                           is not destroyed object.
       3/26/91 Brad Myers - fixed bug introduced last time for main-agg from
                            running-where rather than start-where in do-start
       3/20/91 Brad Myers - fixed bug in SelectObj
       1/13/91 Brad Myers - made the selection list always be in reverse order
       12/4/90 Brad Myers - fixed bug in SelectObj for when interactor
                            hasn't run
       10/11/90 Brad Myers - added explicit Stop-Interactor code
	9/21/90 Brad Myers - fixed final-feedback so works if :start-where
                             returns :none.
			     Added new procedures:
				Return-Final-Selection-Objs,
                                gv-Final-Selection-Objs, DeSelectObj, SelectObj
        7/23/90 Brad Myers - added new parameter to Destroy-Extra-Final-
                             Feedback-Objs
          7/11/90 Ed Pervin - new :destroy-me method
  	  6/14/90 Brad Myers - added destroy method
 	   6/7/90 Brad Myers - add final-feedback-objs
	  4/27/90 Brad Myers - fixed so can be non-continuous
	 10/25/89 Brad Myers - small bug in setting the :selected slot
         10/5/89 Brad Myers - Add Final-Function
	 10/4/89 Roger Dannenberg - Change debugging output
         9/22/89 Brad Myers - Made more robust when :start-where = T
         8/14/89 Brad Myers - Fixed for multiple priority levels
         6/26/89 Brad Myers - Fixed to have quote for create-schema
         6/8/89  Brad Myers -  Fixed so how-set handled consistently
         5/30/89 Brad Myers -  call-method -> kr-send;
                               allow running-where to be set after initialized
         4/20/89 Brad Myers - schema-call -> call-method
         4/14/89 Brad Myers - fixed self-deactivate
         4/7/89  Brad Myers and Dario Giuse - changed to work with new KR
         1/15/89 Lynn Baumeister - changed x,y to event structure in func calls
	12/22/88 Brad Myers - feedback both in feedbackobj and main aggregate
	11/28/88 Brad Myers - removed from Interactor.lisp
============================================================
|#

(in-package "INTERACTORS")


;;;;===========================================================
;;;  Utility procedures
;;;============================================================
;;;============================================================

;;; parts of :slots-to-set list

(defun check-slots-to-set (inter)
  (let ((slots (g-value inter :slots-to-set)))
    (unless (and (listp slots)
		 (= 3 (length slots))
		 (or (null (first slots)) (keywordp (first slots)))
		 (or (null (second slots)) (keywordp (second slots)))
		 (or (null (third slots)) (keywordp (third slots))))
      (error "The :slots-to-set of ~s must be a list of three NILs or keywords"
	     inter))))

(defun interim-sel-slot (inter)
  (first (g-value inter :slots-to-set)))

(defun obj-sel-slot (inter)
  (second (g-value inter :slots-to-set)))

;;There always must be an aggregate selected slot because used for
;;final-feedback
(defun agg-sel-slot (inter)
  (or (third (g-value inter :slots-to-set))
      :*Inter-agg-selected*))


;;;;===========================================================
;;; Final Feedback objects
;;;============================================================

(defun Clear-Finals (an-interactor feedback-objs-in-use)
  #-garnet-debug (declare (ignore an-interactor))
  (dolist (f feedback-objs-in-use)
    (dbprint-feed :obj-over f NIL an-interactor)
    (s-value f :obj-over NIL)))

;; This clears the final feedback objects and resets the list in the
;; interactor.
(defun Clear-Finals-And-Set (an-interactor feedback-objs-in-use)
  (clear-finals an-interactor feedback-objs-in-use)
  (if-debug an-interactor (format T "Clearing interactor final feedback slots~%"))
  (s-value an-interactor :final-feed-avail
	   (append (g-value an-interactor :final-feed-avail)
		   feedback-objs-in-use))
  (s-value an-interactor :final-feed-inuse NIL))


;;; destroys any objects created to be extra final feedback objects.  This
;;; is called when the interactor is destroyed.
(defun Destroy-Extra-Final-Feedback-Objs (an-interactor erase)
  (if-debug an-interactor
	    (format T "Destroying extra final feedback objects~%"))
  (let ((final-feedback-protos
	 (get-local-value an-interactor :final-feedback-protos)))
    (when final-feedback-protos
      (dolist (obj (get-local-value an-interactor :final-feed-avail))
	(when (schema-p obj)
	  (unless (member obj final-feedback-protos)
	    (with-constants-disabled
		(opal:destroy obj erase)))))
      (s-value an-interactor :final-feed-avail NIL)
      (dolist (obj (get-local-value an-interactor :final-feed-inuse))
	(when (schema-p obj)
	  (unless (member obj final-feedback-protos)
	    (with-constants-disabled
		(opal:destroy obj erase)))))
      (s-value an-interactor :final-feed-inuse NIL))))

;;; sets the final feedback object to be only new-sel-obj, or if
;;; new-sel-obj is NIL, then sets there to be no final-feedback-objs.
;;; There always must be at least one final-feedback-obj (the prototype
;;; itself), so this procedure does not need to worry about allocating any.
(defun One-Final-Feedback-Obj (an-interactor new-sel-obj)
  (let ((final-feedback-proto (g-value an-interactor :final-feedback-obj)))
    (when final-feedback-proto ; otherwise, just exit
      (let ((feedback-objs-in-use
	     (get-local-value an-interactor :final-feed-inuse)))
	(Clear-Finals-And-Set an-interactor feedback-objs-in-use)
	(when new-sel-obj
	  ;; set feedback obj to it
	  (let* ((final-feedback (find-final-feedback-obj an-interactor
					     final-feedback-proto)))
	    (dbprint-feed :obj-over final-feedback new-sel-obj an-interactor)
	    (s-value final-feedback :obj-over new-sel-obj)))))))

;;; Adds (if add-p is T) or removes (if add-p is NIL) any final-feedback
;;; objs refering to newval, but leaves rest of the final feedback
;;; objects alone
(defun List-Final-Feedback-Obj (an-interactor newval add-p)
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
	      (dbprint-feed :obj-over feed-for-newval NIL an-interactor)
	      (s-value feed-for-newval :obj-over NIL)
	      (s-value an-interactor :final-feed-avail
		       (cons feed-for-newval feedback-objs-avail))
	      (s-value an-interactor :final-feed-inuse
		       (delete feed-for-newval feedback-objs-in-use))))))))


;;; create an instance of final-feedback-proto unless it is a constant
(defun Get-New-feedback-Obj (final-feedback-proto)
  (let ((new-obj (create-instance NIL final-feedback-proto)))
    (with-constants-disabled
	(opal:add-component (g-value final-feedback-proto :parent)
			    new-obj))
    new-obj))


;;; find an instance of final-feedback-proto in the available list of
;;; final feedback objects. This allows the prototype to change,
;;; e.g. because there is a formula in the :final-feedback-obj slots
;;; which might depend on the object that the mouse is over.  This
;;; routine maintains the slots  :final-feed-avail, :final-feed-inuse
;;; and :final-feedback-proto, so don't set the slots after calling
;;; this function.
(defun Find-Final-Feedback-Obj (inter final-feedback-proto)
  (let ((available-objs (get-local-value inter :final-feed-avail))
	final-feedback-obj final-feedback-protos)
    ;; search if one of the current type is availble
    (setf final-feedback-obj (car (member final-feedback-proto available-objs
					 :test #'(lambda (item list-obj)
						   (is-a-p list-obj item)))))
    (cond (final-feedback-obj
	   )
	  ;; if there is no final feedback object of the desired type
	  ;; available, check to see if the the prototype final feedback has
	  ;; been added to the :final-feedback-protos list. If it has not,
	  ;; add it to this list and make the final feedback object be
	  ;; the prototype.
	  ((not (member final-feedback-proto
		   (setf final-feedback-protos
			 (get-local-value inter :final-feedback-protos))))
	   (setf final-feedback-obj final-feedback-proto)
	   (s-value inter :final-feedback-protos
		    (push final-feedback-proto final-feedback-protos)))
	  ;; create an instance of the final feedback prototype
	  (t (setf final-feedback-obj
		   (get-new-feedback-obj final-feedback-proto))
	     (if-debug inter
		       (format T "----Allocating final feedback obj:~s~%"
			       final-feedback-obj))))
    ;; now have a final-feedback-obj
    (s-value inter :final-feed-avail  ;; no-op if not there
	     (delete final-feedback-obj available-objs))
    (s-value inter :final-feed-inuse
	     (cons final-feedback-obj
		   (get-local-value inter :final-feed-inuse)))
    final-feedback-obj))

;;;initialize the final-feedback internal slots if necessary--should not
;;;be necessary in new version. Keep around just in case code is needed.
(defun Check-Start-Final-Feedback-Obj (an-interactor)
  (declare (ignore an-interactor)))
#|
  (when (and (g-value an-interactor :final-feedback-obj)
	     (null (get-local-value an-interactor :final-feed-avail))
	     (null (get-local-value an-interactor :final-feed-inuse)))
    (s-value an-interactor :final-feed-avail
	     (list (g-value an-interactor :final-feedback-obj)))))
|#


;;;;===========================================================
;;; Exported "useful" functions
;;;============================================================

(defun Return-Final-Selection-Objs (an-interactor)
"Returns a list of all the final-feedback objects currently in use by the
interactor.  This can be used to have another interactor operate on the
final feedback objects (e.g., moving from the selection handles)."
  (when (g-value an-interactor :final-feedback-obj)
    (copy-list (get-local-value an-interactor :final-feed-inuse))))

(defun DeSelectObj (an-interactor obj)
"Cause obj to no longer be selected.  Turns off the final-feedback
objects and clears the various selected slots appropriately.  If obj is
not selected, this does nothing."
  (let ((how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(obj-sel-slot (obj-sel-slot an-interactor)))
    (Check-Start-Final-Feedback-Obj an-interactor)
    (when (null main-agg) ; hasn't been run yet
      (setq main-agg
	    (s-value an-interactor :main-aggregate
		   (get-gob-of-where (g-value an-interactor :start-where)))))
    (setq how-set
	  (case how-set
	    ((:list-add :list-remove :list-toggle) :list-remove)
	    ((:set :clear :toggle) :clear)))
    ; first do object itself
    (when obj-sel-slot
      (dbprint-sel obj-sel-slot obj NIL an-interactor)
      (s-value obj obj-sel-slot NIL))
    ; now do aggregate
    (if (eq main-agg obj)
	;; if no aggregate, then just clear any final-feedbacks
	(Clear-Finals-And-Set an-interactor
		      (get-local-value an-interactor :final-feed-inuse))
	;; otherwise, do the aggregate and any final-feedback objects
	(Calc-Set-Agg-Slot an-interactor main-agg obj how-set))
    obj))

(defun SelectObj (an-interactor obj)
  "Cause obj to be selected.  Turns on the final-feedback
  objects and sets the various selected slots appropriately.  Does not
  check whether obj is part of the domain of an-interactor (in start-where)."
  (let ((how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(agg-selected-slot (agg-sel-slot an-interactor)))
    (Check-Start-Final-Feedback-Obj an-interactor)
    (when (null main-agg) ; hasn't been run yet
      (setq main-agg
	    (s-value an-interactor :main-aggregate
		   (get-gob-of-where (g-value an-interactor :start-where)))))
    (setq how-set
	  (case how-set
	    ((:list-add :list-remove :list-toggle) :list-add)
	    ((:set :clear :toggle) :set)))
    ; first do object itself
    (Calc-set-obj-slot an-interactor obj how-set
			 (if (eq obj main-agg)
			     NIL
			     (g-value main-agg agg-selected-slot)))
    ; now do aggregate
    (if (eq main-agg obj)
	;; if no aggregate, then just set the final-feedback, if any
	(One-Final-Feedback-Obj an-interactor obj)
	;; otherwise, do the aggregate and any final-feedback objects
	(Calc-Set-Agg-Slot an-interactor main-agg obj how-set))
    obj))


;;;;===========================================================
;;; Calculating how to set the selected slots
;;;============================================================

(Defun how-set-error (how-set)
  (error
  "** Bad how-set: ~s.  Options are :set :clear :toggle
   :list-add :list-remove :list-toggle <num> (<num> <num>)" how-set))

;;; Sets the selected slot of the object according to how-set.
;;; Other-obj contains the other objects that may need
;;; to be cleared because this one was set.
;;; Does handle when obj or other-obj are not schemas
(defun Calc-set-obj-slot (an-interactor obj how-set other-obj)
  (if-debug an-interactor (format T "how-set=~s~%" how-set))
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
					 (dbprint-sel obj-sel-slot o NIL
						      an-interactor)
					 (s-value o obj-sel-slot NIL)))
				     ;; otherwise, only one object to de-select
				     (when (schema-p other-obj)
				       (dbprint-sel obj-sel-slot other-obj NIL
						    an-interactor)
				       (s-value other-obj obj-sel-slot NIL))))
	  (otherwise)))			; is a number so do nothing

  ;; now set the selected slot of the new object
    (let (val)
      (when (schema-p obj);; otherwise, can't set its selected slot!
	(case how-set
	  ((:set :list-add) (dbprint-sel obj-sel-slot obj T an-interactor)
	   (s-value obj obj-sel-slot T))
	  ((:clear :list-remove) (dbprint-sel obj-sel-slot obj NIL an-interactor)
	   (s-value obj obj-sel-slot NIL))
	  ((:toggle :list-toggle)
	   (setq val (if (g-value obj obj-sel-slot) NIL T))
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

;; used when the new values is :none, this clears out all the
;; selections from the aggregate and the final-feedback-objects
(defun Clear-All-Selected (an-interactor main-agg)
  (if-debug an-interactor (format T "clearing all selections from ~s~%"
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
	      (dbprint-sel obj-sel-slot o NIL an-interactor)
	      (s-value o obj-sel-slot NIL)))
	  ; otherwise, only one object to de-select
	  (when (and (schema-p other-obj) (kr::schema-name other-obj))
	    (dbprint-sel obj-sel-slot other-obj NIL an-interactor)
	    (s-value other-obj obj-sel-slot NIL))))
      ;; then clear out the aggregate's slot
      (s-value main-agg agg-sel-slot NIL)))

  (when (g-value an-interactor :final-feedback-obj) ; then we are doing final
						    ; feedback objects
    (Clear-Finals-And-Set an-interactor
			  (get-local-value an-interactor :final-feed-inuse))))

;;; Sets the selected slot of the aggregate that the selected object is in
;;; according to how-set.  Newval is the new object selected.
(defun Calc-set-agg-slot (an-interactor agg newval how-set)
  (let* ((agg-sel-slot (agg-sel-slot an-interactor))
	 (old-sel (g-value agg agg-sel-slot))
	val)
    (when (schema-p agg)
      (case how-set
	(:set (dbprint-sel agg-sel-slot agg newval an-interactor)
	      (s-value agg agg-sel-slot newval)
	      (One-Final-Feedback-Obj an-interactor newval))
	(:clear (dbprint-sel agg-sel-slot agg NIL an-interactor)
		(s-value agg agg-sel-slot NIL)
		(One-Final-Feedback-Obj an-interactor NIL))
	(:toggle (setq val
		       (if (listp old-sel)
			   ; then converting from a list to a single value
			   (if (member newval old-sel)
			       NIL ;if used to be selected, then clear
			       newval) ; else select just this one
			   ; otherwise, just check the old single value
			   (if (eq old-sel newval)
			       NIL ;if used to be selected, then clear
			       newval))) ; else select this one
		 (dbprint-sel agg-sel-slot agg val an-interactor)
		 (s-value agg agg-sel-slot val)
		 (One-Final-Feedback-Obj an-interactor val))
	(:list-add
	 (cond ((listp old-sel)
		(pushnew newval (g-value agg agg-sel-slot))
		(dbprint-sel agg-sel-slot
			     agg (g-value agg agg-sel-slot) an-interactor))
	       ((schema-p old-sel)
		       ;; make it into a list (in case how-set changed
		       ;; from single-selectable to be multiple selectable).
		       ;; If new obj same as old, only include once, however.
		(setq val (if (eq newval old-sel)
			      (list newval)
			      (list newval old-sel)))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val))
			     ;; otherwise, throw away old value
	       (t (setq val (list newval))
		  (dbprint-sel agg-sel-slot agg val an-interactor)
		  (s-value agg agg-sel-slot val)))
	 (List-Final-Feedback-Obj an-interactor newval T)) ; add newval
	(:list-remove
	 (cond ((listp old-sel)
		(setq val (delete newval (g-value agg agg-sel-slot)))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)
		(Mark-As-Changed agg agg-sel-slot)) ;; s-value may not cause
					;slot to be marked since its value is
					;a list which will be
					;destructively modified by delete
	       ;; else convert to a list
	       ((eq old-sel newval) (dbprint-sel agg-sel-slot agg NIL
						 an-interactor)
		(s-value agg agg-sel-slot NIL)) ;remove old
	       ((schema-p old-sel) (setq val (list old-sel))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)) ;keep old
	       (t (dbprint-sel agg-sel-slot agg NIL an-interactor)
		  (s-value agg agg-sel-slot NIL))) ; bad old value, remove it
	 (List-Final-Feedback-Obj an-interactor newval NIL)) ; remove newval
	(:list-toggle
	 (cond ((listp old-sel)
		(if (member newval old-sel)
		    (progn
		      (setq val (delete newval old-sel))
		      (dbprint-sel agg-sel-slot agg val an-interactor)
		      (s-value agg agg-sel-slot val)
		      (Mark-As-Changed agg agg-sel-slot) ; s-value may not
				  ; cause slot to be marked since its value is
				  ; a list which will be
				  ; destructively modified by delete
		      (List-Final-Feedback-Obj an-interactor newval NIL)) ;remove
		    (progn
		      (push newval (g-value agg agg-sel-slot))
		      (dbprint-sel agg-sel-slot agg (g-value agg agg-sel-slot)
				   an-interactor)
		      (List-Final-Feedback-Obj an-interactor newval T)))) ;add
	       ; otherwise, if was the old value, now none
	       ((eq old-sel newval) (dbprint-sel agg-sel-slot agg NIL
						 an-interactor)
		(s-value agg agg-sel-slot NIL)
		(List-Final-Feedback-Obj an-interactor newval NIL)) ;remove
	       ; if was a different object, use both
	       ((schema-p old-sel)
		(setq val (list newval old-sel))
		(dbprint-sel agg-sel-slot agg val an-interactor)
		(s-value agg agg-sel-slot val)
		(List-Final-Feedback-Obj an-interactor newval T)) ;add
	       (t (setq val (list newval))   ; bad old val, remove it
		  (dbprint-sel agg-sel-slot agg val an-interactor)
		  (s-value agg agg-sel-slot val)
		  (List-Final-Feedback-Obj an-interactor newval T)))) ;add
	(otherwise ; is a number, already incremented object's selected slot,
	 ; here just note newval
	 (dbprint-sel agg-sel-slot agg newval an-interactor)
	 (s-value agg agg-sel-slot newval)
	 (List-Final-Feedback-Obj an-interactor newval T)))))) ;add


;;;;===========================================================
;;; Menu Interactors
;;;============================================================

;;;;===========================================================
;;; Default Procedures to go into the slots
;;;============================================================

(declaim (special Menu-Interactor))

(defun Menu-Interactor-Initialize (new-Menu-schema)
  (if-debug new-Menu-schema (format T "Menu initialize ~s~%" new-menu-schema))
  (Check-Interactor-Type new-Menu-schema inter:menu-interactor)
  (Check-Required-Slots new-Menu-schema)
  (check-slots-to-set new-Menu-schema)
  (Set-Up-Defaults new-Menu-schema)
  (s-value new-Menu-schema :remembered-last-object NIL) ; this slot must be local
  ) ;end initialize procedure

(defun Menu-Int-Running-Action (an-interactor prev-obj-over new-obj-over)
  (if-debug an-interactor (format T "Menu int-running, old = ~s, new= ~s~%"
				  prev-obj-over new-obj-over))
  (unless (eq prev-obj-over new-obj-over)
    (let ((interim-sel-slot (interim-sel-slot an-interactor))
	  (feedbackobj (g-value an-interactor :feedback-obj)))
      (when feedbackobj
	(dbprint-feed :obj-over feedbackobj new-obj-over an-interactor)
	(s-value feedbackobj :obj-over new-obj-over))
      (when (and interim-sel-slot prev-obj-over
		 (schema-p prev-obj-over))
	(dbprint interim-sel-slot prev-obj-over NIL an-interactor)
	(s-value prev-obj-over interim-sel-slot NIL))
      (when (and interim-sel-slot
		 new-obj-over (schema-p new-obj-over))
	(dbprint interim-sel-slot new-obj-over T an-interactor)
	(s-value new-obj-over interim-sel-slot T)))))

(defun Menu-Int-Start-Action (an-interactor obj-under-mouse)
  (if-debug an-interactor (format T "Menu int-start over ~s~%" obj-under-mouse))
  (kr-send an-interactor :running-action
	   an-interactor NIL obj-under-mouse))  ;turn on feedback

(defun Menu-Int-Outside-Action (an-interactor outside-control prev-obj-over)
  (if-debug an-interactor (format T "Menu int-outside, old = ~s~%" prev-obj-over))
  (unless (eq :last outside-control)
    (kr-send an-interactor :running-action
	   an-interactor prev-obj-over NIL)))

(defun Menu-Int-Back-Inside-Action (an-interactor outside-control
					       prev-obj-over new-obj-over)
  (if-debug an-interactor (format T "Menu int-back-inside, old = ~s, new= ~s~%"
				  prev-obj-over new-obj-over))
  (kr-send an-interactor :running-action an-interactor
	   (if (eq :last outside-control) prev-obj-over NIL)
	   new-obj-over))

(defun Menu-Int-Stop-Action (an-interactor final-obj-over)
  (if-debug an-interactor (format T "Menu int-stop over ~s~%" final-obj-over))
  (let ((feedbackobj (g-value an-interactor :feedback-obj))
	(how-set (g-value an-interactor :how-set))
	(main-agg (g-value an-interactor :main-aggregate))
	(interim-sel-slot (interim-sel-slot an-interactor))
	(agg-sel-slot (agg-sel-slot an-interactor))
	(obj-sel-slot (obj-sel-slot an-interactor)))
    (when feedbackobj
      (dbprint-feed :obj-over feedbackobj NIL an-interactor)
      (s-value feedbackobj :obj-over NIL))
    (when final-obj-over
      (when (and interim-sel-slot (schema-p final-obj-over))
	(dbprint interim-sel-slot final-obj-over NIL an-interactor)
	(s-value final-obj-over interim-sel-slot NIL))
      (when (schema-p main-agg)
	(Calc-set-obj-slot an-interactor
			 final-obj-over how-set
			 ; old-object is the one that used to be selected,
			 ; and get it from the aggregate, if any
			 (if (eq final-obj-over main-agg)
			     NIL
			     (g-value main-agg agg-sel-slot)))))
    (if (eq :none final-obj-over)
	(Clear-All-Selected an-interactor main-agg)
	; else handle the new object normally
	(when (and main-agg (schema-p main-agg))
	  (if (eq final-obj-over main-agg) ;; if eq, then selected already set,
				    ; but still need to do final-feedback-obj
	      (One-Final-Feedback-Obj an-interactor
			       (if (and obj-sel-slot
					(g-value final-obj-over obj-sel-slot))
				   final-obj-over NIL))
	      ;; else set the selected slot of the main-agg.  This procedure
	      ;; will also handle the final-feedback-obj
	      (Calc-set-agg-slot an-interactor main-agg
				 final-obj-over how-set))))
    (KR-Send an-interactor :final-function an-interactor final-obj-over)))

(defun Menu-Int-Abort-Action (an-interactor final-obj-over)
  (if-debug an-interactor (format T "Menu int-abort over ~s~%" final-obj-over))
  (kr-send an-interactor :running-action an-interactor
	   final-obj-over NIL))


;;; Go procedure utilities

;;; remove from running level, put on start level, change state to
;;; start, call abort procedure.  Become-inactive ignored because :active
;;; set before this is called
(defun menu-do-abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Menu aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	       (get-local-value an-interactor :remembered-last-object)))

;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running)
;;; save object over, call start procedure.
(defun menu-do-start (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Menu starting over ~s~%" new-obj-over))

  (s-value an-interactor :remembered-last-object new-obj-over)
  (Fix-Running-Where an-interactor new-obj-over)
  (s-value an-interactor :main-aggregate
           (get-gob-of-where (Get-Running-where an-interactor)))
  (Check-Start-Final-Feedback-Obj an-interactor)
  (if (g-value an-interactor :continuous)  ;then will go to running state
    (progn
      (GoToRunningState an-interactor T)
      (kr-send an-interactor :start-action an-interactor new-obj-over))
    ;else call stop-action
    (progn
      (GoToStartState an-interactor NIL)
      (kr-send an-interactor :stop-action an-interactor new-obj-over))))


;;; call outside procedure, clear saved obj, change state to outside
(defun menu-do-outside (an-interactor)
  (if-debug an-interactor (format T "Menu outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :outside)
	       (g-value an-interactor :remembered-last-object))
  (unless (eq :last (g-value an-interactor :outside))
    (s-value an-interactor :remembered-last-object NIL)))

;;;check to see if need to stop or abort based on whether :outside = :last
(defun menu-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Menu stop outside~%"))
  (if (eq :last (g-value an-interactor :outside))
      (menu-do-stop an-interactor (g-value an-interactor
					:remembered-last-object) event)
      (menu-do-abort an-interactor NIL event)))

;;; call back-inside procedure, change state to running
(defun menu-do-back-inside (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Menu back-inside over ~s~%" new-obj-over))
  (s-value an-interactor :current-state :running)
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (kr-send an-interactor :back-inside-action an-interactor
		 (g-value an-interactor :outside) prev-obj-over new-obj-over)
    (s-value an-interactor :remembered-last-object new-obj-over)))

;;;if new object is different from old one, call running-procedure
(defun menu-do-running (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Menu running over ~s~%" new-obj-over))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (unless (eq prev-obj-over new-obj-over)
      (kr-send an-interactor :running-action an-interactor prev-obj-over
	       new-obj-over)
      (s-value an-interactor :remembered-last-object new-obj-over))))

;;;if new-obj-over not equal to :remembered-last-object, then call
;;; running-action on :remembered-last-object so its interim-feedback can
;;; be removed.  Then, remove from running level, add to start level
;;; change state to start, call stop procedure
(defun menu-do-stop (an-interactor new-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Menu stop over ~s~%" new-obj-over))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (unless (eq prev-obj-over new-obj-over)
      (kr-send an-interactor :running-action an-interactor prev-obj-over
		   new-obj-over)
      (s-value an-interactor :remembered-last-object new-obj-over)))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor new-obj-over))

;;; This is used if explicitly call Stop-Interactor.  It uses the last
;;; selected object
(defun menu-explicit-stop (an-interactor)
  (if-debug an-interactor (format T "Menu explicit stop~%"))
  (let ((prev-obj-over (g-value an-interactor :remembered-last-object)))
    (if prev-obj-over
	(progn
	  (GoToStartState an-interactor T)
	  (kr-send an-interactor :Stop-Action an-interactor prev-obj-over))
	(menu-do-abort an-interactor NIL NIL))))

;;; Menu schema
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
					; slots: interim, in object, in aggregate
	       (:remembered-last-object nil)
	       (:main-aggregate nil)
	       (:go 'general-go)
	       ;; proc executed when events happen. these are called
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

;;; Need special destroy to remove the extra final feedback objects
;;; that may have been allocated.
(define-method :destroy-me
    menu-interactor (an-interactor &optional (erase T))
    (if-debug
     an-interactor
     (format T "Menu special destroy ~s erase=~s~%" an-interactor erase))
    (Destroy-Extra-Final-Feedback-Objs an-interactor erase)
    (call-prototype-method an-interactor erase))
