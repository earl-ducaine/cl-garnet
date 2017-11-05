;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CHANGE LOG:
;;;  8-Jan-92 Mickish Added :in-progress quarantine slot
;;;  4-Jan-92 Mickish Rewrote clean-up for *garnet-windows* change
;;; 10-Dec-92 Mickish *drawable-to-window-mapping* ---> *garnet-windows*
;;; 22-Jul-92 Mickish Added total-p parameter to update-all
;;; 18-Jun-92 ECP Undid change of 24-Feb-92; rewrote clean-up to do what
;;;		  it was supposed to do.
;;; 18-Apr-92 ECP In update-all, check that windows are not destroyed.
;;;  6-Apr-92 BAM Wrap (let ((kr::*constants-disabled* T)) around all destroying.
;;; 25-Mar-92 Mickish  Dolist-->Do in update-all to remove CMUCL warning
;;; 24-Feb-92 ECP In get-table-contents, remove destroyed windows that
;;;		  are accidentally still in the hash table.
;;; 17-Feb-92 ECP kr::schema-name --> kr::schema-slots
;;; 15-Aug-90 ECP Total rewrite of change-garnet-display.
;;; 21-Aug-90 ECP In clean-up, make sure each window hasn't
;;;               already been destroyed.

(in-package :opal)

;;; Returns all non-destroyed windows.  Windows which are already
;;; destroyed but are accidentally still in the hash table are
;;; removed.
(defun get-table-contents ()
  (error "The function opal::GET-TABLE-CONTENTS has been rendered obsolete.
You can get a list of all Garnet windows by referencing the variable
opal::*garnet-windows*."))

(defun clean-up (&optional (how-to :orphans-only))
  "options are:
  1) :opal => destroy all garnet windows by calling xlib:destroy-window on
              orphaned clx-windows and opal:destroy on non-orphaned windows
  2) :opal-set-agg-to-nil => same as above, but before calling opal:destroy,
              set the aggregate to nil so it won't get destroyed too
  3) :orphans-only => destroy all orphaned garnet windows (the default)
  4) :clx =>  destroy all CLX windows created by Garnet (possibly leaving
              stale pointers to destroyed drawables)
  5) :help => print this message

  return value is how many windows were destroyed"

  (let* ((num-killed 0)
	 (root (g-value gem:device-info :current-root))
	 ;; Store all drawables that were created by Garnet in clx-window-list
	 (clx-window-list (unless (eq how-to :help)
			    (gem:all-garnet-windows root))))
    (case how-to
      ;; destroy all Garnet windows by calling xlib:destroy-window on orphaned
      ;; clx-windows and opal:destroy on non-orphaned windows.
      ;; In the case of :opal-set-agg-to-nil, set the aggregate to nil so it
      ;; won't get destroyed too
      ((:opal :opal-set-agg-to-nil)
       (dolist (clx-window clx-window-list)
	 (let ((opal-window (gem:drawable-to-window root clx-window)))
	   (if (and (schema-p opal-window)
		    (eq clx-window (g-value opal-window :drawable)))
	     ;; The clx-window and opal-window match
	     (with-constants-disabled
		 (if (or (eq how-to :opal-set-agg-to-nil)
			 (not (schema-p (g-value opal-window :aggregate))))
		   (s-value opal-window :aggregate NIL))
		 (opal:destroy opal-window))
	     ;; The clx-window was created for some Opal window
	     ;; that no longer exists -- it is orphaned!
	     (gem:delete-window root clx-window))
	   (incf num-killed)))
       (setf *garnet-windows* NIL))
      (:orphans-only
       ;;  destroy all orphaned garnet windows
       (dolist (clx-window clx-window-list)
	 (let ((opal-window (gem:drawable-to-window root clx-window)))
	   (unless (and (schema-p opal-window)
			(eq clx-window (g-value opal-window :drawable)))
	     (gem:delete-window root clx-window)
	     (incf num-killed)))))
      (:clx
       (dolist (clx-window clx-window-list)
	 (let ((opal-window (gem:drawable-to-window root clx-window)))
	   (if (schema-p opal-window)
	     (s-value opal-window :drawable NIL))
	   (gem:delete-window root clx-window))
	 (incf num-killed))
       (setf *garnet-windows* NIL)
       )
      (:help
       (format t (documentation 'opal::clean-up 'function)))
      (t (format t "options are :opal, :opal-set-agg-to-nil, :orphans-only, :clx, or :help.")))
    num-killed))


(defun change-garnet-display (new-display)
  (disconnect-garnet)
  (reconnect-garnet new-display))

#+garnet-debug
(defun update-all (&optional (total-p NIL))
  ; update all top-level windows
  (dolist (win *garnet-windows*)
    (if (schema-p win)
	(unless (or (g-value win :parent)
		    (g-value win :in-progress))
	  ;; The :in-progress quarantine slot will be reset at the end of the
	  ;; update method.
	  (s-value win :in-progress T)
	  (update win total-p))
	(setf *garnet-windows* (remove win *garnet-windows*))))
  )

;;; The same as update-all above, except without the (if (schema-p ...))
;;;
#-garnet-debug
(defun update-all (&optional (total-p NIL))
  ; update all top-level windows
  (dolist (win *garnet-windows*)
    (unless (or (g-value win :parent)
		(g-value win :in-progress))
      ;; The :in-progress quarantine slot will be reset at the end of the
      ;; update method.
      (s-value win :in-progress T)
      (update win total-p)))
  )

(defun reset-cursor (a-window)
  (s-value a-window :cursor (cons arrow-cursor arrow-cursor-mask))
  (update a-window))
