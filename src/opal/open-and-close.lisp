;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Close all connections to the X server by saying:
;;      (opal:Disconnect-Garnet)
;;
;;  While the connection to the X server is closed, you may
;;  save a core image of Garnet.  To save a core image:
;;    In CMU Common Lisp say        (ext:save-lisp filename)
;;    In Allegro Lisp say           (excl:dumplisp)
;;    In Lucid Lisp the command is  (disksave filename)
;;
;;  Reopen all connections to the X server by saying:
;;      (opal:Reconnect-Garnet)
;;


(in-package "OPAL")

(defvar *all-the-windows* nil)
(defvar *all-windows-which-have-been-closed* nil)
(defvar *garnet-has-been-disconnected* nil)
(defvar *main-event-loop-process-was-halted* nil)


(defun do-all-instances (obj a-function &key (self NIL))
  (dolist (inst (g-value obj :is-a-inv))
    (do-all-instances inst a-function :self NIL)
    (funcall a-function inst))
  (if self (funcall a-function obj)))


(defun Destroy-Font-Slots (fnt)
  (when (g-cached-value fnt :xfont)
    (gem:delete-font (g-value gem:device-info :current-root) fnt)))


(defun Destroy-Color-Slots (col)
  ;; Formulas will be re-inherited and re-evaluated upon reconnection
  (progn
    (kr::set-slot-accessor col :xcolor kr::*NO-VALUE* 0 NIL)
    (kr::set-slot-accessor col :colormap-index kr::*NO-VALUE* 0 NIL)))

(defun Disconnect-Garnet ()
  (when *garnet-has-been-disconnected*
    (return-from disconnect-garnet))
  (when (main-event-loop-process-running-p)
    (setq *main-event-loop-process-was-halted* t)
    (kill-main-event-loop-process))
  (setq *all-the-windows* (copy-list *garnet-windows*))
  (setq *all-windows-which-have-been-closed* nil)
  ;; Make all the windows invisible.
  (dolist (w *all-the-windows*)
    (when (and (g-value w :visible)
	       (g-value w :drawable))
       (push w *all-windows-which-have-been-closed*)
       (kr:s-value w :visible nil)
       (update w)))			; generalized update

  ;; Remove all connections to X from the font objects,
  ;; and from update-slots array of text objects
  (do-all-instances font #'Destroy-Font-Slots)
  (do-all-instances font-from-file #'Destroy-Font-Slots)
  (do-all-instances color #'Destroy-Color-Slots)

  (do-all-instances font-from-file
    #'(lambda (fnt)
	(kr:s-value fnt :display-xfont-plist nil))
    :self T)

  (do-all-instances text
    #'(lambda (txt)
	(if (g-cached-value txt :update-slots-values)
	    (setf (aref (g-cached-value txt :update-slots-values)
			+text-xfont+)
		  :closed))))

  ;; Remove all connections to X from the window objects.
  (setf *garnet-windows* NIL)
  (dolist (w *all-the-windows*)
    (kr:s-value w :cursor-pairs nil)
    (kr:s-value w :drawable nil)
    (kr:s-value w :lineage nil)
    (kr:s-value w :already-initialized-border-widths nil)
    (kr:s-value w :event-mask nil)
    (when (g-cached-value w :display-info)
      (kr:s-value w :display-info nil)))
  ;; Clear all colors.
  (setf *colormap-index-table* (clrhash *colormap-index-table*))
  (setq *garnet-has-been-disconnected* T))

(defun Reconnect-Garnet ()
  (unless *garnet-has-been-disconnected*
    (return-from reconnect-garnet))
  (let ((current-device (g-value gem:DEVICE-INFO :current-device))
        root-window)
    (s-value gem:DEVICE-INFO :active-devices
	     (delete current-device (g-value gem:DEVICE-INFO :active-devices)))
    (destroy-schema (g-value current-device :root-window))
    (destroy-schema current-device)
    ;; Binding this variable prevents warings when gem::*root-window* and
    ;; gem::x-device are redefined as the image is restarted.
    (gem:init-device)
    (setf root-window (g-value gem:device-info :current-root))
    (with-constants-disabled
      (s-value opal::COLOR :color-p gem:*color-screen-p*))
    ;; you can't call alloc-color-cells on a :true-color or
    ;; :static-color screen...conditionalized it. [1995/12/08:goldman]
    (when gem:*read-write-colormap-cells-p*
      (let ((indices (gem:colormap-property root-window :ALLOC-COLOR-CELLS)))
	(reset-first-allocatable-colormap-index root-window)
	(set-first-allocatable-colormap-index root-window (car indices))
	(gem:colormap-property root-window :FREE-COLORS indices)))
    ;; Re-initialize fonts
    (with-constants-disabled
	(do-all-instances text
	  #'(lambda (txt)
	      (let ((vals (g-cached-value txt :update-slots-values)))
		(if (and vals (eq (aref vals +text-xfont+) :closed))
		  (setf (aref vals +text-xfont+)
			(s-value txt :xfont (g-value txt :font :xfont))))))))
    (dolist (f *auxilliary-reconnect-routines*)
      (funcall f))
    (dolist (w *all-windows-which-have-been-closed*)
      (unless (already-been-destroyed w)
	(kr:s-value w :visible t)))
    (dolist (w *all-the-windows*)
      (unless (or (already-been-destroyed w)
		  (kr:g-value w :parent))
	(update w T)))
    (setf *garnet-windows* *all-the-windows*)
    (when *main-event-loop-process-was-halted*
      (launch-main-event-loop-process))
    (gem:set-window-property root-window :REPORT-ASYNCHRONOUS-ERRORS NIL)
    (setq *garnet-has-been-disconnected* nil)
    t))
