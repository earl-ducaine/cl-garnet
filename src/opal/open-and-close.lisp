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


;;; CHANGE LOG:
;;   12-Sep-95 goldman  Had to change the way reconnection is done to
;;                      reflect the fact that *colormap-index-table*
;;                      is now a hash table instead of an array.
;;   17-Apr-94 amickish Restored s-value of :lineage to NIL when disconnect;
;;                      Destroyed colormap slots after all.
;;   25-Mar-94 amickish Eliminated redefinition warnings when reconnecting
;;   15-Dec-93 amickish Do not maintain :display of opal::window; maintain
;;                      :active-devices of DEVICE-INFO when destroy *root-window*
;;   18-Nov-93 amickish Destroyed X-DEVICE and *root-window*
;;   24-May-93 koz      Converted kr::set-slot-accessor calls to use
;;                      new KR 2.3 format (one more argument)
;;   19-Apr-93 amickish Destroyed font slots in opal:font-from-files
;;   24-Feb-93 amickish moved *auxilliary-reconnect-routines* to new-defs
;;   02-Feb-93 DZG In disconnect-garnet, call kr::set-slot-accessor on the
;;                 font objects instead of destroy-slot.
;;   01-Feb-93 amickish all-the-instances ---> do-all-instances
;;   13-Jan-93 amickish Now sever X connections to fonts rather than texts
;;   10-Dec-92 amickish *drawable-to-window-mapping* ---> *garnet-windows*
;;   21-Sep-92 amickish No longer necessary to call notice-items-changed on
;;                      menubars, due to reimplementation of :submenu-window-list
;;                      in MENUBAR gadget.
;;   22-Jun-92 ECP It is necessary to call notice-items-changed on
;;                 menubars during the execution of reconnect-garnet.
;;   19-Jun-92 ECP In reconnect-garnet, turn off asynchronous error reports.
;;   29-May-92 ECP/KY Determine display number and screen number from
;;                    full display name, by calling initialize-x11-values.
;;	              If you call disconnect-garnet when already disconnected,
;;		      or reconnect-garnet when already reconnected, exit.
;;   25-May-92 ECP Check that elements of *all-windows* and
;;	           *all-windows-which-have-been-closed* have not
;;		   been destroyed.
;;   06-May-92 ECP Only call main-event-loop-process in reconnect-garnet
;;		   if it had been halted in disconnect-garnet.
;;   16-Apr-92 ECP Call launch-main-event-loop-process at end of
;;		   reconnect-garnet.
;;   30-Mar-92 amickish  Changed funcalls of :update method to update call;
;;                       Changed the way *all-the-windows* is computed in
;;                       Disconnect-Garnet.
;;   25-Mar-92 amickish  Get-Values ---> G-Value
;;   23-Mar-92 ECP In reconnect-windows, must update all the windows,
;;	           not just the visible ones.
;;   20-Mar-92 ECP Moved exports to defs.lisp.  Use process routines.
;;   11-Mar-92 ECP Added references to kr::*constants-disabled*
;;	           When reinitializing colors, just call g-value,
;;		   not s-value.
;;   17-Feb-92 ECP Added *auxilliary-reconnect-routines*
;;   31-Jan-92 ECP Eliminated *display-name-to-display-mapping*.
;;   24-Jan-92 ECP reinitialized text objects in reconnect-garnet.
;;   26-Mar-91 ECP kcl patch
;;   24-Mar-91 ECP Fixed bug involving reconnect to a color screen.
;;   07-Mar-91 ECP The question of whether the screen is color or
;;                 black-and-white is now determined inside
;;                 initialize-default-x-values in defs.lisp.
;;   14-Feb-91 ECP More changes to color for connect and disconnect
;;   08-Feb-91 ECP Added :color-p slot to opal:color to tell if
;;                 screen is black-and-white or color.
;;   11-Sep-90 ECP Get display name in allegro by (sys::getenv "DISPLAY")
;;                 Use (short-site-name) as an #+allegro alternative
;;                 to (machine-instance)
;;   15-Aug-90 ECP Yet more debugging.  Disconnect-garnet must
;;                 set windows :lineage slot to NIL.
;;                 Reconnect-garnet has an optional argument.
;;                 Call to initialize-default-x-values.
;;   14-Aug-90 ECP In reconnect-garnet, just explicitly update
;;	           top level windows.
;;   10-Aug-90 ECP In reconnect-garnet, recompute display name.
;;   21-Mar-90 ECP Lots of debugging, as well as the above comments.
;;   09-Mar-90 ECP Released locally



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
  (if (g-cached-value fnt :xfont)
    (gem:delete-font (g-value device-info :current-root) fnt))
  
  ;; The calls to kr::set-slot-accessor are conceptually the same
  ;; as calls to destroy-slot.  Destroying all these local slots will case
  ;; formulas to be re-inherited and re-evaluated upon reconnection.
  ;;
  ;; amickish 11/18/93 - Commented out because font formulas now depend
  ;; on the DEVICE-INFO schema, and you can make its slots constant if you
  ;; want the formulas to become constant.
  #+comment
  (progn
    (kr::set-slot-accessor fnt :xfont kr::*NO-VALUE* 0 NIL)
    (kr::set-slot-accessor fnt :char-width kr::*NO-VALUE* 0 NIL)
    (kr::set-slot-accessor fnt :max-char-ascent kr::*NO-VALUE* 0 NIL)
    (kr::set-slot-accessor fnt :max-char-descent kr::*NO-VALUE* 0 NIL)
    (kr::set-slot-accessor fnt :font-height kr::*NO-VALUE* 0 NIL)
    (if (is-a-p fnt opal:font)
	(kr::set-slot-accessor fnt :font-from-file kr::*NO-VALUE* 0 NIL)))
  )


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
			*text-xfont*)
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


(defun Reconnect-Garnet (&optional display-name device-type)
  (unless *garnet-has-been-disconnected*
    (return-from reconnect-garnet))

  (let ((current-device (g-value DEVICE-INFO :current-device))
        root-window)

    (unless device-type
      (setf device-type (g-value current-device :device-type)))
    (s-value DEVICE-INFO :active-devices
	     (delete current-device (g-value DEVICE-INFO :active-devices)))
    (destroy-schema (g-value current-device :root-window))
    (destroy-schema current-device)

    ;; Binding this variable prevents warings when gem::*root-window* and
    ;; gem::x-device are redefined as the image is restarted.
    (let (#+allegro (excl:*redefinition-warnings* NIL))
      (gem:init-device device-type display-name))

    (setf root-window (g-value device-info :current-root))

    ;; Should be called in X-Init-Device?
    (set-draw-functions)		; defined in defs.lisp

    ;; you can't call alloc-color-cells on a :true-color or :static-color
    ;; screen...conditionalized it. [1995/12/08:goldman]
    (when *read-write-colormap-cells-p*
      (let ((indices (gem:colormap-property root-window :ALLOC-COLOR-CELLS)))
	(reset-first-allocatable-colormap-index root-window)
	(set-first-allocatable-colormap-index root-window (car indices))
	(gem:colormap-property root-window :FREE-COLORS indices)))

    ;; Re-initialize fonts
    (with-constants-disabled
	(do-all-instances text
	  #'(lambda (txt)
	      (let ((vals (g-cached-value txt :update-slots-values)))
		(if (and vals (eq (aref vals *text-xfont*) :closed))
		  (setf (aref vals *text-xfont*)
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
