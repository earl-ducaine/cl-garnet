;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; lapidary's functions for save and restore
;;;

;;; CHANGE LOG
;;;
;;; 07/14/93 amickish - Get-Gilt-Bitmap ---> opal:Get-Garnet-Bitmap
;;; 08/25/92 amickish - Commented out declaration of export-p in Show-Save-
;;;                     Dialog because its use is commented out

(in-package "LAPIDARY")

;;; This function loads the bitmap specified from the Gilt directory
(defun Get-Gilt-Bitmap (bitmapname)
  (opal:read-image (merge-pathnames bitmapname common-lisp-user::Garnet-Gilt-Bitmap-PathName)))

(defparameter HourGlassCursor
  (cons (create-instance NIL opal:bitmap
			 (:image (opal:Get-Garnet-Bitmap "hourglass.cursor")))
	(create-instance NIL opal:bitmap
			 (:image (opal:Get-Garnet-Bitmap "hourglass.mask")))))
(defparameter RegularCursor (g-value opal::window :cursor))

(defvar *Last-Filename* "") ; last file name used to read or save a file
(defvar *Last-WindowName* "") ; last window name used to read a file
(defvar *Last-Package-Name* "COMMON-LISP-USER"
  "last package name used to read or save a file")
(defparameter *Top-Gadget-Name* "TEMP-GADGET") ; name used for the top gadget
(defvar *Top-Level-Obj* nil) ; current object being saved
(defvar *link-values* nil) ; list of link slots and objects they point to;
                           ; used to restore links to their correct values
                           ; after saving an object
(defvar *save-objects* nil) ; copy of set of objects to be saved
(defvar *original-save-objs* nil) ; set of objects to be saved

;; These are slots that should not be put into the file from any objects
(defparameter save-time-do-not-dump-slots
  (list :p-selected :s-selected :selection-type :do-not-dump-slots 
	:p-feedback-obj :s-feedback-obj :box :points :final-feed-inuse
	:final-feed-avail :selected :save-values :obj-over
	:copy-old-window :constant :behaviors :cursor-index
	:saved-cursor-index :mark :current-priority-level 
	:parameters-defined-in-lapidary))

(defvar *editor-agg* nil
  "aggregate to place created objects in")

(defvar point-at-window-inter nil
  "indicates the window a user wants an object loaded into")

(defun SetHourGlassCursor (&optional extrawindows)
  (declare (special editor-win shape-win))
  (dolist (win (g-value *selection-info* :window))
     (s-value win :cursor HourGlassCursor)
     (opal:update win))
  (s-value editor-win :cursor HourGlassCursor)
  (s-value shape-win :cursor HourGlassCursor)
  (opal:update editor-win)
  (opal:update shape-win)
  (dolist (win extrawindows)
    (s-value win :cursor HourGlassCursor)
    (opal:update win)))


(defun RestoreRegularCursor (&optional extrawindows)
  (declare (special editor-win shape-win))
  (dolist (win (g-value *selection-info* :window))
     (s-value win :cursor RegularCursor)
     (opal:update win))
  (s-value editor-win :cursor RegularCursor)
  (s-value shape-win :cursor RegularCursor)
  (opal:update editor-win)
  (opal:update shape-win)
  (dolist (win extrawindows)
    (s-value win :cursor RegularCursor)
    (opal:update win)))
 
;;; compare the given string with the :title slot in each of Lapidary's
;;; drawing windows and return the window that the string matches
(defun get-window-from-string (window-name)
  (dolist (window (g-value *selection-info* :window))
	  (when (or (string= window-name (g-value window :title))
		    (string= window-name (g-value window :icon-title)))
		(return-from get-window-from-string window)))
  ;; if we got this far, we failed to find a window so return nil
  nil)

(defun gadget-values (top-gadget)
"This returns the values of all the named components of the gadget.
The return is of the form:
((:RADIO-B1 (\"Label1\"))(:TEXT-B1 \"Label3\")).  You might use
the function Value-Of to get the appropriate value."
  (let ((values-list NIL))
    (dovalues (part top-gadget :components)
       (if (g-local-value part :known-as)
         (push (list (g-local-value part :known-as)
           (g-value part :value)) values-list)))
      values-list))

(defun value-of (gadget value-list)
"Value-list is the return from gadget-values, which is also the second
parameter to a user-specified OK function.  gadget is the name of a
gadget (e.g. :add-replace) Returns the associated value"
  (cadr (assoc gadget value-list)))

(defun set-initial-value (top-gadget gadget-name value)
"This is used to set the initial value of a component in the dialog box
before it is displayed.  For example:
(set-initial-value TOP-GADGET :rb1 \"Label1\")
This can be done before or after the window is shown."
  (g-value top-gadget gadget-name :value)
  (s-value (g-value top-gadget gadget-name) :value value))

(defun standard-OK-function (top-gadget)
"This is embedded in Gilt-created gadgets to call the user-specified OK
 function"
  (let ((win (g-local-value top-gadget :window)))
    (when win
      (s-value win :visible NIL)
      (kr-send top-gadget :function-for-ok top-gadget
	       (gadget-values top-gadget))
      (opal:update win))))

(defun standard-Apply-function (top-gadget)
"This is embedded in Gilt-created gadgets to call the user-specified
OK function, but does not take down the window" 
  (kr-send top-gadget :function-for-ok top-gadget (gadget-values top-gadget)))

(defun standard-Cancel-function (top-gadget)
"This is embedded in Gilt-created gadgets to handle the cancel function"
  (let ((win (g-local-value top-gadget :window)))
    (when win
      (s-value win :visible NIL)
      (opal:update win))))

(defun OKCancel-Function (gadget value)
  "This is embedded in Gilt-created gadgets to handle the OK, Apply
and cancel functions"
  (cond ((string= value "OK")
	 (standard-OK-function (g-value gadget :parent)))
	((string= value "Apply")
	 (standard-Apply-function (g-value gadget :parent)))
	((string= value "Cancel")
	 (standard-Cancel-function (g-value gadget :parent)))))
#|
(defun show-in-window (top-gadget &optional x y)
"This pops up a window to show the specified gadget.  If x and y
supplied, they are used, otherwise the gadget's :window-left and :window-top
are used."
  (let (agg (win (g-local-value top-gadget :window))
	    (kr::*constants-disabled* nil))
    (if win
	(progn
	  (when x (s-value win :left x))
	  (when y (s-value win :top y))
	  (s-value win :visible T))
        (progn 
          (setq win
            (create-instance NIL inter:interactor-window
              (:left   (or x (g-value top-gadget :window-left)))
              (:top    (or y (g-value top-gadget :window-top)))
              (:width  (g-value top-gadget :window-width))
              (:height (g-value top-gadget :window-height))
              (:title  (g-value top-gadget :window-title))
              (:aggregate (setq agg (create-instance NIL opal:aggregate)))))
          (opal:add-component agg top-gadget)))
    (opal:update win)
    win))
|#
;;; This pops up the save dialog box, after determining the default values 
(defun Show-Save-Dialog (gadget stringsel)
  (declare (ignore gadget stringsel))
  (let* ((filename *Last-Filename*)
	 (save-obj (or (g-value save-file :inter)
		       (car (g-value *selection-info* :selected))))
	 (package (or (and save-obj
			   (g-value save-obj :package-name))
		      *Last-Package-Name*))
	 (window-title "")
;	 (export-p (and save-obj (g-value save-obj :export-p)))
	 (gadget-name *Top-Gadget-Name*))
    (set-initial-value save-file :filename filename)
    (set-initial-value save-file :gadget-name gadget-name)
    (set-initial-value save-file :win-title window-title)
    (set-initial-value save-file :package-name package)
    ;;; ** BROKEN
    ;;;** (set-initial-value save-file :export-p "Export Top-level Gadget?")
    (gilt:show-in-window save-file (g-value save-file :window-left)
		    (g-value save-file :window-top) t)))


(defun Write-Standard-Header (package)
  (format T ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ~a; Base: 10 -*-~%"
	  package)
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (format T ";;; This file created by LAPIDARY ~a: A Garnet Interface Builder~%" 
	  Common-Lisp-User::Garnet-Version-Number)
  (format T ";;; on ~a~%" (inter::time-to-string))
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")  
  (format T "(in-package ~s)~%~%" package)
  (Format T "(setf common-lisp-user::*Used-Garnet-Version* ~s)~%~%" Common-Lisp-User::Garnet-Version-Number)
  (format t ";;;~%")
  (format t ";;;     Functions needed from Lapidary~%")
  (format t "(load (merge-pathnames \"lapidary-functions-loader\" common-lisp-user::Garnet-Lapidary-PathName))~%"))


;;; This is called by the Save-file dialog box when OK is hit.  Values is a
;;; list of the gadgets and their values. An interactor, a set of selected
;;; objects, or a window may be saved. Save assumes that a window is being
;;; saved in the :inter slot of gadget is nil and no objects are selected
(defun Do-Save-File (gadget values)
  (let* ((kr::*print-as-structure* nil)
	 (filename (value-of :filename values))
	(gadget-name (value-of :gadget-name values))
	(window-title (value-of :win-title values))
	(inter (g-value gadget :inter))
	; window-p indicates if window is being saved
	(window-p (not (or inter
			   (g-value *selection-info* :selected)))) 
	(package (string-upcase (value-of :package-name values)))
	(export-p (if (car (value-of :export-p values))
		      T NIL)) ;use T instead of string name
	window)
  (cond ((string= "" filename) (Lapidary-Error "Filename must be supplied"))
	((and window-p (string= "" gadget-name))
	 (Lapidary-Error "Gadget name must be supplied"))
	((and window-p (string= "" window-title))
	 (Lapidary-Error "Window name must be supplied--A valid window title is
one that appears on a window's title-bar or on a window's icon"))
	((and window-p 
	      (null (setf window (get-window-from-string window-title))))
	 (lapidary-error 
	  (format nil "Could not find window ~S--A valid window title is
one that appears on a window's title-bar or on a window's icon" 
		  window-title)))
	((string= "" package) (Lapidary-Error "Package name must be supplied"))
	(T 
	 ;; find objects to be saved
	 (setf *original-save-objs*
	       (or (when inter (list inter))
		   (when window
			 (let ((agg (g-value window :editor-agg)))
			   (s-value agg :known-as 
				    (keyword-from-string gadget-name))
			   (list agg)))
		   (g-value *selection-info* :selected)))

	 ;; must do an update here to make the save dialog box disappear.
	 ;; if we do not do this, then fix-up-all-interactors will try
	 ;; to register copies of interactors that are being saved with
	 ;; the save dialog box window. This registration does not work
	 ;; properly and causes the destroy method to crash when it tries
	 ;; to get rid of these copies.
	 (opal:update-all)

	 ;; destroy corrupted slots in the objects to be saved and 
	 ;; determine if there are any links that might be parameters
	 
	 (dolist (obj *original-save-objs*)
		 (fix-up-obj obj)
		 (check-for-links obj obj
				  (transitive-closure obj) gadget))

	 ;; create copies of the objects to be saved. the save routine
	 ;; will modify the copies by removing most of the lapidary support
	 ;; slots and then write out the copies.
	 (with-constants-disabled
	  (setf *save-objects*
	       (mapcar #'(lambda (obj) 
			   (let ((new-obj (opal:copy-gadget obj nil)))
			     (setf (kr::schema-name new-obj) 
				   (kr::schema-name obj))
			     new-obj))
		       *original-save-objs*))

	 ;; fix the :active, :visible, and :window slots of all interactors and
	 ;; their feedback objects
	  (dolist (obj *save-objects*)
		  (fix-all-interactors obj t)))

	 (Set-Do-Not-Dump-Slot-For-Save *save-objects*)

	 (dolist (obj *save-objects*)
		 (setf *top-level-obj* obj)
		 (process-an-obj obj gadget))

	 (setq *Last-Filename* filename)
	 (setq *Last-Package-Name* package)
	 (with-open-file (*standard-output* filename :direction :output
					    :if-exists :supersede)
	   (unless (find-package package)
		   (make-package package))
      
	   ;; now start dumping
	   (write-standard-header package)
	   (when export-p
		 (Format T "~%(export '(")
		 (dolist (obj *save-objects*)
			 (format t "~a " (kr::schema-name obj)))
		 (format t "))~%~%"))

	   (dolist (obj *save-objects*)
		   (s-value obj :package-name package)
		   (s-value obj :export-p export-p))

	   (when window-p
		 (let ((obj (car *save-objects*)))
		   (s-value obj :window-p t)
		   (s-value obj :window-height (g-value window :height))
		   (s-value obj :window-width (g-value window :width))
		   (s-value obj :window-top (g-value window :top))
		   (s-value obj :window-left (g-value window :left))
		   (s-value obj :window-title window-title)))

	   (Format T "(setf common-lisp-user::*Garnet-Objects-Just-Created* (list ~%")
	   (opal:write-gadget (car *save-objects*) T T)
	   ;; set initialize flag for write-gadget to nil for remaining objects
	   (dolist (obj (cdr *save-objects*))
		   (opal:write-gadget obj t nil))
	   (Format T "))~%~%"))	 
	 ;; clean up after save--destroy the copied objects
	 (dolist (obj *save-objects*)
		 (opal:destroy obj))
	 ;; go through the list of original save objects and make their
	 ;; names point to them again. The save process names the copies
	 ;; the same as the originals, so when the copies are destroyed,
	 ;; kr sets the name pointers to nil
	 (dolist (obj *original-save-objs*)
		 (set (kr::schema-name obj) obj))
	 (format T "...Done saving file~%")))))

; set-cursor and restore-cursor
;
(let (previous-cursor)
  (defun set-cursor (new-cursor)
    (let ((windows (g-value *selection-info* :window)))
      (setf previous-cursor (g-value (car windows) :cursor))
    (dolist (win windows)
      (s-value win :cursor new-cursor))))
  (defun restore-cursor ()
    (dolist (win (g-value *selection-info* :window))
      (s-value win :cursor previous-cursor))))


(defun init-cursors ()
  ;;; copy cursor
  (create-instance 'copy-cursor-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-copy.cursor"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (create-instance 'copy-mask-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-copy.mask"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (setf copy-cursor-pair (cons copy-cursor-bitmap copy-mask-bitmap))

  ;;; instance cursor
  (create-instance 'instance-cursor-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-instance.cursor"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (create-instance 'instance-mask-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-instance.mask"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (setf instance-cursor-pair (cons instance-cursor-bitmap instance-mask-bitmap))

  ;;; load cursor
  (create-instance 'load-cursor-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-load.cursor"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (create-instance 'load-mask-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-load.mask"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (setf load-cursor-pair (cons load-cursor-bitmap load-mask-bitmap))

  ;;; move cursor
  (create-instance 'move-cursor-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-move.cursor"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (create-instance 'move-mask-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-move.mask"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (setf move-cursor-pair (cons move-cursor-bitmap move-mask-bitmap))

  ;;; delete cursor
  (create-instance 'delete-cursor-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-delete.cursor"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (create-instance 'delete-mask-bitmap opal:bitmap
		   (:image (opal:read-image
			    (merge-pathnames "lapidary-delete.mask"
					     Common-Lisp-User::Garnet-Bitmap-Pathname))))
  (setf delete-cursor-pair (cons delete-cursor-bitmap delete-mask-bitmap)))

(defun save-restore-do-go ()
  (save-restore-do-stop))
#|
  (create-instance 'POINT-AT-WINDOW-INTER inter:two-point-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:start-where T)
   (:active NIL)
   (:waiting-priority inter:high-priority-level)
   (:self-deactivate T)))
|#
(defun save-restore-do-stop ()
  (when (and (boundp 'point-at-window-inter) point-at-window-inter)
    (opal:destroy point-at-window-inter)))

;;;  ===========
;;;  BITMAP CODE
;;;  ===========

(defun load-bitmap (gadget values)
  (declare (ignore gadget))
  (let* ((filename (value-of :filename values))
	 (window-name (value-of :windowname values))
	 (window (get-window-from-string window-name))
	 (check-value most-positive-fixnum)
	 new-obj editor-agg)

  (cond ((string= "" filename) (Lapidary-Error "Filename must be supplied"))
	((null window)
	 (lapidary-error 
	  (format nil "Could not find window ~S--A valid window title is
one that appears on a window's title-bar or on a window's icon" 
		  window-name)))
	(T (format T "Loading bitmap from file ~s...~%" filename)
	   (setq *Last-WindowName* window-name)
	   (setf editor-agg (g-value window :editor-agg))
	   (setf new-obj (create-instance NIL opal:bitmap
			   (:left 10)
			   (:top 10)
			   (:filename filename)
			   (:image (o-formula (opal:read-image
					       (gvl :filename))))))
	   ;; make sure the new filename is good. if it isn't, destroy
	   ;; the bitmap and get out
	   (unwind-protect
	       (setf check-value (g-value new-obj :image))
	     ;; if check-value is equal to most-positive-fixnum, the
	     ;; filename was bad
	     (when (eq check-value most-positive-fixnum)
		   (opal:destroy new-obj)
		   (format t "The bitmap object was destroyed and lapidary has been restored to a consistent state.")
		   (format t "~% To continue, type (inter:main-event-loop)~%")
		   (opal:update-all)))
	   ;; name the new object
	   (s-value new-obj :known-as 
		    (keyword-from-string (princ-to-string (kr::schema-name new-obj))))

	   (opal:add-component editor-agg new-obj)
	   ;; cause the new object to be selected
	   (deselect-objects)
	   (primary-select new-obj)
	   new-obj))))


;;; ===================
;;; OBJS FROM FILE CODE
;;; ===================

;    Syntax for file of objects:
;  The file should be a series of calls to kr:create-instance, except for
;  the last line which should be a setf, such as
;    (setf *CREATED-INSTANCES* (list OBJ1 OBJ2 ...)).

; set up the cursor when file is loaded
(init-cursors)

;;; find a valid window to read a set of gadgets into. The following 
;;; rules, in order of priority, are used to find a window:
;;; 1. If there are selected objects, the window should be the window 
;;;      the selected objects are in
;;; 2. If the last window that was read into still exists, use that
;;;      window
;;; 3. If any windows exist in Lapidary, use the first one in the :window
;;;      slot of *selection-info*
;;; 4. If all else fails, return an empty string
(defun find-read-window ()
  (or (and (g-value *selection-info* :selected)
	   (g-value (car (g-value *selection-info* :selected)) :window))
      (get-window-from-string *Last-WindowName*)
      (car (g-value *selection-info* :window))))

;;; This pops up the read dialog box, after determining the default values 
(defun Show-Read-Dialog (gadget stringsel)
  (declare (ignore gadget stringsel))
  (let* ((filename *Last-Filename*)
	 (window (find-read-window))
	 (window-name (if window (g-value window :title) ""))
	 (editor-agg (if window (g-value window :editor-agg)))
	 (add-replace-invalid (if (and window 
				       (g-value editor-agg :components))
				  NIL T)))
    (set-initial-value read-file :filename filename)
    (set-initial-value read-file :windowname window-name)
    (s-value (g-value read-file :add-replace-valid) :visible
	     add-replace-invalid)
    (set-initial-value read-file :add-replace "Replace existing objects")
    (gilt:show-in-window read-file (g-value read-file :window-left)
		    (g-value read-file :window-top) t)))

;;; This is called by the Read-file dialog box when OK is hit.  Values is a
;;; list of the gadgets and their values
(defun Do-Read-File (gadget values)
  (let* ((filename (value-of :filename values))
	 (window-name (value-of :windowname values))
	 (window (get-window-from-string window-name))
	(addp (and window
		   (string-equal (value-of :add-replace values)
				 "Add to existing objects")))
	(readdialogwindow (list (g-value gadget :window)))
	new-obj-list)
  (cond ((string= "" filename) (Lapidary-Error "Filename must be supplied"))
	((null window)
	 (lapidary-error 
	  (format nil "Could not find window ~S--A valid window title is
one that appears on a window's title-bar or on a window's icon" 
		  window-name)))
	(T (format T "Loading gadgets from file ~s...~%" filename)
	   (SetHourGlassCursor readdialogwindow)
	   (setf common-lisp-user::*Garnet-Objects-Just-Created* nil)
	   ;; even if the load fails, we want to restore the regular cursor
	   (unwind-protect
	       (progn
		 (format T "Restoring objects...~%")
		 (Load filename))
	     ;; only do the next operations if the load was successful--we
	     ;; assume that the load is successful if 
	     ;; *Garnet-Objects-Just-Created* is set to a non-nil value
	     (if common-lisp-user::*Garnet-Objects-Just-Created*
		 (progn
		   (when (not (listp common-lisp-user::*Garnet-Objects-Just-Created*))
			 (setf common-lisp-user::*Garnet-Objects-Just-Created*
			       (list common-lisp-user::*Garnet-Objects-Just-Created*)))
		   (setq *Last-Filename* filename)
		   (setq *Last-WindowName* window-name)

		 ; determine if the loaded objects are graphical objects that
		 ; will have to be added to a window
		   (when (not (is-a-p (car common-lisp-user::*Garnet-Objects-Just-Created*)
				      inter:interactor))
			 (let* ((first-obj (car common-lisp-user::*Garnet-Objects-Just-Created*))
				(window-p (g-value first-obj :window-p))
				editor-agg)
			   (when (null window)
			         ; create a window in which to put the gadgets
				 (setf window (make-drawing-window)))
			   (s-value window :width
				    (if addp ; if adding objects, then wider
					(max (+ (max-right-side) 10)
					     (g-value window :width))
		            ;; otherwise, maximum right side of created objects
				      (+ (max-right-side) 10)))

			   (s-value window :height
				    (if addp ; if adding objects, then higher
					(max (+ (max-bottom-side) 10)
					     (g-value window :height))	
		           ;; otherwise, maximum bottom side of created objects
				      (+ (max-bottom-side) 10)))

			   (setf editor-agg (g-value window :editor-agg))
 
			   (unless addp 
			   ; if not add, then replace, so use new file's values
				   (s-value editor-agg :package-name
					    (g-value first-obj :package-name))
				   (s-value editor-agg :export-p
					    (g-value first-obj :export-p))

				   ;; delete all old objects
				   (clear-workspace window))

			   ;; now add all objects
			   (if window-p
			       (progn
				 (setq new-obj-list
				       (copy-list (g-value first-obj :components)))
				 (dolist (obj new-obj-list)
					 (opal:remove-component first-obj obj))
				 (setq *Top-Gadget-Name*
				       (or (name-for-schema first-obj)
					   *Top-Gadget-Name*)))
			     (setf new-obj-list common-lisp-user::*Garnet-Objects-Just-Created*))
		   
			   (dolist (obj new-obj-list)
				   (fix-all-interactors obj nil)
				   (when (is-a-p obj opal:view-object)
					 (opal:add-component editor-agg obj)))))
		   (format T "...Done~%"))
	         ;; something went wrong--no objects may have been created,
	         ;; the file name may have been wrong, or the create-instance
	         ;; code was bad.
	         (progn
		   (format t "Either no objects were created or an error was signaled")
		   (format t "~% If no error was signaled, the specified file did not create any objects")
		   (format t "~% If an error was signaled, continue by typing (inter:main-event-loop)~%")
		   (opal:update-all)))
	     ;; now clean up
	     (RestoreRegularCursor readdialogwindow))))))



	   
