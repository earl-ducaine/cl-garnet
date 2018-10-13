;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNETDRAW; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.


(in-package :garnetdraw)


;;;
;;;   GARNET DRAW
;;;
;;;  Implemented by Vivek Gupta

(defparameter GarnetDraw-Version "2.0")

;; VARIABLES

;; These are the variables used in different
;; parts of the file

;; (declaim (special CREATE-OR-EDIT CURRENT-STATE DRAW-WIN MOVER-GROWER
;; 		  MOVING-AGG MOVING-ARROWLINE NEW-MOVING-ARROWLINE
;; 		  MOVING-DOUBLEARROWLINE
;; 		  MOVING-LINE MOVING-OVAL MOVING-RECT MOVING-ROUNDTANGLE
;; 		  PALETTE-FEEDBACK TEXT-FEEDBACK
;; 		  TOOLS-MENU TOP-DRAW-AGG WIN PS-READ-WIN GRID-WIN SAVE-WIN
;; 		  GRID-OBJ MAIN-MENU COMMON-LISP-USER::*GARNET-OBJECT-JUST-CREATED*
;; 		  EDIT-POLYLINE-INTER CREATOR-DOUBLEARROLINE TOOL-FEEDBACK
;; 		  LINE-FEEDBACK LINE-PALETTE PALETTE-ITEM STIPPLE-PALETTE
;; 		  COLOR-PALETTE-ITEM CREATOR-LINE CREATOR-RECT
;; 		  CREATOR-ROUNDTANGLE CREATOR-OVAL CREATOR-ARROWLINE
;; 		  CREATOR-DOUBLEARROWLINE TOP-AGG COLOR-PALETTE))

(defvar *draw-agg*)
(defvar *grid-db*)
(defvar *q-box*)
(defvar *read-db*)
(defvar *save-db*)

(defparameter *the-color-list* nil)

(create-instance 'grid-obj nil
  ;; Can see gridding?
  (:gridvis nil)
  ;; Gridding is in use?
  (:gridon nil)
  ;; Amount to grid by
  (:gridamt 10))

;; Set with menu bar item for grid on/off
(defparameter *grid-menu-item* nil)

;; Set with menu bar item for grid vis on/off
(defparameter *grid-vis-item* nil)

(defparameter *polygon-maker* nil)

(defparameter rgbvalues
  `((1.00 0.00 0.52) (1.00 0.00 0.82) (,opal:purple-fill ,opal:purple-line)
    (0.82 0.00 1.00)
    (0.52 0.00 1.00) (,opal:blue-fill ,opal:blue-line)
    (0.00 0.52 1.00) (0.00 0.82 1.00)
    (,opal:cyan-fill ,opal:cyan-line) (0.00 1.00 0.82) (0.00 1.00 0.52)
    (,opal:green-fill ,opal:green-line)
    (0.52 1.00 0.00) (0.82 1.00 0.00) (,opal:yellow-fill ,opal:yellow-line)
    (1.00 0.82 0.00)
    (1.00 0.52 0.00) (,opal:red-fill ,opal:red-line)))

(defvar *temp-points* nil)
(defvar *temp-list* nil)
(defvar *clipboard* nil)
(defvar *document-name* "untitled")
(defvar *stored-line-styles*
  (list opal:default-line-style opal:line-1 opal:line-2
	opal:line-4 opal:line-8 opal:red-line opal:green-line opal:blue-line
	opal:yellow-line opal:purple-line opal:cyan-line opal:orange-line))

(defvar *feedback-line-style*
  (create-instance nil opal:line-style
    (:constant t)
    (:line-thickness 3)))

(defvar *garnetdraw-high-priority*
  (create-instance nil inter:priority-level
    (:stop-when :if-any)))
(defvar *garnetdraw-high-running-priority*
  (create-instance nil inter:priority-level
    (:stop-when :if-any)))

;; need a priority level higher than the motif-tab priority level so
;; text editing will take precedence over accelerators
(pushnew *garnetdraw-high-priority* inter:priority-level-list)
(pushnew *garnetdraw-high-running-priority* inter:priority-level-list)


#|
====================================================================
prototypes

below we have the prototypes for all the objects which we are going
to draw.  the first is the one for grouping objects, the rest are
individual objects.
====================================================================
|#


(defun create-moving-prototypes ()
  (create-instance 'moving-agg opal:aggregadget
    (:group-p t)
    (:grow-p t))

  (create-instance 'moving-line opal:line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t)
    (:group-p nil)
    (:line-p t)
    (:draw-function :xor)
    (:fast-redraw-p t)
    (:visible-p nil)
    (:line-style opal:dashed-line))
  (create-instance 'creator-line opal:line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t))

  (create-instance 'new-moving-arrowline garnet-gadgets:arrow-line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:line-p t)
    (:grow-p t)
    (:group-p nil)
    (:visible-p nil)
    (:filling-style nil)
    (:line-style opal:dashed-line)
    (:open-p nil)
    (:parts `((:line :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead :modify (:fast-redraw-p t) (:draw-function :xor)))))
  (create-instance 'creator-arrowline garnet-gadgets:arrow-line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t)
    (:open-p nil))

  (create-instance 'moving-doublearrowline garnet-gadgets:double-arrow-line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:arrow-p t)
    (:arrowhead-p 3)
    (:grow-p t)
    (:group-p nil)
    (:line-p t)
    (:visible-p nil)
    (:filling-style nil)
    (:line-style opal:dashed-line)
    (:open-p nil)
    (:parts `((:line :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead1 :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead2 :modify (:fast-redraw-p t) (:draw-function :xor)))))
  (create-instance 'creator-doublearrowline garnet-gadgets:double-arrow-line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:arrow-p t)
    (:arrowhead-p 3)
    (:grow-p t))

  ;; moving-arrowline is just created so that can read old garnetdraw files
  (create-instance 'moving-arrowline moving-doublearrowline
    (:arrowhead-p 2))

  (create-instance 'moving-rect opal:rectangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top  (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:group-p nil)
    (:grow-p t)
    (:filling-style nil)
    (:line-p nil)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:visible-p nil)
    (:line-style opal:dashed-line))
  (create-instance 'creator-rect opal:rectangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top  (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:grow-p t))

  (create-instance 'moving-roundtangle opal:roundtangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:filling-style nil)
    (:grow-p t)
    (:group-p nil)
    (:visible-p nil)
    (:line-p nil)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:line-style opal:dashed-line))
  (create-instance 'creator-roundtangle opal:roundtangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:grow-p t))

  (create-instance 'moving-oval opal:oval
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:filling-style nil)
    (:grow-p t)
    (:group-p nil)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:visible-p nil)
    (:line-p nil)
    (:line-style opal:dashed-line))
  (create-instance 'creator-oval opal:oval
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:grow-p t))
  )

(defun create-text-feedback ()
  (create-instance 'text-feedback opal:cursor-multi-text
    (:box (list 0 0 0 0))
    (:string "")
    (:visible (o-formula (gvl :obj-over)))
    (:group-p nil)
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))))


;; Dialog box functions
;; since the same save gadget is used for both creating ps files and
;; for saving, this function resets the :selection-function and the
;; :text of the save gadget after it is done.

(defun ps-sel-fn (g v)
  (opal:make-ps-file draw-win v)
  (s-value g :selection-function 'save-sel-fn)
  (s-value (g-value g :text) :string "saving..."))

(defun read-sel-fn (g v)
  (declare (ignore g))
  (let ((filename v))
    (if (probe-file filename)
	(progn
	  (gg:set-selection mover-grower nil)
	  (if (schema-p *draw-agg*) (opal:destroy *draw-agg*))
	  (setf *document-name* filename)
	  (with-constants-disabled
	    (load *document-name*))
	  (unless (schema-p *draw-agg*)
	    ;; then is a new style file, have to set *draw-agg*
	    (setf *draw-agg* common-lisp-user::*garnet-object-just-created*))
	  (s-value mover-grower :start-where (list :element-of-or-none
						   *draw-agg*))
	  (s-value create-or-edit :start-where
		   (list :element-of-or-none *draw-agg*
			 :type opal:cursor-multi-text))
	  (s-value polygon-maker :start-where
		   (list :in *draw-agg*))
	  (gg:standard-initialize-gadget main-menu mover-grower *draw-agg*)
	  ;; This is necessary because the items functions are called
	  ;; by the motif-menu sitting in the submenus
	  (dolist (menu (g-value main-menu :menubar-items :components))
	    (gg:standard-initialize-gadget (g-value menu :submenu)
					   mover-grower *draw-agg*))
	  (with-constants-disabled
	    (opal:add-component top-draw-agg *draw-agg* :where :back))
	  (s-value main-window :title (file-namestring *document-name*)))
	(gg:display-query *q-box* "there is no file by that name"
			  '("ok")))))

(defun read-file ()
  (gg:display-load-gadget-and-wait *read-db* *document-name*)
  )

(defun write-draw-agg ()
  (with-open-file (*standard-output* *document-name*
		   :direction :output :if-exists :supersede)
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (format t ";;; this file created by garnetdraw ~a~%"
	  garnetdraw-version)
  (format t ";;; on ~a~%" (inter::time-to-string))
  (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
  (format t "(in-package :common-lisp-user)~%(use-package :kr)~%~%")
  (format t "(defparameter common-lisp-user::*used-garnetdraw-version* ~s)~%~%"
	  garnetdraw-version)
  (s-value *draw-agg* :package-name "common-lisp-user")
  (s-value *draw-agg* :window-height (g-value draw-win :height))
  (s-value *draw-agg* :window-width (g-value draw-win :width))
  (s-value *draw-agg* :window-title "garnet draw")
  (format t "(defparameter common-lisp-user::*garnet-object-just-created* ~%")
  (opal:write-gadget *draw-agg* t t)
  (format t ")~%")))

;; this is called by savefun and saveasfun.  if confirm-p is t, it
;; displays the save gadget.  else, it simply saves.

(defun save-file (&key confirm-p)
  (if (or confirm-p (equal *document-name* "untitled"))
      (gg:display-save-gadget-and-wait *save-db* *document-name*)
      (write-draw-agg)))

;; this is the selection function of the save gadget.  it sets the
;; document name to be the filename the gadget returns, v, and
;; calls write-draw-agg, which does the saving.

(defun save-sel-fn (g v)
  (declare (ignore g))
  (setq *document-name* v)
  (write-draw-agg))

;;; Dialog boxes

(defun create-query-gadget ()
  (setf *q-box* (create-instance nil garnet-gadgets:motif-query-gadget
		  (:foreground-color opal:motif-green)
		  (:parent-window main-window))))

(defun create-file-db ()
  (setf *save-db*
	(create-instance nil gg:motif-save-gadget
	  (:foreground-color opal:motif-green)
	  (:min-gadget-width 285)
	  (:parent-window main-window)
	  (:modal-p t)
	  (:top 40)
	  (:button-panel-items '("ok" "cancel"))
	  (:selection-function 'save-sel-fn)
	  (:parts `(:dir-input
		    :file-menu
		    :file-input
		    :message
		    :ok-cancel-buttons
		    (:text ,opal:text
		     (:constant t  :except :string)
		     (:left ,(o-formula (gvl :parent :left)))
		     (:top 10)
		     (:font ,(opal:get-standard-font nil :bold-italic :large))
		     (:string "saving..."))
		    ))))
  (setf save-win (g-value *save-db* :window)))

(defun create-grid-db ()
  (create-instance 'grid-win inter:interactor-window
    (:background-color opal:motif-green)
    (:parent main-window)
    (:modal-p t)
    (:visible nil)
    (:left (o-formula (- (floor (gv main-window :width) 2)
			 (floor (gvl :width) 2))))
    (:top (o-formula (- (floor (gv main-window :height) 2)
			(floor (gvl :height) 2))))
    (:width 345)
    (:height 85))
  (setf *grid-db*
	(create-instance nil opal:aggregadget
	  (:left 0) (:top 0)
	  (:parts
	   `((:text ,opal:text
	      (:constant t)
	      (:left 10) (:top 10)
	      (:font ,(opal:get-standard-font nil :bold-italic :large))
	      (:string "new grid increment:"))
	     (:value ,gg:motif-trill-device
	      (:foreground-color ,opal:motif-green)
	      (:constant t)
	      (:left 225) (:top 5)
	      (:height 30) (:width 100))
	     (:ok-cancel ,gg:motif-text-button-panel
	      (:foreground-color ,opal:motif-green)
	      (:constant t)
	      (:text-offset 5)
	      (:left 114) (:top 40)
	      (:items ("ok" "cancel"))
	      (:final-feedback-p nil)
	      (:gray-width 3) (:shadow-offset 5)
	      (:direction :horizontal)
	      (:selection-function
	       ,#'(lambda (g v)
		    (s-value grid-win :visible nil)
		    (opal:update grid-win)
		    (when (equal v "ok")
		      (s-value grid-obj :gridamt
			       (g-value g :parent :value :value))))))))))
  (s-value grid-win :aggregate *grid-db*))

(defun create-read-db ()
  (setf *read-db*
	(create-instance nil gg:motif-load-gadget
	  (:foreground-color opal:motif-green)
	  (:selection-function #'read-sel-fn)
	  (:min-gadget-width 285)
	  (:modal-p t)
	  (:check-filenames-p nil)
	  (:parent-window main-window)
	  (:top 40)
	  (:button-panel-items '("ok" "cancel"))
	  (:parts
	   `(:dir-input
	     :file-menu
	     :file-input
	     :message
	     (:text ,opal:text
		    (:constant t :except :string)
		    (:left 10) (:top 10)
		    (:font ,(opal:get-standard-font nil :bold-italic :large))
		    (:string "reading..."))
	     (:ok-cancel-buttons :modify
				 (:top ,(o-formula (+ (gvl :parent :file-input :top)
						      (gvl :parent :file-input :height)
						      20))))))))
  (setf ps-read-win (g-value *read-db* :window)))


;;; Menu functions and menubar
;;;
;;; These functions are the necessary functions for the menubar to act
;;; properly on any action by the user.  additional functions must be
;;; added here for cut, paste, copy, and various font functions.


(defun quitfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:save-file-if-wanted *save-db* *document-name*))
    (do-stop)))

(defun psfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (s-value *save-db* :selection-function #'ps-sel-fn)
  (s-value (g-value *save-db* :text) :string "ps file...")
  (gg:display-save-gadget-and-wait *save-db* *document-name*))

(defun openfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:save-file-if-wanted *save-db* *document-name*))
    (read-file)))

(defun newfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:save-file-if-wanted *save-db* *document-name*))
	(gg:set-selection mover-grower nil)
	(opal:remove-component top-draw-agg *draw-agg*)
	(with-constants-disabled
	    (dolist (comp (copy-list (g-value *draw-agg* :components)))
	      (opal:destroy comp)))
	(opal:add-component top-draw-agg *draw-agg*)))

(defun saveasfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (save-file :confirm-p t))

(defun savefun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (save-file))


(defun getfontfrommenu (submenu-item old-font)
  (cond
    ((equal submenu-item " fixed ")
     (opal:get-standard-font :fixed
			     (g-value old-font :face)
			     (g-value old-font :size)))
    ((equal submenu-item " serif ")
     (opal:get-standard-font :serif
			     (g-value old-font :face)
			     (g-value old-font :size)))
    ((equal submenu-item " sans-serif ")
     (opal:get-standard-font :sans-serif
			     (g-value old-font :face)
			     (g-value old-font :size)))
    ((equal submenu-item " roman ")
     (opal:get-standard-font (g-value old-font :family)
			     :roman
			     (g-value old-font :size)))
    ((equal submenu-item " bold ")
     (opal:get-standard-font (g-value old-font :family)
			     :bold
			     (g-value old-font :size)))
    ((equal submenu-item " italic ")
     (opal:get-standard-font (g-value old-font :family)
			     :italic
			     (g-value old-font :size)))
    ((equal submenu-item " bold-italic ")
     (opal:get-standard-font (g-value old-font :family)
			     :bold-italic
			     (g-value old-font :size)))
    ((equal submenu-item " small ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :small))
    ((equal submenu-item " medium ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :medium))
    ((equal submenu-item " large ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :large))
    ((equal submenu-item " very-large ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :very-large))
    (t old-font)))

(defun fontfun (gadget menu-item submenu-item)
  (declare (ignore gadget menu-item))
  (if (g-value mover-grower :value)
      (dolist (item (g-value mover-grower :value))
	(when (g-value item :text-p)
	  (with-constants-disabled
		(s-value item :font
		       (getfontfrommenu submenu-item (g-value item :font)))))))
  ;; always set the global state
  (let* ((text-state (g-value tools-menu :text-tool :text-state))
	 (new-font (getfontfrommenu submenu-item
				    (g-value text-state :font))))
    (s-value text-state :font new-font)))

(defun gridtoggle (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (if (g-value grid-obj :gridon)
      (progn ; turn if off, and make menu so it will turn it on
	(s-value grid-obj :gridon nil)
	(gg:menubar-set-title *grid-menu-item* " turn grid on "))
      (progn; turn if on, and make menu so it will turn it off
	(s-value grid-obj :gridon t)
	(gg:menubar-set-title *grid-menu-item* " turn grid off "))))

(defun gridvisible (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (if (g-value grid-obj :gridvis)
      (progn ; turn if off, and make menu so it will turn it on
	(s-value grid-obj :gridvis nil)
	(gg:menubar-set-title *grid-vis-item* " show grid dots "))
      (progn; turn if on, and make menu so it will turn it off
	(s-value grid-obj :gridvis t)
	(gg:menubar-set-title *grid-vis-item* " hide grid dots "))))

(defun setgrid (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (s-value (g-value *grid-db* :value) :value (g-value grid-obj :gridamt))
  (s-value grid-win :visible t)
  (opal:raise-window grid-win)
  (opal:update grid-win))

;;; This function hides the mover-grower handles. This is called by
;;; both the accelerator and the edit-polyline-inter.  basically, if
;;; an object is selected and that object is a polyline, then edit it.
;;; else, if the obj passed to the function is a polyline, edit that
;;; instead.
(defun reshape-fn (inter obj &optional extra)
  (declare (ignore inter extra))
  (s-value polygon-maker :reshape-called-p t)
  (let* ((selected-objs (g-value mover-grower :value))
	 (selected-obj (when (not (cdr selected-objs)) (first selected-objs))))
    (if (is-a-p selected-obj opal:polyline)
	(progn
	  (gg::toggle-polyline-handles polygon-maker selected-obj)
	  (gg:set-selection mover-grower nil))
	(if (is-a-p obj opal:polyline)
	    (progn
	      (gg:set-selection mover-grower nil)
	      (gg::toggle-polyline-handles polygon-maker obj))
	    (inter:beep)))))

(defun create-main-menubar ()
  (create-instance 'main-menu garnet-gadgets:motif-menubar
    (:foreground-color opal:motif-green)
    (:min-menubar-width (o-formula (gv main-window :width)))
    (:left 0) (:top 0)
    (:left-offset 60)
    (:title-font (opal:get-standard-font :sans-serif :bold :large))
    (:item-font (opal:get-standard-font :sans-serif :bold :medium))
    (:accel-font (opal:get-standard-font :sans-serif :bold :medium))
    (:items
     '(("  file  " nil
	((" open " openfun)
	 (" new " newfun)
	 (" save " savefun)
	 (" save as " saveasfun)
	 (" create ps " psfun)
	 (" quit " quitfun)))
       ("  edit  " nil
	((" cut " gg:standard-cut) (" copy " gg:standard-copy)
	 (" paste " gg:standard-paste-same-place)
	 (" clear " gg:standard-delete)
	 (" duplicate " gg:standard-duplicate)
	 (" clear all " gg:standard-delete-all)
	 (" to top " gg:standard-to-top)
	 (" to bottom " gg:standard-to-bottom) (" group " gg:standard-group)
	 (" ungroup " gg:standard-ungroup) (" reshape " reshape-fn)
	 (" refresh " gg:standard-refresh)))
       ("  font  " fontfun
	((" fixed ")(" serif ")(" sans-serif ")(" roman ")(" bold ")
	 (" italic ")(" bold-italic ")(" small ")(" medium ")
	 (" large ")(" very-large ")))
       ("  options  " nil
	((" turn grid on " gridtoggle) (" set grid... " setgrid)
	 (" show grid dots " gridvisible)
	 ))
       ))
    (:accelerators
     '((("m-o" :|meta-o|) ("m-n" :|meta-n|) ("m-s" :|meta-s|) nil
	("m-p" :|meta-p|) ("m-q" :|meta-q|))
       (("m-x" :|meta-x|) ("m-c" :|meta-c|) ("m-v" :|meta-v|) ("del" #\rubout)
	("m-d" :|meta-d|) nil ("m-f" :|meta-f|) ("m-j" :|meta-j|)
	("m-g" :|meta-g|) ("m-h" :|meta-h|) ("m-r" :|meta-r|) ("m-l" :|meta-l|))
       (nil nil nil ("m-r" :|meta-r|) ("m-b" :|meta-b|) ("m-i" :|meta-i|)
	("m-t" :|meta-t|) ("m-1" :|meta-1|) ("m-2" :|meta-2|)
	("m-3" :|meta-3|) ("m-4" :|meta-4|))
       (nil nil nil)))
    (:bar-above-these-items
     '(nil
       (" to top ")
       (" roman " " small ")
       nil))))

;;; Tool palette
;;;
;;; this is the list of tools available for drawing objects.  each
;;; contains a bitmap representation of the tool and the location
;;; where it goes.

(defun create-tool-palette ()
  (create-instance 'tool-feedback opal:rectangle
    (:left (o-formula (gvl :obj-over :left)))
    (:top (o-formula (gvl :obj-over :top)))
    (:width (o-formula (gvl :obj-over :width)))
    (:height (o-formula (gvl :obj-over :height)))
    (:visible (o-formula (gvl :obj-over)))
    (:line-style opal:line-4))
  (create-instance 'tools-menu opal:aggregadget
    (:top (+ 2 (g-value main-menu :height)))
    (:selected nil)
    (:parts
     `((:line-tool ,opal:aggregadget
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (gvl :parent :top)))
	(:feedback-object ,moving-line)
	(:creator-object ,creator-line)
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "line.bm"
				   (merge-pathnames "garnetdraw/"
						    common-lisp-user::garnet-bitmap-pathname))))))))
       (:rect-tool ,opal:aggregadget
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :line-tool :top))))
	(:feedback-object ,moving-rect)
	(:creator-object ,creator-rect)
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "rectangle.bm"
				   (merge-pathnames "garnetdraw/"
						    common-lisp-user::garnet-bitmap-pathname))))))))
       (:roundrect-tool ,opal:aggregadget
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :rect-tool :top))))
	(:feedback-object ,moving-roundtangle)
	(:creator-object ,creator-roundtangle)
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "roundrect.bm"
				   (merge-pathnames "garnetdraw/"
						    common-lisp-user::garnet-bitmap-pathname))))))))
       (:oval-tool ,opal:aggregadget
	(:feedback-object ,moving-oval)
	(:creator-object ,creator-oval)
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :roundrect-tool :top))))
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "oval.bm"
				   (merge-pathnames "garnetdraw/"
						    common-lisp-user::garnet-bitmap-pathname))))))))
       (:text-tool ,opal:aggregadget
	(:feedback-object ,text-feedback)
	(:creator-object ,text-feedback)
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :oval-tool :top))))
	(:parts
	 ((:background ,opal:rectangle
		       (:left 5)(:width 32)(:height 32)
		       (:top ,(o-formula (gvl :parent :top)))
		       (:filling-style ,opal:white-fill)
		       (:line-style ,opal:line-0))
	  (:text-state ,opal:text
		       (:left ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
		       (:top ,(o-formula (opal:gv-center-y-is-center-of (gvl :parent))))
		       (:string "t")
		       (:line-style ,(create-instance nil opal:default-line-style))
		       (:font ,(opal:get-standard-font nil nil nil))))))
       (:polygon-tool ,opal:aggregadget
	(:feedback-object ,moving-rect)
	(:creator-object nil)
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :text-tool :top))))
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "polygon.bm"
				   (merge-pathnames
				    "garnetdraw/"
				    common-lisp-user::garnet-bitmap-pathname))))))))
       (:arrowline-tool ,opal:aggregadget
	(:feedback-object ,new-moving-arrowline)
	(:creator-object ,creator-arrowline)
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :polygon-tool :top))))
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "linearrow.bm"
		    (merge-pathnames
		     "garnetdraw/"
		     common-lisp-user::garnet-bitmap-pathname))))))))
       (:doublearrowline-tool ,opal:aggregadget
	(:feedback-object ,moving-doublearrowline)
	(:creator-object ,creator-doublearrowline)
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (+ 31 (gvl :parent :arrowline-tool :top))))
	(:parts
	 ((:bm ,opal:bitmap
	       (:left 5) (:top ,(o-formula (gvl :parent :top)))
	       (:image
		,(opal:read-image
		  (merge-pathnames "doublelinearrow.bm"
				   (merge-pathnames
				    "garnetdraw/"
				    common-lisp-user::garnet-bitmap-pathname))))))))))
    (:interactors
     `((:tool-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:final-feedback-obj ,tool-feedback)
	(:how-set :set)
	(:start-where ,(o-formula (list :element-of (gvl :operates-on))))))))
  (s-value tools-menu :selected (car (g-value tools-menu :components)))
  (s-value tool-feedback :obj-over (car (g-value tools-menu :components))))


;;; get-line-style:
;;;
;;; This function first goes through the *stored-line-styles* list and
;;; checks to see if the required line-style is already there. If it
;;; is, then it returns that line-style.  otherwise, it creates a new
;;; line style with the appropriate thickness and foreground color,
;;; pushes the new style into the *stored-line-styles* list, and
;;; returns it.

(defun get-line-style (thickness fg-color)
  (let ((new-line-style nil))
    ;; first, go through list and see if the needed
    ;; line style already exists.
    (dolist (style *stored-line-styles*)
      (when (and
	     (eql (g-value style :line-thickness) thickness)
	     (eq (g-value style :foreground-color) fg-color))
	(setf new-line-style style)
	(return)))
    ;; if line style doesn't already exist, create it and push
    ;; it onto list.
    (unless new-line-style
      (setf new-line-style
	    (create-instance nil opal:line-style
	      (:line-thickness thickness)
	      (:foreground-color fg-color)))
      (push new-line-style *stored-line-styles*))
    ;; return the new line style, which was either in the list
    ;; or was created.
    new-line-style))

;; Line palette
;;
;; This is the line palette and the function which sets the value of
;; the line style slot of all selected object, if a new value is
;; selected in the line palette.


(defun create-line-palette ()
  (create-instance 'line-feedback opal:rectangle
    (:obj-over nil)
    (:left 2)
    (:top (o-formula (gvl :obj-over :top)))
    (:width 52)
    (:height (o-formula (gvl :obj-over :height)))
    (:filling-style nil)
    (:visible (o-formula (gvl :obj-over)))
    (:line-style opal:line-4))
  (create-instance 'line-palette opal:aggregadget
    (:left 2) (:top 321) (:width 52) (:height 106)
    (:selected nil)
    (:parts
     `((:background ,opal:rectangle
	(:left 2) (:top 321) (:width 52) (:height 106)
	(:filling-style ,opal:white-fill)
	(:line-style ,opal:line-0))
       (:line0 ,opal:line
	(:x1 11) (:y1 332) (:x2 43) (:y2 332)
	(:top 322) (:height 20)
	(:line-thick 0)
	(:line-style ,opal:line-0)
	(:hit-threshold 5))
       (:line1 ,opal:line
	(:x1 11)(:y1 353) (:x2 43) (:y2 353)
	(:top 343) (:height 20)
	(:line-thick 1)
	(:line-style ,opal:line-1)
	(:hit-threshold 5))
       (:line2 ,opal:line
	(:x1 11)(:y1 374) (:x2 43) (:y2 374)
	(:top 364) (:height 20)
	(:line-thick 2)
	(:line-style ,opal:line-2)
	(:hit-threshold 5))
       (:line4 ,opal:line
	(:x1 11)(:y1 395) (:x2 43) (:y2 395)
	(:top 385) (:height 20)
	(:line-thick 4)
	(:line-style ,opal:line-4)
	(:hit-threshold 5))
       (:line8 ,opal:line
	(:x1 11)(:y1 416) (:x2 43) (:y2 416)
	(:top 406) (:height 20)
	(:line-thick 8)
	(:line-style ,opal:line-8)
	(:hit-threshold 5))))
    (:interactors
     `((:line-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:how-set :set)
	(:final-feedback-obj ,line-feedback)
	(:final-function
	 ,#'(lambda (an-interactor final-obj-over)
	      (declare (ignore an-interactor final-obj-over))
	      (when (g-value mover-grower :value)
		(dolist (thing (copy-list (g-value mover-grower :value)))
		  (if (not (g-value thing :group-p))
		      ;; create a new line-style
		      (let ((new-line-style
			     (get-line-style
			      (g-value line-palette :selected :line-thick)
			      (g-value thing :line-style :foreground-color))))
			;; set line style to be new line style
			(s-value thing :line-style new-line-style)))))))
	(:start-where ,(o-formula (list :element-of (gvl :operates-on)
					:type opal:line)))))))
  (let ((line0 (g-value line-palette :line0)))
    (s-value line-palette :selected line0)
    (s-value line-feedback :obj-over line0)))

;;; selected function
;;;
;;; this is used to change the line style or filling color of a selected
;;; object using the current state aggregadget.

(defun selectedfun (an-interactor final-obj-over)
  (declare (ignore an-interactor final-obj-over))
  (when (g-value mover-grower :value)
    (if (g-value current-state :selectable-objs :frame :selected)
	(dolist (thing (g-value mover-grower :value))
	  (unless (g-value thing :group-p)
	    (let ((new-line-style
			     (get-line-style
			      (g-value thing :line-style :line-thickness)
			      (g-value palette-feedback :obj-over :line-hold
				       :foreground-color))))

	      ;; set line style to be new line style
	      (s-value thing :line-style new-line-style))))

	(dolist (thing (g-value mover-grower :value))
	  (unless (g-value thing :group-p)
	    (s-value thing :filling-style
		     (g-value palette-feedback :obj-over :filling-style))))
	))

  (let* ((selectable-objs (g-value current-state :selectable-objs))
	 (frame (g-value selectable-objs :frame))
	 (filler (g-value selectable-objs :filler))
	 (obj-over (g-value palette-feedback :obj-over)))
    (if (g-value frame :selected)
	(progn
	  (s-value frame :line-hold (g-value obj-over :line-hold))
	  (s-value frame :filling-style (g-value obj-over :filling-style)))
	(s-value filler :filling-style (g-value obj-over :filling-style))))
  )

#|
====================================================================
pattern and color palettes

the first one is for the pattern palette, this contains in the items
slot the necessary numbers for creating the different halftone
values.  the second one uses the values in the color list generated
earlier.
====================================================================
|#

(defun create-palette-feedback ()
  (create-instance 'palette-feedback opal:aggregadget
    (:obj-over nil)
    (:left (o-formula (gvl :obj-over :left)))
    (:top (o-formula (gvl :obj-over :top)))
    (:width (o-formula (gvl :obj-over :width)))
    (:height (o-formula (gvl :obj-over :height)))
    (:visible (o-formula (gvl :obj-over)))
    (:fast-redraw-filling-style nil)
    (:parts
     `((:black-rect ,opal:rectangle
	(:left ,(o-formula (+ 1 (gvl :parent :left))))
	(:top ,(o-formula (+ 1 (gvl :parent :top))))
	(:width ,(o-formula (- (gvl :parent :width) 2)))
	(:height ,(o-formula (- (gvl :parent :height) 2)))
	(:line-style ,*feedback-line-style*)
	(:fast-redraw-p :rectangle)
	(:fast-redraw-filling-style
	 ,(o-formula (gvl :parent :fast-redraw-filling-style))))
       (:thin-white-rect ,opal:rectangle
	(:left ,(o-formula (+ 4 (gvl :parent :left))))
	(:top ,(o-formula (+ 4 (gvl :parent :top))))
	(:width ,(o-formula (- (gvl :parent :width) 8)))
	(:height ,(o-formula (- (gvl :parent :height) 8)))
	(:line-style ,opal:white-line)
	(:fast-redraw-p :rectangle)
	(:fast-redraw-filling-style
	 ,(o-formula (gvl :parent :fast-redraw-filling-style)))))))
  )

(defun create-stipple-palette ()
  (create-instance 'palette-item opal:rectangle
    (:left (o-formula (gvl :parent :left)))
    (:top (o-formula (gvl :parent :top)))
    (:width 32)
    (:line-hold
     (create-instance nil opal:line-style
       (:foreground-color opal:black)
       (:line-thickness 0)))
    (:height 32)
    (:filling-style (o-formula
		     (let ((item (nth (gvl :rank) (gvl :parent :items))))
		       (if (schema-p item)
			   item
			   (opal:halftone (nth (gvl :rank)
					       (gvl :parent :items))))))))

  (create-instance 'stipple-palette opal:aggregadget
    (:left 90)
    (:top (o-formula (if (g-value opal:color :color-p) 442 477)))
    (:selected nil)
    (:items `(,opal:white-fill
	      5 10 15
	      ,opal:light-gray-fill 30 35 40
	      ,opal:gray-fill 55 60 65
	      ,opal:dark-gray-fill 80 85 90
	      ,opal:black-fill))
    (:parts
     `((:background ,opal:rectangle
	(:left 87)
	(:top ,(o-formula (if (g-value opal:color :color-p) 439 474)))
	(:width 616)
	(:height ,(if (g-value opal:color :color-p) 73 38))
	(:filling-style ,opal:white-fill)
	(:line-style ,opal:line-0))
       (:patterns-agg ,opal:aggrelist
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (gvl :parent :top)))
	(:selected nil)
	(:items ,(o-formula (gvl :parent :items)))
	(:h-spacing 2)
	(:direction :horizontal)
	(:item-prototype ,palette-item))
       (:nil-text ,opal:text
	(:left 674)
	(:top ,(o-formula (if (g-value opal:color :color-p) 450 485)))
	(:string "nil"))))
    (:interactors
     `((:palette-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:how-set :set)
	(:final-feedback-obj ,palette-feedback)
	(:final-function ,#'selectedfun)
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :patterns-agg))))
	(:start-action
	 ,#'(lambda (inter obj)
	      (let ((fill (g-value palette-feedback :obj-over :filling-style)))
		(s-value palette-feedback :fast-redraw-filling-style fill)
		(s-value (g-value current-state :feedback)
			 :fast-redraw-filling-style (or fill opal:white-fill)))
	      (call-prototype-method inter obj)))))))

  (s-value (car (g-value stipple-palette :patterns-agg :components))
	   :line-hold
	   (create-instance nil opal:line-style
	     (:foreground-color opal:white)
	     (:line-thickness 0)))

  ;; this is the nil filling style.
  (let* ((kr::*constants-disabled* t)
	 (patterns-agg (g-value stipple-palette :patterns-agg))
	 (line-hold (g-value (car (g-value patterns-agg :components))
			     :line-hold)))
    (opal:add-component patterns-agg
         (create-instance 'nil-palette-item opal:rectangle
	   (:left 638) (:width 32) (:height 32)
	   (:top (if (g-value opal:color :color-p) 442 477))
	   (:line-hold line-hold)
	   (:filling-style nil))))
  )

(defun create-color-palette ()
  (create-instance 'color-palette-item opal:rectangle
    (:left (o-formula (gvl :parent :left)))
    (:top (o-formula (gvl :parent :top)))
    (:width 32)(:height 32)
    (:line-hold (o-formula
		 (car (cdr (nth (gvl :rank) (gvl :parent :items))))))
    (:filling-style (o-formula (car (nth (gvl :rank) (gvl :parent :items))))))


  (create-instance 'color-palette opal:aggregadget
    (:left 90) (:top 477)
    (:selected nil)
    (:items (copy-list *the-color-list*))
    (:parts
     `((:colors-agg ,opal:aggrelist
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (gvl :parent :top)))
	(:selected nil)
	(:items ,(o-formula (gvl :parent :items)))
	(:h-spacing 2)
	(:direction :horizontal)
	(:item-prototype ,color-palette-item))))
    (:interactors
     `((:palette-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:how-set :set)
	(:final-feedback-obj ,palette-feedback)
	(:final-function ,#'selectedfun)
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :colors-agg))))
	(:start-action
	 ,#'(lambda (inter obj)
	      (let ((fill (g-value palette-feedback :obj-over :filling-style)))
		(s-value palette-feedback :fast-redraw-filling-style fill)
		(s-value (g-value current-state :feedback)
			 :fast-redraw-filling-style (or fill opal:white-fill)))
	      (call-prototype-method inter obj)))))))
  )

(defun create-fill-palettes ()
  (create-palette-feedback)       ; creates palette-feedback
  (create-stipple-palette)        ; creates stipple-palette
  (if (g-value opal:color :color-p)
      (create-color-palette))     ; creates color-palette

  ;; set the initial selected values
  (s-value (g-value stipple-palette :patterns-agg) :selected
	   (nth 16 (g-value (g-value stipple-palette :patterns-agg) :components)))
  (s-value palette-feedback :obj-over
	   (nth 16 (g-value (g-value stipple-palette :patterns-agg) :components)))
  )

#|
====================================================================
 the-color-list is used to store the list of tuples, made up of
 line-styles and filling-styles.  these are created from the
 list of defined rgbvalues.  this is used to create the color
 palette on screens which can display color.  the function
 create-color-list takes care of the actual creation of the
 list to be stored in the-color-list.
====================================================================
|#

(defun create-color-list ()
  (let ((val 0) l2 l)
    (dotimes (i 18)
      (let* ((triplet (nth val rgbvalues)))
	(if (schema-p (car triplet))
	    ;; must be a pair of styles
	    (push (copy-list triplet) l2)
	    (let ((red (first triplet))
		  (green (second triplet))
		  (blue (third triplet)))
	      (push (create-instance nil opal:line-style
		      (:line-thickness 2)
		      (:foreground-color
		       (create-instance nil opal:color
			 (:red red) (:green green) (:blue blue))))
		    l)
	      (push (create-instance nil opal:filling-style
		      (:foreground-color
		       (create-instance nil opal:color
			 (:red red) (:green green) (:blue blue))))
		    l)
	      (push l l2)
	      (setq l nil))))
      (incf val 1))
    (setq *the-color-list* l2)))


#|
====================================================================
current state

the current-state menu shows the current colors selected for the
line-styles and filling-styles.
====================================================================
|#

(defun create-current-state ()
  (create-instance 'current-state opal:aggregadget
    (:left 2) (:top 446) (:width 52) (:height 52)
    (:parts
     `((:decoration ,opal:aggregadget
	(:left 2) (:top 446) (:width 52) (:height 52)
	(:parts
	 ((:background ,opal:rectangle
		       (:left 2) (:top 446) (:width 52) (:height 62)
		       (:filling-style ,opal:white-fill))
	  (:line-text ,opal:text
		      (:left 4) (:top 491)
		      (:string "line")
		      (:font ,(opal:get-standard-font
			       :sans-serif :roman :small)))
	  (:fill-text ,opal:text
		      (:left 31) (:top 491)
		      (:string "fill")
		      (:font ,(opal:get-standard-font
			       :sans-serif :roman :small))))))
       (:selectable-objs ,opal:aggregadget
	(:left 4) (:top 442) (:width 51) (:height 40)
	(:parts
	 ((:frame ,opal:rectangle
		  (:left 4)(:top 448)(:width 24)(:height 40)
		  (:filling-style ,opal:black-fill))
	  (:filler ,opal:rectangle
		   (:left 27)(:top 448)(:width 24)(:height 40)))))
       (:feedback ,opal:aggregadget
	(:obj-over nil)
	(:left ,(o-formula (gvl :obj-over :left)))
	(:top ,(o-formula (gvl :obj-over :top)))
	(:width ,(o-formula (gvl :obj-over :width)))
	(:height ,(o-formula (gvl :obj-over :height)))
	(:visible ,(o-formula (gvl :obj-over)))
	(:fast-redraw-filling-style nil)
	(:parts
	 ((:black-rect ,opal:rectangle
		       (:left ,(o-formula (+ 1 (gvl :parent :left))))
		       (:top ,(o-formula (+ 1 (gvl :parent :top))))
		       (:width ,(o-formula (- (gvl :parent :width) 2)))
		       (:height ,(o-formula (- (gvl :parent :height) 2)))
		       (:line-style ,*feedback-line-style*)
		       (:fast-redraw-p :rectangle)
		       (:fast-redraw-filling-style
			,(o-formula (gvl :parent :fast-redraw-filling-style))))
	  (:thin-white-rect ,opal:rectangle
			    (:left ,(o-formula (+ 4 (gvl :parent :left))))
			    (:top ,(o-formula (+ 4 (gvl :parent :top))))
			    (:width ,(o-formula (- (gvl :parent :width) 8)))
			    (:height ,(o-formula (- (gvl :parent :height) 8)))
			    (:line-style ,opal:white-line)
			    (:fast-redraw-p :rectangle)
			    (:fast-redraw-filling-style
			     ,(o-formula (gvl :parent :fast-redraw-filling-style)))))))))
    (:interactors
     `((:state-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
	(:how-set :set)
	(:start-where ,(o-formula
			(list :element-of
			      (gvl :operates-on :selectable-objs))))
	(:start-action
	 ,#'(lambda (inter obj)
	      (let* ((feedback-obj (g-value inter :operates-on :feedback))
		     (fill (g-value feedback-obj :obj-over :filling-style)))
		(s-value feedback-obj :fast-redraw-filling-style
			 (or fill opal:white-fill)))
	      (call-prototype-method inter obj)))))))


  (let* ((selectable-objs (g-value current-state :selectable-objs))
	 (frame (g-value selectable-objs :frame))
	 (filler (g-value selectable-objs :filler)))
    (s-value selectable-objs :selected frame)
    (s-value (g-value current-state :feedback) :obj-over frame)
    (s-value frame :selected t)
    (s-value frame :line-hold
	     (g-value palette-feedback :obj-over :line-hold))
    (s-value frame :filling-style
	     (g-value palette-feedback :obj-over :filling-style))
    (s-value filler :filling-style
	     (g-value (car (g-value stipple-palette :patterns-agg :components))
		      :filling-style)))
  )

(defun create-main-window ()
  ;; this is the main window.
  (create-instance 'main-window inter:interactor-window
    (:left 10) (:top 40) (:width 750) (:height 512)
    (:background-color (create-instance nil opal:color
			 (:red 0.65) (:blue 0.65) (:green 0.65)))
    (:position-by-hand nil)
    (:title (concatenate 'simple-string "garnet draw v" garnetdraw-version)))
  ;; this aggregate is where the tool and paint palette are stored,
  ;; and the feedback current selection of the palettes are stored
  ;; here.
  (s-value main-window :aggregate (create-instance 'top-agg opal:aggregate))
  ;; if we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::garnet-note-quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::garnet-note-quitted "garnetdraw"))
     (g-value main-window :destroy-hooks)))
  (create-moving-prototypes) ; creates moving-rect, moving-oval, etc.
  (create-text-feedback)     ; creates text-feedback
  (create-main-menubar)      ; creates main-menu
  (create-line-palette)      ; creates line-palette and line-feedback
  (create-tool-palette)      ; creates tools-menu and tool-feedback
  (create-fill-palettes)     ; creates stipple-palette, color-palette, and
					; palette-feedback
  (create-color-list)
  (create-current-state)     ; creates current-state
  (opal:add-components top-agg
		       tools-menu tool-feedback
		       current-state line-palette line-feedback
		       stipple-palette)
  (if (g-value opal:color :color-p)
      (opal:add-component top-agg color-palette))
  (opal:add-component top-agg palette-feedback)
  (opal:update main-window)
  ;; these functions must be called after win is updated!
  (opal:add-component top-agg main-menu)
  (opal:update main-window)
  (setf *grid-menu-item* (gg:find-submenu-component main-menu "  options  "
						    " turn grid on "))
  (setf *grid-vis-item* (gg:find-submenu-component main-menu "  options  "
						   " show grid dots "))
  (gg:menubar-disable-component *grid-vis-item*) ; not implemented yet
  (gg:menubar-disable-component
   (gg:find-submenu-component main-menu "  edit  " " reshape ")))

;; Mover grower gadget
;; This is the gadget used to move and scale different graphical
;; objects.

;;; If the newly selected object is a *polyline*, enable the "reshape"
;;; item in the menubar.  otherwise, disable it.
(defun mover-grower-sel-fn (gad new-sel)
  (declare (ignore gad))
  (let ((selected-obj (when (not (cdr new-sel)) (car new-sel))))
    (if (is-a-p selected-obj opal:polyline)
	(gg:menubar-enable-component
	 (gg:find-submenu-component main-menu "  edit  " " reshape "))
	(gg:menubar-disable-component
	 (gg:find-submenu-component main-menu "  edit  " " reshape ")))))

(defun create-mover-grower ()
  (create-instance 'mover-grower garnet-gadgets:multi-graphics-selection
    (:input-filter (o-formula (if (gv grid-obj :gridon)
				  (gv grid-obj :gridamt)
				  nil)))
    (:selection-function 'mover-grower-sel-fn)
    (:start-where (list :element-of-or-none *draw-agg*))
    (:check-line t)
    (:check-polygon t)
    (:check-group t)
    (:check-grow-p t)
    (:multiple-select t)
    (:movegrow-boxes-p t)
    (:movegrow-lines-p t)
    (:value nil)
    (:running-where t))

  )


;; Polygon gadget
;; Used to make polygons when the polygon tool is the one being used.

(defun edit-polyline-start-where (obj inter ev)
  (declare (ignore inter))
  ;; make sure event is in the right window
  (when (eq (g-value obj :window) (inter:event-window ev))
    (let ((obj (kr-send *draw-agg* :point-to-leaf *draw-agg*
			(inter:event-x ev) (inter:event-y ev) :type opal:polyline)))
      (if obj
	  obj
	  t))))

(defun create-polygon-maker ()
  (setf *polygon-maker*
	(create-instance 'polygon-maker garnet-gadgets:polyline-creator
	  (:input-filter (o-formula (if (gv grid-obj :gridon)
					(gv grid-obj :gridamt)
					nil)))
	  (:start-event :rightdown)
	  (:deleter-start-event :double-leftdown)
	  (:start-where `(:in ,*draw-agg*))
	  (:close-enough-value 3)
	  (:active-p (o-formula (gv (nth 5 (gv tools-menu :components))
				    :selected)))
	  (:running-where t)
	  (:selection-function
	   #'(lambda (gadget new-point-list)
	       (declare (ignore gadget))
	       (garnet-gadgets:set-selection mover-grower nil)
	       (let* ((selectable-objs (g-value current-state :selectable-objs))
		      (fill (g-value selectable-objs :filler :filling-style))
		      (frame (g-value selectable-objs :frame))
		      (line-hold (g-value frame :line-hold))
		      (new-obj
		       (create-instance nil opal:polyline
			 (:point-list (copy-list new-point-list))
			 (:line-p nil)
			 (:polygon-p t)
			 (:grow-p t)
			 (:group-p nil)
			 (:filling-style (if fill
					     (create-instance nil fill)))
			 (:line-style (get-line-style
				       (g-value line-palette :selected :line-thick)
				       (g-value line-hold :foreground-color))))))
		 (with-constants-disabled
		   (opal:add-component *draw-agg* new-obj))
		 (garnet-gadgets:set-selection mover-grower new-obj))))))
  ;; this baby takes care of the case where you have a polyline being edited,
  ;; and you move the mouse over another polyline and hit meta-r.  it should
  ;; start editing the second polyline.
  (create-instance 'edit-polyline-inter inter:button-interactor
    (:operates-on polygon-maker)
    (:active (o-formula
	      (or
	       (gv (gg:find-submenu-component main-menu "  edit  " " reshape ") :enabled)
	       (gvl :operates-on :polyline-being-edited))))
    (:start-where (o-formula (list :custom (gvl :operates-on)
				   'edit-polyline-start-where)))
    (:window draw-win)
    (:start-event :meta-\r)
    (:continuous nil)
    ;; when you hit meta-r, the accelerator first calls the reshape
    ;; function if a polyline is selected.  then, this interactor is
    ;; started.  you don't want to call reshape-fn unless it's not
    ;; been called by the accelerator, since if you call reshape-fn
    ;; twice, it will turn off the handles!!
    (:final-function #'(lambda (inter obj)
			 (unless (g-value polygon-maker :reshape-called-p)
			   (reshape-fn inter obj))
			 (s-value polygon-maker :reshape-called-p nil)))))

;; Note, moving-rect, moving-oval, etc. were created in
;; create-main-window text-feedback was created in create-main-window
(defun create-draw-window ()
  ;; this is the window in which the drawings are done.
  (create-instance 'draw-win inter:interactor-window
    (:left 55)
    (:top (+ 2 (g-value main-menu :height)))
    (:width 680)
    (:height (- (g-value main-window :height) (g-value main-menu :height)
		(g-value stipple-palette :height) 7))
    (:border-width 2)
    (:omit-title-bar t)
    (:parent main-window))
  ;; Holds the feedback objects for the different objects are stored.
  (s-value draw-win :aggregate
	   (create-instance 'top-draw-agg opal:aggregate))
  ;; *draw-agg* is used to actually store the drawings.
  (setf *draw-agg* (create-instance nil opal:aggregadget
		     (:left 0) (:top 0)
		     (:width (o-formula (gv draw-win :width)))
		     (:height (o-formula (gv draw-win :height)))))
  (create-mover-grower)
  (create-polygon-maker)
  (opal:add-components top-draw-agg
		       *draw-agg* mover-grower moving-line moving-rect
		       moving-oval moving-roundtangle text-feedback
		       new-moving-arrowline moving-doublearrowline
		       polygon-maker))


;; Called for rect, oval, line, arrow-lines, etc. Not called for text
;; or polygons
(defun create-copy-obj (creator-obj line-p points-list filler-fill
				    frame-color line-thick)
  (let ((obj (with-constants-disabled
		 (opal:copy-gadget creator-obj nil))))
    (if line-p
	(s-value obj :points (copy-list points-list))
	(s-value obj :box (copy-list points-list)))
    (s-value obj :filling-style (if filler-fill
				    (create-instance nil filler-fill)
				    nil))
    (s-value obj :line-style
	     (get-line-style line-thick frame-color))
    obj))

(defun create-interactors ()
  ;; Two point interactor
  ;; This is the interactor used to give information necessary for
  ;; drawing a new object.
  (create-instance 'new-item inter:two-point-interactor
    (:window draw-win)
    (:active (o-formula (and (not (gv (nth 5 (g-value tools-menu :components))
				      :selected))
			     (not (gv (nth 4 (g-value tools-menu :components))
				      :selected)))))
    (:input-filter (o-formula (if (gv grid-obj :gridon)
				  (gv grid-obj :gridamt)
				  nil)))
    (:start-event :rightdown)
    (:start-where t)
    (:final-function
     #'(lambda (an-interactor points-list)
	 (garnet-gadgets:set-selection mover-grower nil)
	 (when points-list
	   (let* ((selectable-objs (g-value current-state :selectable-objs))
		  (creator-obj (g-value an-interactor :creator-obj))
		  (frame-color (g-value selectable-objs :frame :line-hold
					:foreground-color))
		  (filler-fill (g-value selectable-objs :filler
					:filling-style))
		  (line-thick (g-value line-palette :selected :line-thick))
		  (obj (create-copy-obj creator-obj
					(g-value an-interactor :line-p)
					points-list
					filler-fill frame-color line-thick)))
	     (with-constants-disabled
		 (opal:add-component *draw-agg* obj))
	     (garnet-gadgets:set-selection mover-grower obj)
	     obj))))
    (:outside-action :last)
    (:feedback-obj (o-formula (gv tools-menu :selected :feedback-object)))
    (:creator-obj (o-formula (gv tools-menu :selected :creator-object)))
    (:line-p (o-formula (gv tools-menu :selected :feedback-object :line-p)))
    (:min-length 0)
    (:min-height 0)
    (:min-width 0))
  ;; Text Editing
  ;; This is used to decide if a newly selected object is text, if so
  ;; then we must set it up so it is editable.  this is the interactor
  ;; used to do text editing.
  (create-instance 'create-or-edit inter:text-interactor
    (:feedback-obj (o-formula (if (eq :none (gvl :first-obj-over))
				  text-feedback)))
    (:active (o-formula (gv (nth 4 (gv tools-menu :components))
			    :selected)))
    (:start-where `(:element-of-or-none ,*draw-agg*
		    :type ,opal:cursor-multi-text))
    (:input-filter (o-formula (if (gv grid-obj :gridon)
				  (gv grid-obj :gridamt)
				  nil)))
    (:window draw-win)
    (:waiting-priority *garnetdraw-high-priority*)
    (:running-priority *garnetdraw-high-running-priority*)
    (:start-event :any-rightdown)
    (:stop-event '(:any-mousedown :control-\j))
    (:start-action #'(lambda (inter obj ev)
		       ;; make sure the font of the feedback object is correct
		       (let ((feed (g-value inter :feedback-obj)))
			 (when feed
			   (let ((current-font (g-value tools-menu :text-tool
							:text-state :font)))
			     (s-value feed :font
				      (opal:get-standard-font
				       (g-value current-font :family)
				       (g-value current-font :face)
				       (g-value current-font :size))))
			   (s-value feed :string "")))
		       (call-prototype-method inter obj ev)))
    (:final-function
     #'(lambda (an-interactor obj-being-edited stop-event final-string x y)
	 (declare (ignore an-interactor stop-event))
	 (garnet-gadgets:set-selection mover-grower nil)
	 (when (eq :none obj-being-edited)
	   (let* ((current-font (g-value tools-menu :text-tool
					 :text-state :font))
		  (current-color (g-value current-state :selectable-objs
					  :frame :line-hold
					  :foreground-color))
		  (new-str
		   (create-instance nil opal:cursor-multi-text
		     (:box (list x y 0 0))
		     (:string (copy-seq final-string))
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:text-p t)
		     (:font (opal:get-standard-font
			     (g-value current-font :family)
			     (g-value current-font :face)
			     (g-value current-font :size)))
		     (:line-style (get-line-style
				   (g-value line-palette :selected :line-thick)
				   current-color)))))
	     (with-constants-disabled
		 (opal:add-component *draw-agg* new-str))
	     (garnet-gadgets:set-selection mover-grower new-str)
	     )))))
  ;; (create-instance 'delete inter:button-interactor
  ;;   (:continuous nil)
  ;;   (:start-where t)
  ;;   (:start-event #\rubout)
  ;;   (:final-function
  ;;    #'(lambda (an-interactor final-obj-over)
  ;; 	 (declare (ignore an-interactor final-obj-over))
  ;; 	 (setf *temp-list* nil)
  ;; 	 (dolist (item (copy-list (g-value mover-grower :value)))
  ;; 	   (with-constants-disabled
  ;; 	       (opal:remove-component *draw-agg* item))
  ;; 	   (setf *temp-list* (cons item *temp-list*)))
  ;; 	 (garnet-gadgets:set-selection mover-grower nil)
  ;; 	 (dolist (item (copy-list *temp-list*))
  ;; 	   (opal:destroy item))
  ;; 	 (setf *temp-list* nil)))
  ;;   (:window `(,draw-win ,win)))
  )

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (declare (ignore double-buffered-p))
  (create-color-list)
  (create-main-window)
  (create-draw-window)
  (s-value main-menu :accelerator-windows `(,main-window ,draw-win))
  (gg:standard-initialize-gadget main-menu mover-grower *draw-agg*)
  ;; This is necessary because the items functions are called
  ;; by the motif-menu sitting in the submenus
  (dolist (menu (g-value main-menu :menubar-items :components))
    (gg:standard-initialize-gadget (g-value menu :submenu)
				   mover-grower *draw-agg*))
  ;; creates *q-box*
  (create-query-gadget)
  ;; creates save-win, *save-db*
  (create-file-db)
  ;; creates grid-win, *grid-db*
  (create-grid-db)
  ;; creates ps-read-db, *read-db*
  (create-read-db)
  (create-interactors)
  (opal:update main-window t)
  (format t "~%garnet draw v1.0:

to draw an object:
^^^^^^^^^^^^^^^^^
  1. select the type of object to be created from the tools-menu
     on the left side of the screen.

  2. press and drag the mouse button to the desired size for the
     object.  if text then just depress mouse button and release,
     to end editing type ctrl-j or click with the mouse elsewhere.
     for polygon, depress right mouse button, release and click
     again for next point, etc. till either you have added enough
     points or you don't wish to add any more points.  if you don't
     want to add any more points then depress any other button to
     stop.


to select objects:
^^^^^^^^^^^^^^^^^
  1. use the left button to select a single item or the middle mouse
     button to select multiple objects.

  2. depress the middle mouse button and select the region within
     which you want all objects to be selected.


to change line or filling color of a(n) object(s):
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  1. select the object(s) to be changed, choose either fill or line
     and then select the appropriate palette pattern or color.

  2. to change the default line and filling colors, make sure no
     objects are selected.  then select line/fill and select the
     palette item of your choice.


to change size, family, or face of text objects:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  1. select text object and use font menu to change item.

  2. to change the default font, deselect all items, then choose
     appropriate menu items from font menu.  (note:  you can
     see what the default font is in the text tools slot of the
     tools menu.)

to edit a polygon(s):
^^^^^^^^^^^^^^^^^^^^
  1. select polygon to be edited.

  2. type meta-r or select 'reshape' from the 'edit' menu.

  3. to move a point, use the left mouse button to drag the handles.
     control-g while moving will return the point to it's previous
     location.  typing delete while moving will delete the point.

     to add a point, click left mouse button on the line where you
     want to add the point.  you can start dragging the point without
     letting go of the button.

     to delete a point, double click with the left button on the point
     to be deleted.

  4. to edit a different polygon from the one you are editing now,
     move the mouse over the polygon you want to edit and hit meta-r.

~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

(defun do-stop ()
  (opal:destroy main-window)
  ;; these interactors were probably not active when win was destroyed, so
  ;; we must destroy them explicitly.
  (dolist (sym '(new-item create-or-edit edit-polyline-inter))
    (if (and (boundp sym) (schema-p (eval sym)))
	(opal:destroy (eval sym))))
  ;; these prototype objects were not added to a window themselves, so we
  ;; must destroy them explicitly.
  (dolist (sym '(palette-item color-palette-item moving-agg
		 creator-line creator-arrowline creator-doublearrowline
		 creator-rect creator-roundtangle creator-oval))
    (if (and (boundp sym) (schema-p (eval sym)))
	(opal:destroy (eval sym))))
  ;;for demo-controller
  (unless (and (fboundp 'common-lisp-user::garnet-note-quitted)
	       (common-lisp-user::garnet-note-quitted "garnetdraw"))))


;; things to fix:
;; * align-to-grid command
