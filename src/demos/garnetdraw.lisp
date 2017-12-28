;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNETDRAW; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


;;;
;;;   GARNET DRAW
;;;
;;;  Implemented by Vivek Gupta


(in-package :GARNETDRAW)

(defparameter GarnetDraw-Version "2.0")

;; Load necessary files
;;
(defvar GARNETDRAW-INIT
  (progn
    ;;;  Load ps-loader.
    ;; (common-lisp-user::garnet-load "ps:ps-loader")

    ;;;  Load gadgets.
    ;; (dolist (file '("multi-selection-loader" "polyline-creator-loader"
    ;; 		    "arrow-line-loader"
    ;; 		    "motif-menubar-loader"
    ;; 		    "motif-trill-device-loader"
    ;; 		    "motif-error-gadget-loader"
    ;; 		    "motif-save-gadget-loader"
    ;; 		    "standard-edit-loader"))
    ;;   (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file)))
    ))

#|
====================================================================
VARIABLES

These are the variables used in different
parts of the file
====================================================================
|#

(declaim (special CREATE-OR-EDIT CURRENT-STATE DRAW-WIN MOVER-GROWER
		  MOVING-AGG MOVING-ARROWLINE NEW-MOVING-ARROWLINE
		  MOVING-DOUBLEARROWLINE
		  MOVING-LINE MOVING-OVAL MOVING-RECT MOVING-ROUNDTANGLE
		  PALETTE-FEEDBACK TEXT-FEEDBACK
		  TOOLS-MENU TOP-DRAW-AGG WIN PS-READ-WIN GRID-WIN SAVE-WIN
		  GRID-OBJ MAIN-MENU COMMON-LISP-USER::*GARNET-OBJECT-JUST-CREATED*
		  EDIT-POLYLINE-INTER CREATOR-DOUBLEARROLINE TOOL-FEEDBACK
		  LINE-FEEDBACK LINE-PALETTE PALETTE-ITEM STIPPLE-PALETTE
		  COLOR-PALETTE-ITEM CREATOR-LINE CREATOR-RECT
		  CREATOR-ROUNDTANGLE CREATOR-OVAL CREATOR-ARROWLINE
		  CREATOR-DOUBLEARROWLINE TOP-AGG COLOR-PALETTE))

(defvar *DRAW-AGG*)(defvar *Q-BOX*)
(defvar *SAVE-DB*)(defvar *Read-DB*)
(defvar *GRID-DB*)

(defparameter the-color-list NIL)

(create-instance 'GRID-OBJ NIL ; use an object so constraints to values
		 (:gridvis NIL) ; whether can see gridding or not
		 (:gridon NIL) ; whether gridding is in use or not
		 (:gridamt 10)) ; amount to grid by
(defparameter *Grid-Menu-Item* NIL) ; set with menu bar item for grid on/off
(defparameter *Grid-Vis-Item* NIL) ; set with menu bar item for grid vis on/off
(defparameter POLYGON-MAKER NIL)

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

(defvar *TEMP-POINTS* NIL)
(defvar *TEMP-LIST* NIL)
(defvar *CLIPBOARD* NIL)
(defvar *DOCUMENT-NAME* "Untitled")
(defvar *STORED-LINE-STYLES*
  (list opal:default-line-style opal:line-1 opal:line-2
	opal:line-4 opal:line-8 opal:red-line opal:green-line opal:blue-line
	opal:yellow-line opal:purple-line opal:cyan-line opal:orange-line))

(defvar *FEEDBACK-LINE-STYLE*
  (create-instance NIL opal:line-style
    (:constant T)
    (:line-thickness 3)))

(defvar *garnetdraw-high-priority*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))
(defvar *garnetdraw-high-running-priority*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))

;; need a priority level higher than the motif-tab priority level so
;; text editing will take precedence over accelerators
(pushnew *garnetdraw-high-priority* inter:priority-level-list)
(pushnew *garnetdraw-high-running-priority* inter:priority-level-list)


#|
====================================================================
PROTOTYPES

Below we have the prototypes for all the objects which we are going
to draw.  The first is the one for grouping objects, the rest are
individual objects.
====================================================================
|#


(defun Create-Moving-Prototypes ()
  (create-instance 'moving-agg opal:aggregadget
    (:group-p t)
    (:grow-p t))

  (create-instance 'MOVING-LINE opal:line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t)
    (:group-p NIL)
    (:line-p t)
    (:draw-function :xor)
    (:fast-redraw-p t)
    (:visible-p NIL)
    (:line-style opal:dashed-line))
  (create-instance 'creator-LINE opal:line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t))

  (create-instance 'new-moving-arrowline garnet-gadgets:Arrow-Line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:line-p t)
    (:grow-p t)
    (:group-p NIL)
    (:visible-p NIL)
    (:filling-style NIL)
    (:line-style opal:dashed-line)
    (:open-p NIL)
    (:parts `((:line :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead :modify (:fast-redraw-p t) (:draw-function :xor)))))
  (create-instance 'creator-arrowline garnet-gadgets:Arrow-Line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:grow-p t)
    (:open-p NIL))

  (create-instance 'moving-doublearrowline garnet-gadgets:Double-Arrow-Line
    (:points (list 0 0 0 0))
    (:x1 (o-formula (first (gvl :points))))
    (:y1 (o-formula (second (gvl :points))))
    (:x2 (o-formula (third (gvl :points))))
    (:y2 (o-formula (fourth (gvl :points))))
    (:arrow-p t)
    (:arrowhead-p 3)
    (:grow-p t)
    (:group-p NIL)
    (:line-p t)
    (:visible-p NIL)
    (:filling-style NIL)
    (:line-style opal:dashed-line)
    (:open-p NIL)
    (:parts `((:line :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead1 :modify (:fast-redraw-p t) (:draw-function :xor))
	      (:arrowhead2 :modify (:fast-redraw-p t) (:draw-function :xor)))))
  (create-instance 'creator-doublearrowline garnet-gadgets:Double-Arrow-Line
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

  (create-instance 'MOVING-RECT opal:rectangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top  (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:group-p NIL)
    (:grow-p t)
    (:filling-style NIL)
    (:line-p NIL)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:visible-p NIL)
    (:line-style opal:dashed-line))
  (create-instance 'creator-RECT opal:rectangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top  (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:grow-p t))

  (create-instance 'MOVING-ROUNDTANGLE opal:roundtangle
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:filling-style NIL)
    (:grow-p t)
    (:group-p NIL)
    (:visible-p NIL)
    (:line-p NIL)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:line-style opal:dashed-line))
  (create-instance 'creator-ROUNDTANGLE opal:roundtangle
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
    (:filling-style NIL)
    (:grow-p t)
    (:group-p NIL)
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:visible-p NIL)
    (:line-p NIL)
    (:line-style opal:dashed-line))
  (create-instance 'creator-oval opal:oval
    (:box (list 0 0 0 0))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:grow-p t))
  )

(defun Create-Text-Feedback ()
  (create-instance 'TEXT-FEEDBACK opal:cursor-multi-text
    (:box (list 0 0 0 0))
    (:string "")
    (:visible (o-formula (gvl :obj-over)))
    (:group-p NIL)
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))))



#|
====================================================================
DIALOG BOX FUNCTIONS

====================================================================
|#

;; Since the same save gadget is used for BOTH creating ps files and
;; for saving, this function resets the :selection-function and the
;; :text of the save gadget after it is done.

(defun PS-Sel-Fn (g v)
  (opal:make-ps-file DRAW-WIN v)
  (s-value g :selection-function 'Save-Sel-Fn)
  (s-value (g-value g :text) :string "Saving...")
  )

(defun Read-Sel-Fn (g v)
  (declare (ignore g))
  (let ((filename v))
    (if (probe-file filename)
	(progn
	  (gg:set-selection MOVER-GROWER NIL)
	  (if (schema-p *DRAW-AGG*) (opal:destroy *DRAW-AGG*))
	  (setf *document-name* filename)
	  (with-constants-disabled
	   (load *document-name*))

	  (unless (schema-p *DRAW-AGG*)
	    ;; then is a new style file, have to set *draw-agg*
	    (setf *draw-agg* common-lisp-user::*Garnet-Object-Just-Created*))
	  (s-value MOVER-GROWER :start-where (list :element-of-or-none
						   *DRAW-AGG*))
	  (s-value CREATE-OR-EDIT :start-where
		   (list :element-of-or-none *DRAW-AGG*
			 :type opal:cursor-multi-text))
	  (s-value POLYGON-MAKER :start-where
		   (list :in *DRAW-AGG*))

	  (gg:Standard-Initialize-Gadget MAIN-MENU MOVER-GROWER *DRAW-AGG*)

	  ;; This is necessary because the items functions are called
	  ;; by the motif-menu sitting in the submenus
	  (dolist (menu (g-value MAIN-MENU :menubar-items :components))
	    (gg:Standard-Initialize-Gadget (g-value menu :submenu)
					   MOVER-GROWER *DRAW-AGG*))

	  (with-constants-disabled
	    (opal:add-component TOP-DRAW-AGG *DRAW-AGG* :where :back))
	  (s-value WIN :title (file-namestring *document-name*)))
	(gg:display-query *Q-BOX* "There is no file by that name"
			  '("OK"))))
  )

(defun Read-File ()
  (gg:display-load-gadget-and-wait *Read-DB* *document-name*)
  )

(defun Write-Draw-Agg ()
  (with-open-file (*standard-output* *document-name*
		   :direction :output :if-exists :supersede)
    (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (format T ";;; This file created by GARNETDRAW ~a~%"
	  GarnetDraw-Version)
  (format T ";;; on ~a~%" (inter::time-to-string))
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
  (format T "(in-package :COMMON-LISP-USER)~%(use-package :KR)~%~%")
  (format T "(defparameter common-lisp-user::*Used-GarnetDraw-Version* ~s)~%~%"
	  GarnetDraw-Version)
  (s-value *draw-agg* :package-name "COMMON-LISP-USER")
  (s-value *draw-agg* :window-height (g-value draw-win :height))
  (s-value *draw-agg* :window-width (g-value draw-win :width))
  (s-value *draw-agg* :window-title "Garnet Draw")
  (Format T "(defparameter common-lisp-user::*Garnet-Object-Just-Created* ~%")
  (opal:write-gadget *DRAW-AGG* T T)
  (format t ")~%")))

;; This is called by savefun and saveasfun.  If confirm-p is T, it
;; displays the save gadget.  Else, it simply saves.

(defun Save-File (&key confirm-p)
  (if (or confirm-p (equal *document-name* "Untitled"))
      (gg:display-save-gadget-and-wait *save-db* *document-name*)
      (Write-Draw-Agg)))

;; This is the selection function of the save gadget.  It sets the
;; document name to be the filename the gadget returns, v, and
;; calls Write-Draw-Agg, which does the saving.

(defun Save-Sel-Fn (g v)
  (declare (ignore g))
  (setq *document-name* v)
  (Write-Draw-Agg))

#|
====================================================================
DIALOG BOXES

====================================================================
|#

(defun Create-Query-Gadget ()
  (setf *Q-BOX* (create-instance NIL garnet-gadgets:motif-query-gadget
		  (:foreground-color opal:motif-green)
		  (:parent-window WIN))))

(defun Create-File-DB ()
  (setf *SAVE-DB*
	(create-instance NIL gg:motif-save-gadget
	  (:foreground-color opal:motif-green)
	  (:min-gadget-width 285)
	  (:parent-window win)
	  (:modal-p T)
	  (:top 40)
	  (:button-panel-items '("OK" "Cancel"))

	  (:selection-function 'Save-Sel-Fn)
	  (:parts `(:dir-input
		    :file-menu
		    :file-input
		    :message
		    :OK-Cancel-buttons
		    (:text ,opal:text
		     (:constant T  :except :string)
		     (:left ,(o-formula (gvl :parent :left)))
		     (:top 10)
		     (:font ,(opal:get-standard-font NIL :bold-italic :large))
		     (:string "Saving..."))
		    ))))
  (setf SAVE-WIN (g-value *save-db* :window))
)

(defun Create-Grid-DB ()
  (create-instance 'GRID-WIN inter:interactor-window
    (:background-color opal:motif-green)
    (:parent WIN)
    (:modal-p T)
    (:visible NIL)
    (:left (o-formula (- (floor (gv WIN :width) 2)
			 (floor (gvl :width) 2))))
    (:top (o-formula (- (floor (gv WIN :height) 2)
			(floor (gvl :height) 2))))
    (:width 345)
    (:height 85))
  (setf *GRID-DB*
	(create-instance NIL opal:aggregadget
	  (:left 0) (:top 0)
	  (:parts
	   `((:text ,opal:text
	      (:constant T)
	      (:left 10) (:top 10)
	      (:font ,(opal:get-standard-font NIL :bold-italic :large))
	      (:string "New Grid Increment:"))
	     (:value ,gg:motif-trill-device
	      (:foreground-color ,opal:motif-green)
	      (:constant T)
	      (:left 225) (:top 5)
	      (:height 30) (:width 100))
	     (:ok-cancel ,gg:motif-text-button-panel
	      (:foreground-color ,opal:motif-green)
	      (:constant T)
	      (:text-offset 5)
	      (:left 114) (:top 40)
	      (:items ("OK" "Cancel"))
	      (:final-feedback-p NIL)
	      (:gray-width 3) (:shadow-offset 5)
	      (:direction :horizontal)
	      (:selection-function
	       ,#'(lambda (g v)
		    (s-value GRID-WIN :visible NIL)
		    (opal:update GRID-WIN)
		    (when (equal v "OK")
		      (s-value GRID-OBJ :gridamt
			       (g-value g :parent :value :value))))))))))
  (s-value GRID-WIN :aggregate *GRID-DB*)
  )

(defun Create-Read-DB ()
  (setf *Read-DB*
	(create-instance NIL gg:motif-load-gadget
	  (:foreground-color opal:motif-green)
	  (:selection-function #'Read-Sel-Fn)
	  (:min-gadget-width 285)
	  (:modal-p T)
	  (:check-filenames-p NIL)
	  (:parent-window win)
	  (:top 40)
	  (:button-panel-items '("OK" "Cancel"))
	  (:parts
	   `(:dir-input
	     :file-menu
	     :file-input
	     :message
	     (:text ,opal:text
	      (:constant T :except :string)
	      (:left 10) (:top 10)
	      (:font ,(opal:get-standard-font NIL :bold-italic :large))
	      (:string "Reading..."))

	     (:OK-cancel-buttons :modify
		     (:top ,(o-formula (+ (gvl :parent :file-input :top)
					  (gvl :parent :file-input :height)
					  20))))))))

  (setf PS-Read-WIN (g-value *Read-DB* :window))
  )


#|
====================================================================
MENU FUNCTIONS AND MENUBAR

These functions are the necessary functions for the Menubar to act
properly on any action by the user.  Additional functions must be
added here for cut, paste, copy, and various font functions.
====================================================================
|#


(defun quitfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:Save-File-If-Wanted *save-db* *document-name*))
    (do-stop)))

(defun psfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (s-value *Save-DB* :selection-function #'PS-Sel-Fn)
  (s-value (g-value *Save-DB* :text) :string "PS File...")
  (gg:display-save-gadget-and-wait *Save-DB* *document-name*)
  )

(defun openfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:save-file-if-wanted *save-db* *document-name*))
    (Read-File)))

(defun newfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (unless (eq :cancel (gg:Save-File-If-Wanted *save-db* *document-name*))
	(gg:set-selection MOVER-GROWER NIL)
	(opal:remove-component TOP-DRAW-AGG *DRAW-AGG*)
	(with-constants-disabled
	    (dolist (comp (copy-list (g-value *DRAW-AGG* :components)))
	      (opal:destroy comp)))
	(opal:add-component TOP-DRAW-AGG *DRAW-AGG*)))

(defun saveasfun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (Save-File :confirm-p T))

(defun savefun (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (Save-File))


(defun GetFontFromMenu (submenu-item old-font)
  (cond
    ((equal submenu-item " Fixed ")
     (opal:get-standard-font :fixed
			     (g-value old-font :face)
			     (g-value old-font :size)))
    ((equal submenu-item " Serif ")
     (opal:get-standard-font :serif
			     (g-value old-font :face)
			     (g-value old-font :size)))
    ((equal submenu-item " Sans-Serif ")
     (opal:get-standard-font :sans-serif
			     (g-value old-font :face)
			     (g-value old-font :size)))

    ((equal submenu-item " Roman ")
     (opal:get-standard-font (g-value old-font :family)
			     :roman
			     (g-value old-font :size)))
    ((equal submenu-item " Bold ")
     (opal:get-standard-font (g-value old-font :family)
			     :bold
			     (g-value old-font :size)))
    ((equal submenu-item " Italic ")
     (opal:get-standard-font (g-value old-font :family)
			     :italic
			     (g-value old-font :size)))
    ((equal submenu-item " Bold-Italic ")
     (opal:get-standard-font (g-value old-font :family)
			     :bold-italic
			     (g-value old-font :size)))
    ((equal submenu-item " Small ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :small))
    ((equal submenu-item " Medium ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :medium))
    ((equal submenu-item " Large ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :large))
    ((equal submenu-item " Very-Large ")
     (opal:get-standard-font (g-value old-font :family)
			     (g-value old-font :face)
			     :very-large))
    (t old-font)))

(defun fontfun (gadget menu-item submenu-item)
  (declare (ignore gadget menu-item))
  (if (g-value MOVER-GROWER :value)
      (dolist (item (g-value MOVER-GROWER :value))
	(when (g-value item :text-p)
	  (with-constants-disabled
		(s-value item :font
		       (GetFontFromMenu submenu-item (g-value item :font)))))))
  ;; Always set the global state
  (let* ((text-state (g-value TOOLS-MENU :text-tool :text-state))
	 (new-font (GetFontFromMenu submenu-item
				    (g-value text-state :font))))
    (s-value text-state :font new-font)))

(defun gridtoggle (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (if (g-value GRID-OBJ :gridon)
      (progn ; turn if off, and make menu so it will turn it on
	(s-value GRID-OBJ :gridon NIL)
	(gg:menubar-set-title *Grid-Menu-Item* " Turn Grid On "))
      (progn; turn if on, and make menu so it will turn it off
	(s-value GRID-OBJ :gridon T)
	(gg:menubar-set-title *Grid-Menu-Item* " Turn Grid Off "))))

(defun gridvisible (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (if (g-value GRID-OBJ :gridvis)
      (progn ; turn if off, and make menu so it will turn it on
	(s-value GRID-OBJ :gridvis NIL)
	(gg:menubar-set-title *Grid-Vis-Item* " Show Grid Dots "))
      (progn; turn if on, and make menu so it will turn it off
	(s-value GRID-OBJ :gridvis T)
	(gg:menubar-set-title *Grid-Vis-Item* " Hide Grid Dots "))))

(defun setgrid (menubar bar-item submenu-item)
  (declare (ignore menubar bar-item submenu-item))
  (s-value (g-value *GRID-DB* :value) :value (g-value GRID-OBJ :gridamt))
  (s-value GRID-WIN :visible T)
  (opal:raise-window GRID-WIN)
  (opal:update GRID-WIN))

;;; This function hides the mover-grower handles.
;;; This is called by BOTH the accelerator and the edit-polyline-inter.
;;; Basically, if an object is selected and that object is a polyline,
;;; then edit it.  Else, if the obj passed to the function is a polyline,
;;; edit that instead.
(defun reshape-fn (inter obj &optional extra)
  (declare (ignore inter extra))
  (s-value POLYGON-MAKER :reshape-called-p T)
  (let* ((selected-objs (g-value MOVER-GROWER :value))
	 (selected-obj (when (not (cdr selected-objs)) (first selected-objs))))
    (if (is-a-p selected-obj opal:polyline)
	(progn
	  (gg::toggle-polyline-handles POLYGON-MAKER selected-obj)
	  (gg:set-selection MOVER-GROWER NIL))
	(if (is-a-p obj opal:polyline)
	    (progn
	      (gg:set-selection MOVER-GROWER NIL)
	      (gg::toggle-polyline-handles POLYGON-MAKER obj))
	    (inter:beep)))))

(defun Create-Main-Menubar ()
  (create-instance 'MAIN-MENU garnet-gadgets:MOTIF-MENUBAR
    (:foreground-color opal:motif-green)
    (:min-menubar-width (o-formula (gv win :width)))
    (:left 0) (:top 0)
    (:left-offset 60)
    (:title-font (opal:get-standard-font :sans-serif :bold :large))
    (:item-font (opal:get-standard-font :sans-serif :bold :medium))
    (:accel-font (opal:get-standard-font :sans-serif :bold :medium))
    (:items
     '(("  File  " NIL
	((" Open " openfun)
	 (" New " newfun)
	 (" Save " savefun)
	 (" Save As " saveasfun)
	 (" Create PS " psfun)
	 (" Quit " quitfun)))
       ("  Edit  " NIL
	((" Cut " gg:standard-cut) (" Copy " gg:standard-copy)
	 (" Paste " gg:standard-paste-same-place)
	 (" Clear " gg:standard-delete)
	 (" Duplicate " gg:standard-duplicate)
	 (" Clear All " gg:standard-delete-all)
	 (" To Top " gg:standard-to-top)
	 (" To Bottom " gg:standard-to-bottom) (" Group " gg:standard-group)
	 (" Ungroup " gg:standard-ungroup) (" Reshape " reshape-fn)
	 (" Refresh " gg:standard-refresh)))
       ("  Font  " fontfun
	((" Fixed ")(" Serif ")(" Sans-Serif ")(" Roman ")(" Bold ")
	 (" Italic ")(" Bold-Italic ")(" Small ")(" Medium ")
	 (" Large ")(" Very-Large ")))
       ("  Options  " NIL
	((" Turn Grid On " gridtoggle) (" Set Grid... " setgrid)
	 (" Show Grid Dots " gridvisible)
	 ))
       ))
    (:accelerators
     '((("M-o" :|META-o|) ("M-n" :|META-n|) ("M-s" :|META-s|) NIL
	("M-p" :|META-p|) ("M-q" :|META-q|))
       (("M-x" :|META-x|) ("M-c" :|META-c|) ("M-v" :|META-v|) ("DEL" #\rubout)
	("M-d" :|META-d|) NIL ("M-f" :|META-f|) ("M-j" :|META-j|)
	("M-g" :|META-g|) ("M-h" :|META-h|) ("M-r" :|META-r|) ("M-l" :|META-l|))
       (NIL NIL NIL ("M-R" :|META-R|) ("M-b" :|META-b|) ("M-i" :|META-i|)
	("M-t" :|META-t|) ("M-1" :|META-1|) ("M-2" :|META-2|)
	("M-3" :|META-3|) ("M-4" :|META-4|))
       (NIL NIL NIL)))
    (:bar-above-these-items
     '(NIL
       (" To Top ")
       (" Roman " " Small ")
       NIL))
    )
  )


#|
====================================================================
TOOL PALETTE

This is the list of tools available for drawing objects.  Each
contains a bitmap representation of the tool and the location
where it goes.
====================================================================
|#

(defun Create-Tool-Palette ()

  (create-instance 'TOOL-FEEDBACK opal:rectangle
    (:left (o-formula (gvl :obj-over :left)))
    (:top (o-formula (gvl :obj-over :top)))
    (:width (o-formula (gvl :obj-over :width)))
    (:height (o-formula (gvl :obj-over :height)))
    (:visible (o-formula (gvl :obj-over)))
    (:line-style opal:line-4))

  (create-instance 'TOOLS-MENU opal:aggregadget
    (:top (+ 2 (g-value MAIN-MENU :height)))
    (:selected NIL)
    (:parts
     `((:line-tool ,opal:aggregadget
	(:left 5) (:height 32) (:width 32)
	(:top ,(o-formula (gvl :parent :top)))
	(:feedback-object ,MOVING-LINE)
	(:creator-object ,creator-LINE)
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
	(:feedback-object ,MOVING-RECT)
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
	(:feedback-object ,MOVING-ROUNDTANGLE)
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
	(:feedback-object ,TEXT-FEEDBACK)
	(:creator-object ,TEXT-FEEDBACK)
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
		       (:string "T")
		       (:line-style ,(create-instance NIL opal:default-line-style))
		       (:font ,(opal:get-standard-font NIL NIL NIL))))))
       (:polygon-tool ,opal:aggregadget
	(:feedback-object ,MOVING-RECT)
	(:creator-object NIL)
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
	(:feedback-object ,MOVING-DOUBLEARROWLINE)
	(:creator-object ,creator-DOUBLEARROWLINE)
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
	(:final-feedback-obj ,TOOL-FEEDBACK)
	(:how-set :set)
	(:start-where ,(o-formula (list :element-of (gvl :operates-on))))))))

  (s-value TOOLS-MENU :selected (car (g-value TOOLS-MENU :components)))
  (s-value TOOL-FEEDBACK :obj-over (car (g-value TOOLS-MENU :components))))


#|
====================================================================
Get-Line-Style:

This function first goes through the *STORED-LINE-STYLES* list and
checks to see if the required line-style is already there.  If it is,
then it returns that line-style.  Otherwise, it creates a new line
style with the appropriate thickness and foreground color, pushes the
new style into the *STORED-LINE-STYLES* list, and returns it.
====================================================================
|#

(defun Get-Line-Style (thickness fg-color)
  (let ((new-line-style NIL))

    ;; First, go through list and see if the needed
    ;; line style already exists.
    (dolist (style *STORED-LINE-STYLES*)
      (when (AND
	     (eql (g-value style :line-thickness) thickness)
	     (eq (g-value style :foreground-color) fg-color))
	(setf new-line-style style)
	(return)))

    ;; If line style doesn't already exist, create it and push
    ;; it onto list.
    (unless new-line-style
      (setf new-line-style
	    (create-instance NIL opal:line-style
	      (:line-thickness thickness)
	      (:foreground-color fg-color)))
      (push new-line-style *STORED-LINE-STYLES*))

    ;; Return the new line style, which was either in the list
    ;; or was created.
    new-line-style))




#|
====================================================================
LINE PALETTE

This is the line palette and the function which sets the value of
the line style slot of all selected object, if a new value is
selected in the line palette.
====================================================================
|#

(defun Create-Line-Palette ()

  (create-instance 'LINE-FEEDBACK opal:rectangle
    (:obj-over NIL)
    (:left 2)
    (:top (o-formula (gvl :obj-over :top)))
    (:width 52)
    (:height (o-formula (gvl :obj-over :height)))
    (:filling-style NIL)
    (:visible (o-formula (gvl :obj-over)))
    (:line-style opal:line-4))

  (create-instance 'LINE-PALETTE opal:aggregadget
    (:left 2) (:top 321) (:width 52) (:height 106)
    (:selected NIL)
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
	      (when (g-value MOVER-GROWER :value)
		(dolist (thing (copy-list (g-value MOVER-GROWER :value)))
		  (if (not (g-value thing :group-p))

		      ;; create a new line-style
		      (let ((new-line-style
			     (Get-Line-Style
			      (g-value line-palette :selected :line-thick)
			      (g-value thing :line-style :foreground-color))))

			;; set line style to be new line style
			(s-value thing :line-style new-line-style)))))))

	(:start-where ,(o-formula (list :element-of (gvl :operates-on)
					:type opal:line)))))))

  (let ((line0 (g-value line-palette :line0)))
    (s-value line-palette :selected line0)
    (s-value line-feedback :obj-over line0))
  )


#|
====================================================================
SELECTED FUNCTION

This is used to change the line style or filling color of a selected
object using the current state aggregadget.
====================================================================
|#

(defun Selectedfun (an-interactor final-obj-over)
  (declare (ignore an-interactor final-obj-over))
  (when (g-value MOVER-GROWER :value)
    (if (g-value CURRENT-STATE :selectable-objs :frame :selected)
	(dolist (thing (g-value MOVER-GROWER :value))
	  (unless (g-value thing :group-p)
	    (let ((new-line-style
			     (Get-Line-Style
			      (g-value thing :line-style :line-thickness)
			      (g-value PALETTE-FEEDBACK :obj-over :line-hold
				       :foreground-color))))

	      ;; set line style to be new line style
	      (s-value thing :line-style new-line-style))))

	(dolist (thing (g-value MOVER-GROWER :value))
	  (unless (g-value thing :group-p)
	    (s-value thing :filling-style
		     (g-value PALETTE-FEEDBACK :obj-over :filling-style))))
	))

  (let* ((selectable-objs (g-value CURRENT-STATE :selectable-objs))
	 (frame (g-value selectable-objs :frame))
	 (filler (g-value selectable-objs :filler))
	 (obj-over (g-value PALETTE-FEEDBACK :obj-over)))
    (if (g-value frame :selected)
	(progn
	  (s-value frame :line-hold (g-value obj-over :line-hold))
	  (s-value frame :filling-style (g-value obj-over :filling-style)))
	(s-value filler :filling-style (g-value obj-over :filling-style))))
  )

#|
====================================================================
PATTERN AND COLOR PALETTES

The first one is for the pattern palette, this contains in the items
slot the necessary numbers for creating the different halftone
values.  The second one uses the values in the color list generated
earlier.
====================================================================
|#

(defun Create-Palette-Feedback ()
  (create-instance 'PALETTE-FEEDBACK opal:aggregadget
    (:obj-over NIL)
    (:left (o-formula (gvl :obj-over :left)))
    (:top (o-formula (gvl :obj-over :top)))
    (:width (o-formula (gvl :obj-over :width)))
    (:height (o-formula (gvl :obj-over :height)))
    (:visible (o-formula (gvl :obj-over)))
    (:fast-redraw-filling-style NIL)
    (:parts
     `((:black-rect ,opal:rectangle
	(:left ,(o-formula (+ 1 (gvl :parent :left))))
	(:top ,(o-formula (+ 1 (gvl :parent :top))))
	(:width ,(o-formula (- (gvl :parent :width) 2)))
	(:height ,(o-formula (- (gvl :parent :height) 2)))
	(:line-style ,*FEEDBACK-LINE-STYLE*)
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

(defun Create-Stipple-Palette ()
  (create-instance 'PALETTE-ITEM opal:rectangle
    (:left (o-formula (gvl :parent :left)))
    (:top (o-formula (gvl :parent :top)))
    (:width 32)
    (:line-hold
     (create-instance NIL opal:line-style
       (:foreground-color opal:black)
       (:line-thickness 0)))
    (:height 32)
    (:filling-style (o-formula
		     (let ((item (nth (gvl :rank) (gvl :parent :items))))
		       (if (schema-p item)
			   item
			   (opal:halftone (nth (gvl :rank)
					       (gvl :parent :items))))))))

  (create-instance 'STIPPLE-PALETTE opal:aggregadget
    (:left 90)
    (:top (o-formula (if (g-value opal:color :color-p) 442 477)))
    (:selected NIL)
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
	(:selected NIL)
	(:items ,(o-formula (gvl :parent :items)))
	(:h-spacing 2)
	(:direction :horizontal)
	(:item-prototype ,palette-item))
       (:NIL-text ,opal:text
	(:left 674)
	(:top ,(o-formula (if (g-value opal:color :color-p) 450 485)))
	(:string "NIL"))))
    (:interactors
     `((:palette-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:how-set :set)
	(:final-feedback-obj ,PALETTE-FEEDBACK)
	(:final-function ,#'Selectedfun)
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :patterns-agg))))
	(:start-action
	 ,#'(lambda (inter obj)
	      (let ((fill (g-value PALETTE-FEEDBACK :obj-over :filling-style)))
		(s-value PALETTE-FEEDBACK :fast-redraw-filling-style fill)
		(s-value (g-value CURRENT-STATE :feedback)
			 :fast-redraw-filling-style (or fill opal:white-fill)))
	      (call-prototype-method inter obj)))))))

  (s-value (car (g-value stipple-palette :patterns-agg :components))
	   :line-hold
	   (create-instance NIL opal:line-style
	     (:foreground-color opal:white)
	     (:line-thickness 0)))

  ;; This is the NIL filling style.
  (let* ((kr::*constants-disabled* T)
	 (patterns-agg (g-value STIPPLE-PALETTE :patterns-agg))
	 (line-hold (g-value (car (g-value patterns-agg :components))
			     :line-hold)))
    (opal:add-component patterns-agg
         (create-instance 'NIL-PALETTE-ITEM opal:rectangle
	   (:left 638) (:width 32) (:height 32)
	   (:top (if (g-value opal:color :color-p) 442 477))
	   (:line-hold line-hold)
	   (:filling-style NIL))))
  )

(defun Create-Color-Palette ()
  (create-instance 'COLOR-PALETTE-ITEM opal:rectangle
    (:left (o-formula (gvl :parent :left)))
    (:top (o-formula (gvl :parent :top)))
    (:width 32)(:height 32)
    (:line-hold (o-formula
		 (car (cdr (nth (gvl :rank) (gvl :parent :items))))))
    (:filling-style (o-formula (car (nth (gvl :rank) (gvl :parent :items))))))


  (create-instance 'COLOR-PALETTE opal:aggregadget
    (:left 90) (:top 477)
    (:selected NIL)
    (:items (copy-list the-color-list))
    (:parts
     `((:colors-agg ,opal:aggrelist
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (gvl :parent :top)))
	(:selected NIL)
	(:items ,(o-formula (gvl :parent :items)))
	(:h-spacing 2)
	(:direction :horizontal)
	(:item-prototype ,color-palette-item))))
    (:interactors
     `((:palette-interactor ,inter:button-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:start-event :any-mousedown)
	(:how-set :set)
	(:final-feedback-obj ,PALETTE-FEEDBACK)
	(:final-function ,#'Selectedfun)
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :colors-agg))))
	(:start-action
	 ,#'(lambda (inter obj)
	      (let ((fill (g-value PALETTE-FEEDBACK :obj-over :filling-style)))
		(s-value PALETTE-FEEDBACK :fast-redraw-filling-style fill)
		(s-value (g-value CURRENT-STATE :feedback)
			 :fast-redraw-filling-style (or fill opal:white-fill)))
	      (call-prototype-method inter obj)))))))
  )

(defun Create-Fill-Palettes ()
  (Create-Palette-Feedback)       ; Creates PALETTE-FEEDBACK
  (Create-Stipple-Palette)        ; Creates STIPPLE-PALETTE
  (if (g-value opal:color :color-p)
      (Create-Color-Palette))     ; Creates COLOR-PALETTE

  ;; Set the initial selected values
  (s-value (g-value stipple-palette :patterns-agg) :selected
	   (nth 16 (g-value (g-value stipple-palette :patterns-agg) :components)))
  (s-value palette-feedback :obj-over
	   (nth 16 (g-value (g-value stipple-palette :patterns-agg) :components)))
  )

#|
====================================================================
 the-color-list is used to store the list of tuples, made up of
 line-styles and filling-styles.  These are created from the
 list of defined rgbvalues.  This is used to create the color
 palette on screens which can display color.  The function
 Create-Color-List takes care of the actual creation of the
 list to be stored in the-color-list.
====================================================================
|#

(defun Create-Color-List ()
  (let ((val 0) l2 l)
    (dotimes (i 18)
      (let* ((triplet (nth val rgbvalues)))
	(if (schema-p (car triplet))
	    ;; Must be a pair of styles
	    (push (copy-list triplet) l2)
	    (let ((red (first triplet))
		  (green (second triplet))
		  (blue (third triplet)))
	      (push (create-instance NIL opal:line-style
		      (:line-thickness 2)
		      (:foreground-color
		       (create-instance NIL opal:color
			 (:red red) (:green green) (:blue blue))))
		    l)
	      (push (create-instance NIL opal:filling-style
		      (:foreground-color
		       (create-instance NIL opal:color
			 (:red red) (:green green) (:blue blue))))
		    l)
	      (push l l2)
	      (setq l NIL))))
      (incf val 1))
    (setq the-color-list l2)))


#|
====================================================================
CURRENT STATE

The Current-State menu shows the current colors selected for the
line-styles and filling-styles.
====================================================================
|#

(defun Create-Current-State ()
  (create-instance 'CURRENT-STATE opal:aggregadget
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
		      (:string "LINE")
		      (:font ,(opal:get-standard-font
			       :sans-serif :roman :small)))
	  (:fill-text ,opal:text
		      (:left 31) (:top 491)
		      (:string "FILL")
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
	(:obj-over NIL)
	(:left ,(o-formula (gvl :obj-over :left)))
	(:top ,(o-formula (gvl :obj-over :top)))
	(:width ,(o-formula (gvl :obj-over :width)))
	(:height ,(o-formula (gvl :obj-over :height)))
	(:visible ,(o-formula (gvl :obj-over)))
	(:fast-redraw-filling-style NIL)
	(:parts
	 ((:black-rect ,opal:rectangle
		       (:left ,(o-formula (+ 1 (gvl :parent :left))))
		       (:top ,(o-formula (+ 1 (gvl :parent :top))))
		       (:width ,(o-formula (- (gvl :parent :width) 2)))
		       (:height ,(o-formula (- (gvl :parent :height) 2)))
		       (:line-style ,*FEEDBACK-LINE-STYLE*)
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


  (let* ((selectable-objs (g-value CURRENT-STATE :selectable-objs))
	 (frame (g-value selectable-objs :frame))
	 (filler (g-value selectable-objs :filler)))
    (s-value selectable-objs :selected frame)
    (s-value (g-value CURRENT-STATE :feedback) :obj-over frame)
    (s-value frame :selected t)
    (s-value frame :line-hold
	     (g-value palette-feedback :obj-over :line-hold))
    (s-value frame :filling-style
	     (g-value palette-feedback :obj-over :filling-style))
    (s-value filler :filling-style
	     (g-value (car (g-value stipple-palette :patterns-agg :components))
		      :filling-style)))
  )

(defun Create-Main-Window ()

  ;; This is the main window.
  (create-instance 'WIN inter:interactor-window
    (:left 10) (:top 40) (:width 750) (:height 512)
    (:background-color (create-instance NIL opal:color
			 (:red 0.65) (:blue 0.65) (:green 0.65)))
    (:position-by-hand NIL)
    (:title (concatenate 'simple-string "Garnet Draw V" GarnetDraw-Version)))

  ;; This aggregate is where the tool and paint palette are stored,
  ;; and the feedback current selection of the palettes are stored
  ;; here.

  (s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "GARNETDRAW"))
     (g-value win :destroy-hooks)))

  (Create-Moving-Prototypes) ; Creates MOVING-RECT, MOVING-OVAL, etc.
  (Create-Text-Feedback)     ; Creates TEXT-FEEDBACK
  (Create-Main-Menubar)      ; Creates MAIN-MENU
  (Create-Line-Palette)      ; Creates LINE-PALETTE and LINE-FEEDBACK
  (Create-Tool-Palette)      ; Creates TOOLS-MENU and TOOL-FEEDBACK
  (Create-Fill-Palettes)     ; Creates STIPPLE-PALETTE, COLOR-PALETTE, and
                             ; PALETTE-FEEDBACK
  (Create-Color-List)

  (Create-Current-State)     ; Creates CURRENT-STATE

  (opal:add-components TOP-AGG
		       TOOLS-MENU TOOL-FEEDBACK
		       CURRENT-STATE line-palette line-feedback
		       stipple-palette)

  (if (g-value opal:color :color-p)
      (opal:add-component TOP-AGG color-palette))
  (opal:add-component TOP-AGG palette-feedback)

  (opal:update win)

  ;; These functions must be called after WIN is updated!
  (opal:add-component TOP-AGG main-menu)

  (opal:update win)
  (setf *Grid-Menu-Item* (gg:find-submenu-component main-menu "  Options  "
						    " Turn Grid On "))
  (setf *Grid-Vis-Item* (gg:find-submenu-component main-menu "  Options  "
						    " Show Grid Dots "))
  (gg:menubar-disable-component *Grid-Vis-Item*) ; not implemented yet
  (gg:menubar-disable-component
   (gg:find-submenu-component main-menu "  Edit  " " Reshape "))
  )


#|
====================================================================
MOVER GROWER GADGET

This is the gadget used to move and scale different graphical objects.
====================================================================
|#


;;; If the newly selected object is a *polyline*, enable
;;; the "reshape" item in the menubar.  Otherwise, disable it.
(defun mover-grower-sel-fn (gad new-sel)
  (declare (ignore gad))
  (let ((selected-obj (when (not (cdr new-sel)) (car new-sel))))
    (if (is-a-p selected-obj opal:polyline)
	(gg:menubar-enable-component
	 (gg:find-submenu-component main-menu "  Edit  " " Reshape "))
	(gg:menubar-disable-component
	 (gg:find-submenu-component main-menu "  Edit  " " Reshape ")))))

(defun Create-MOVER-GROWER ()
  (create-instance 'MOVER-GROWER garnet-gadgets:multi-Graphics-Selection
    (:input-filter (o-formula (if (gv GRID-OBJ :gridon)
				  (gv GRID-OBJ :gridamt)
				  NIL)))
    (:selection-function 'mover-grower-sel-fn)
    (:start-where (list :element-of-or-none *DRAW-AGG*))
    (:check-line T)
    (:check-polygon T)
    (:check-group T)
    (:check-grow-p T)
    (:multiple-select t)
    (:movegrow-boxes-p t)
    (:movegrow-lines-p t)
    (:value NIL)
    (:running-where t))

  )

#|
====================================================================
POLYGON GADGET

Used to make polygons when the polygon tool is the one being used.
====================================================================
|#
(defun edit-polyline-start-where (obj inter ev)
  (declare (ignore inter))
  ;; make sure event is in the right window
  (when (eq (g-value obj :window) (inter:event-window ev))
    (let ((obj (kr-send *DRAW-AGG* :point-to-leaf *DRAW-AGG*
			(inter:event-x ev) (inter:event-y ev) :type opal:polyline)))
      (if obj
	  obj
	  T))))

;; (list :leaf-element-of *DRAW-AGG* :type opal:polyline)

(defun Create-Polygon-Maker ()
  (create-instance 'POLYGON-MAKER garnet-gadgets:polyline-creator
    (:input-filter (o-formula (if (gv GRID-OBJ :gridon)
				  (gv GRID-OBJ :gridamt)
				  NIL)))
    (:start-event :rightdown)
    (:deleter-start-event :double-leftdown)
    (:start-where `(:in ,*DRAW-AGG*))
    (:close-enough-value 3)
    (:active-p (o-formula (gv (nth 5 (gv TOOLS-MENU :components))
			      :selected)))
    (:running-where t)
    (:selection-function
     #'(lambda (gadget new-point-list)
	 (declare (ignore gadget))
	 (garnet-gadgets:set-selection MOVER-GROWER NIL)
	 (let* ((selectable-objs (g-value CURRENT-STATE :selectable-objs))
		(fill (g-value selectable-objs :filler :filling-style))
		(frame (g-value selectable-objs :frame))
		(line-hold (g-value frame :line-hold))
		(new-obj
		 (create-instance NIL opal:polyline
		   (:point-list (copy-list new-point-list))
		   (:line-p NIL)
		   (:polygon-p t)
		   (:grow-p t)
		   (:group-p NIL)
		   (:filling-style (if fill
				       (create-instance NIL fill)))
		   (:line-style (Get-Line-Style
				 (g-value line-palette :selected :line-thick)
				 (g-value line-hold :foreground-color))))))
	   (with-constants-disabled
	       (opal:add-component *DRAW-AGG* new-obj))
	   (garnet-gadgets:set-selection MOVER-GROWER new-obj)))))


  ;; this baby takes care of the case where you have a polyline being edited,
  ;; and you move the mouse over another polyline and hit meta-r.  It should
  ;; start editing the second polyline.
  (create-instance 'edit-polyline-inter inter:button-interactor
    (:operates-on POLYGON-MAKER)
    (:active (o-formula
	      (OR
	       (gv (gg:find-submenu-component main-menu "  Edit  " " Reshape ") :enabled)
	       (gvl :operates-on :polyline-being-edited))))
    (:start-where (o-formula (list :custom (gvl :operates-on)
				   'edit-polyline-start-where)))
    (:window DRAW-WIN)
    (:start-event :meta-\r)
    (:continuous NIL)
    ;; When you hit meta-r, the accelerator first calls the reshape
    ;; function if a polyline is selected.  Then, this interactor is
    ;; started.  You don't want to call reshape-fn unless it's not
    ;; been called by the accelerator, since if you call reshape-fn
    ;; twice, it will turn off the handles!!
    (:final-function #'(lambda (inter obj)
			 (unless (g-value POLYGON-MAKER :reshape-called-p)
			   (reshape-fn inter obj))
			 (s-value POLYGON-MAKER :reshape-called-p NIL))))
  )



(defun Create-Draw-Window ()

  ;; This is the window in which the drawings are done.
  (create-instance 'DRAW-WIN inter:interactor-window
    (:left 55)
    (:top (+ 2 (g-value MAIN-MENU :height)))
    (:width 680)
    (:height (- (g-value WIN :height) (g-value MAIN-MENU :height)
		(g-value STIPPLE-PALETTE :height) 7))
    (:border-width 2)
    (:omit-title-bar t)
    (:parent WIN))

  ;; This aggregate is where the feedback objects for the different
  ;; objects are stored.

  (s-value DRAW-WIN :aggregate
	   (create-instance 'TOP-DRAW-AGG opal:aggregate))

  ;; The *DRAW-AGG* is used to actually store the actual drawings.

  (setf *DRAW-AGG* (create-instance NIL opal:aggregadget
		     (:left 0) (:top 0)
		     (:width (o-formula (gv DRAW-WIN :width)))
		     (:height (o-formula (gv DRAW-WIN :height)))))

  ;; MOVING-RECT, MOVING-OVAL, etc. were created in Create-Main-Window
  ;; TEXT-FEEDBACK was created in Create-Main-Window
  (Create-MOVER-GROWER)      ; Creates MOVER-GROWER
  (Create-Polygon-Maker)     ; Creates POLYGON-MAKER

  (opal:add-components TOP-DRAW-AGG
		       *DRAW-AGG* MOVER-GROWER MOVING-LINE MOVING-RECT
		       moving-oval MOVING-ROUNDTANGLE TEXT-FEEDBACK
		       new-moving-arrowline moving-doublearrowline
		       polygon-maker)
  )


;; called for rect, oval, line, arrow-lines, etc.
;; NOT called for text or polygons
(defun create-copy-obj (creator-obj line-p points-list filler-fill
				    frame-color line-thick)
  (let ((obj (with-constants-disabled
		 (opal:copy-gadget creator-obj NIL))))
    (if line-p
	(s-value obj :points (copy-list points-list))
	(s-value obj :box (copy-list points-list)))
    (s-value obj :filling-style (if filler-fill
				    (create-instance NIL filler-fill)
				    NIL))
    (s-value obj :line-style
	     (Get-Line-Style line-thick frame-color))
    obj))



(defun Create-Interactors ()

  #|
  ====================================================================
  TWO POINT INTERACTOR

  This is the interactor used to give information necessary for drawing
  a new object.
  ====================================================================
  |#

  (create-instance 'NEW-ITEM inter:two-point-interactor
    (:window DRAW-WIN)
    (:active (o-formula (and (not (gv (nth 5 (g-value TOOLS-MENU :components))
				      :selected))
			     (not (gv (nth 4 (g-value TOOLS-MENU :components))
				      :selected)))))
    (:input-filter (o-formula (if (gv GRID-OBJ :gridon)
				  (gv GRID-OBJ :gridamt)
				  NIL)))
    (:start-event :rightdown)
    (:start-where T)
    (:final-function
     #'(lambda (an-interactor points-list)
	 (garnet-gadgets:set-selection MOVER-GROWER NIL)
	 (when points-list
	   (let* ((selectable-objs (g-value CURRENT-STATE :selectable-objs))
		  (creator-obj (g-value an-interactor :creator-obj))
		  (frame-color (g-value selectable-objs :frame :line-hold
					:foreground-color))
		  (filler-fill (g-value selectable-objs :filler
					:filling-style))
		  (line-thick (g-value LINE-PALETTE :selected :line-thick))
		  (obj (create-copy-obj creator-obj
					(g-value an-interactor :line-p)
					points-list
					filler-fill frame-color line-thick)))
	     (with-constants-disabled
		 (opal:add-component *DRAW-AGG* obj))
	     (garnet-gadgets:set-selection MOVER-GROWER obj)
	     obj))))
    (:outside-action :last)
    (:feedback-obj (o-formula (gv TOOLS-MENU :selected :feedback-object)))
    (:creator-obj (o-formula (gv TOOLS-MENU :selected :creator-object)))
    (:line-p (o-formula (gv TOOLS-MENU :selected :feedback-object :line-p)))
    (:Min-length 0)
    (:Min-height 0)
    (:Min-width 0))

  #|
  ====================================================================
  TEXT EDITING

  This is used to decide if a newly selected object is text, if so
  then we must set it up so it is editable.  This is the interactor
  used to do text editing.
  ====================================================================
  |#

  (create-instance 'CREATE-OR-EDIT inter:text-interactor
    (:feedback-obj (o-formula (if (eq :none (gvl :first-obj-over))
				  TEXT-FEEDBACK)))
    (:active (o-formula (gv (nth 4 (gv TOOLS-MENU :components))
			    :selected)))
    (:start-where `(:element-of-or-none ,*DRAW-AGG*
		    :type ,opal:cursor-multi-text))
    (:input-filter (o-formula (if (gv GRID-OBJ :gridon)
				  (gv GRID-OBJ :gridamt)
				  NIL)))
    (:window DRAW-WIN)
    (:waiting-priority *garnetdraw-high-priority*)
    (:running-priority *garnetdraw-high-running-priority*)
    (:start-event :any-rightdown)
    (:stop-event '(:any-mousedown :control-\j))
    (:start-action #'(lambda (inter obj ev)
		       ;; make sure the font of the feedback object is correct
		       (let ((feed (g-value inter :feedback-obj)))
			 (when feed
			   (let ((current-font (g-value TOOLS-MENU :text-tool
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
	 (garnet-gadgets:set-selection MOVER-GROWER NIL)
	 (when (eq :none obj-being-edited)
	   (let* ((current-font (g-value TOOLS-MENU :text-tool
					 :text-state :font))
		  (current-color (g-value CURRENT-STATE :selectable-objs
					  :frame :line-hold
					  :foreground-color))
		  (new-str
		   (create-instance NIL opal:cursor-multi-text
		     (:box (list x y 0 0))
		     (:string (copy-seq final-string))
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:text-p t)
		     (:font (opal:get-standard-font
			     (g-value current-font :family)
			     (g-value current-font :face)
			     (g-value current-font :size)))
		     (:line-style (Get-Line-Style
				   (g-value line-palette :selected :line-thick)
				   current-color)))))
	     (with-constants-disabled
		 (opal:add-component *DRAW-AGG* new-str))
	     (garnet-gadgets:set-selection MOVER-GROWER new-str)
	     )))))

  #+comment
  (create-instance 'DELETE inter:button-interactor
    (:continuous NIL)
    (:start-where T)
    (:start-event #\rubout)
    (:final-function
     #'(lambda (an-interactor final-obj-over)
	 (declare (ignore an-interactor final-obj-over))
	 (setf *temp-list* NIL)
	 (dolist (item (copy-list (g-value MOVER-GROWER :value)))
	   (with-constants-disabled
	       (opal:remove-component *DRAW-AGG* item))
	   (setf *temp-list* (cons item *temp-list*)))
	 (garnet-gadgets:set-selection MOVER-GROWER NIL)
	 (dolist (item (copy-list *temp-list*))
	   (opal:destroy item))
	 (setf *temp-list* NIL)))
    (:window `(,draw-win ,WIN)))

  )


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  (declare (ignore double-buffered-p))
  (Create-Color-List)
  (Create-Main-Window)
  (Create-Draw-Window)
  (s-value MAIN-MENU :accelerator-windows `(,WIN ,DRAW-WIN))

  (gg:Standard-Initialize-Gadget MAIN-MENU MOVER-GROWER *DRAW-AGG*)

  ;; This is necessary because the items functions are called
  ;; by the motif-menu sitting in the submenus
  (dolist (menu (g-value MAIN-MENU :menubar-items :components))
    (gg:Standard-Initialize-Gadget (g-value menu :submenu)
				   MOVER-GROWER *DRAW-AGG*))

  (Create-Query-Gadget)      ; Creates *Q-BOX*
  (Create-File-DB)           ; Creates SAVE-WIN, *SAVE-DB*
  (Create-Grid-DB)           ; Creates GRID-WIN, *GRID-DB*
  (Create-Read-DB)           ; Creates PS-Read-DB, *Read-DB*
  (Create-Interactors)

  (opal:update WIN T)

  (Format T "~%GARNET DRAW v1.0:

To Draw an object:
^^^^^^^^^^^^^^^^^
  1. Select the type of object to be created from the Tools-Menu
     on the left side of the screen.

  2. Press and drag the mouse button to the desired size for the
     object.  If text then just depress mouse button and release,
     to end editing type ctrl-j or click with the mouse elsewhere.
     For polygon, depress right mouse button, release and click
     again for next point, etc. till either you have added enough
     points or you don't wish to add any more points.  If you don't
     want to add any more points then depress any other button to
     stop.


To select objects:
^^^^^^^^^^^^^^^^^
  1. Use the left button to select a single item or the middle mouse
     button to select multiple objects.

  2. Depress the middle mouse button and select the region within
     which you want all objects to be selected.


To change line or filling color of a(n) object(s):
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  1. Select the object(s) to be changed, choose either fill or line
     and then select the appropriate palette pattern or color.

  2. To change the default line and filling colors, make sure no
     objects are selected.  Then select line/fill and select the
     palette item of your choice.


To change size, family, or face of text objects:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  1. Select text object and use Font menu to change item.

  2. To change the default font, deselect all items, then choose
     appropriate menu items from Font menu.  (NOTE:  You can
     see what the default font is in the text tools slot of the
     tools menu.)

To edit a polygon(s):
^^^^^^^^^^^^^^^^^^^^
  1. Select polygon to be edited.

  2. Type Meta-r or select 'Reshape' from the 'Edit' menu.

  3. To move a point, use the left mouse button to drag the handles.
     Control-g while moving will return the point to it's previous
     location.  Typing DELETE while moving will delete the point.

     To add a point, click left mouse button on the line where you
     want to add the point.  You can start dragging the point without
     letting go of the button.

     To delete a point, double click with the left button on the point
     to be deleted.

  4. To edit a different polygon from the one you are editing now,
     move the mouse over the polygon you want to edit and hit meta-r.

~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

(defun Do-Stop ()
  (opal:destroy WIN)

  ;; These interactors were probably not active when WIN was destroyed, so
  ;; we must destroy them explicitly.
  (dolist (sym '(NEW-ITEM CREATE-OR-EDIT EDIT-POLYLINE-INTER))
    (if (and (boundp sym) (schema-p (eval sym)))
	(opal:destroy (eval sym))))

  ;; These prototype objects were not added to a window themselves, so we
  ;; must destroy them explicitly.
  (dolist (sym '(PALETTE-ITEM COLOR-PALETTE-ITEM MOVING-AGG
		 CREATOR-LINE CREATOR-ARROWLINE CREATOR-DOUBLEARROWLINE
		 CREATOR-RECT CREATOR-ROUNDTANGLE CREATOR-OVAL))
    (if (and (boundp sym) (schema-p (eval sym)))
	(opal:destroy (eval sym))))

  ;;for demo-controller
  (unless (and (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
	       (Common-Lisp-User::Garnet-Note-Quitted "GARNETDRAW")))
  )

#|
Things to fix:

* Align-to-grid command

|#
