;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MULTIFONT; Base: 10 -*-

;;; The Garnet User Interface Development Environment.

;;; This code was written as part of the Garnet project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you are using this code or any part of Garnet,
;;; please contact garnet@cs.cmu.edu to be put on the mailing list.

;;; This file contains demo code for testing the text interactor
;;;
;;; This is intended as a test and demonstration of the text interactor
;;; as part of the Garnet project.
;;; 
;;; ** Call (Do-Go) to start and (Do-Stop) to stop **
;;;
;;; Designed by Brad A. Myers
;;; Implemented by Richard McDaniel

(in-package :cl-processing)

(declaim (special focus-inter mouse-inter text1 text2 win top message
		  pull-down scroll-win1 scroll-win2))

(defvar *test-debug* NIL)

;; First we load required gadgets.  In this demo, we use the menubar
;; and the motif-scrolling-window-with-bars gadgets.

;; (eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Only loads this file when not compiling all of Garnet.
  ;; (unless (get :garnet-modules :multifont)
  ;;   (common-lisp-user::garnet-load "opal:multifont-loader"))
  ;; Load menubar gadget.
  ;; (unless (get :garnet-modules :motif-menubar)
  ;;   (common-lisp-user::garnet-load "gadgets:motif-menubar-loader"))
  ;; Load scrolling window gadget.
  ;; (unless (get :garnet-modules :motif-scrolling-window)
  ;;   (common-lisp-user::garnet-load "gadgets:motif-scrolling-window-loader")))
;; Following are functions used in the menubar.  Each function is used
;; to control either a menu or a submenu item.

;; This is used in the "New" submenu of "File."  This function clears
;; the two windows of text.
(defun new-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (opal:set-text text1 nil)
   (opal:set-text text2 nil)
   (inter:set-focus focus-inter text1))

;; The is the "Quit" selection of "File."  It exits the main loop.
(defun quit-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (Do-Stop))

#|
;; This is used in the "Color" menu.  It changes the color.
(defun change-color (gadget menu-item value)
   (declare (ignore gadget menu-item))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-color-of-selection multifont value)
	 (gg:auto-scroll multifont)
	 )
      )
   )
|#

;; This is used in the "Font" menu.  It changes the family of the current font.
(defun change-font (gadget menu-item value)
   (declare (ignore gadget menu-item))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-font-of-selection multifont nil :family value)
	 (gg:auto-scroll multifont))))


;; This is used in the "Size" menu.  It changes the size attribute of the
;; current font.
(defun change-size (gadget menu-item value)
   (declare (ignore gadget menu-item))
   (let ((multifont (g-value focus-inter :obj-to-change)))
     (when multifont
         (opal:change-font-of-selection multifont nil :size value)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Copy" selection of the "Edit" menu.  It copies whatever is
;; currently selected.  The copied item is put in the cut-buffer.
(defun copy-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (inter:copy-selection focus-inter)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Cut" selection of the "Edit" menu.  It deletes whatever is
;; currently selected.  The deleted item is put in the cut-buffer.
(defun cut-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (inter:cut-selection focus-inter)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Paste" selection of the "Edit" menu.  It copies whatever is
;; in the cut buffer at the current cursor position.
(defun paste-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (inter:paste-selection focus-inter)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Italic" selection of the "Edit" menu.  It toggles the current
;; font to be italic.
(defun italic-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-font-of-selection multifont nil :italic :toggle-first)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Bold" selection of the "Edit" menu.  It toggles the current
;; font to be bold.
(defun bold-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-font-of-selection multifont nil :bold :toggle-first)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Font Bigger" selection of the "Edit" menu.  It toggles the
;; current font to be one size larger.
(defun bigger-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-font-of-selection multifont nil :size :bigger)
	 (gg:auto-scroll multifont)
      )
   )
)


;; This is the "Font Smaller" selection of the "Edit" menu.  It toggles the
;; current font to be one size smaller.
(defun smaller-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((multifont (g-value focus-inter :obj-to-change)))
      (when multifont
         (opal:change-font-of-selection multifont nil :size :smaller)
      )
   )
)

;; This is the "Toggle Lisp Mode" selection of the "Edit" menu.  It toggles the
;; text-handling mode between Lisp and Normal.
(defun lisp-fn (gadget menu-item value)
   (declare (ignore gadget menu-item value))
   (let ((lisp-mode (not (g-value focus-inter :lisp-mode-p))))
     (if lisp-mode
       (progn
	 (opal:set-text text1 (inter:lispify (opal:get-string text1)))
	 (opal:set-text text2 (inter:lispify (opal:get-string text2))))
       (progn
	 (inter:turn-off-match focus-inter text1)
	 (inter:turn-off-match focus-inter text2)))
     (s-value focus-inter :lisp-mode-p lisp-mode)
     (s-value focus-inter :match-parens-p lisp-mode)
     (s-value mouse-inter :match-parens-p lisp-mode)
     )
   )


;; The main procedure.  Initializes all garnet gadgets and interactors.
(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  (declare (ignore double-buffered-p))
  
  ;; Create the main window.
  (create-instance 'win inter:interactor-window
    (:title "Multifont Demonstration")
    (:top 100)
    (:left 150)
    (:height 254)
    (:background-color opal:black)
    )
  (create-instance 'top opal:aggregate)
  (s-value win :aggregate top)
  (opal:update win)

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-MULTIFONT"))
     (g-value win :destroy-hooks)))
  
  ;; Create the message bar
  (create-instance 'message opal:multifont-text
    (:left 0)
    (:top (o-formula (- (gv win :height) 19)))
    (:word-wrap-p nil)
    (:auto-scroll-p nil)
    (:text-width (o-formula (gv win :width)))
    (:line-style opal:white-line)
    (:fill-background-p nil)
    )

  (opal:add-component top message)
   
  ;; Create the menubar.
  (create-instance 'pull-down garnet-gadgets:motif-menubar
    (:foreground-color opal:motif-green)
    (:items '(("File" nil
	       (("New" new-fn) ("Open") ("Save") ("Quit" quit-fn)))
	      ("Edit" nil
	       (("Copy" copy-fn) ("Cut" cut-fn) ("Paste" paste-fn)
		("Italic" italic-fn) ("Bold" bold-fn)
		("Font Bigger" bigger-fn) ("Font Smaller" smaller-fn)
		("Toggle Lisp Mode" lisp-fn)))
	      ("Font" change-font
	       ((:Fixed) (:Serif) (:Sans-Serif)))
	      ("Size" change-size
	       ((:Small) (:Medium)
		(:Large) (:Very-Large)))))
    (:bar-above-these-items '(NIL ("Italic" "Toggle Lisp Mode") NIL NIL))
    (:min-menubar-width (o-formula (gv win :width)))
    )
   
  (opal:add-component top pull-down)

  ;; Create the top window of the demo.
  (create-instance 'text1 opal:multifont-text ; This is the internal multifont
    (:word-wrap-p t)			      ; text object.
    (:auto-scroll-p T)
    (:fast-redraw-p :rectangle)
    (:fast-redraw-filling-style opal:motif-gray-fill)
    )
   
  (create-instance 'scroll-win1 gg:motif-scrolling-window-with-bars
    (:left 0)
    (:top (g-value pull-down :height))
    (:width (o-formula (- (gv win :width) (* 2 (gvl :border-width)))))
    (:height (o-formula (- (floor
			    (/ (- (gv win :height)
				  23 ;; height of paren-matching message
				  (g-value pull-down :height)) 2))
			   (* 2 (gvl :border-width)))))
    (:parent-window win)
    (:total-width (o-formula (+ (gv text1 :width) (gv text1 :left)) 200))
    (:total-height (o-formula (+ (gv text1 :top) (gv text1 :height)) 200))
    (:h-scroll-bar-p nil)
    (:v-scroll-bar-p t)
    )
  (s-value text1 :text-width (o-formula (gv scroll-win1 :clip-window :width)))
  (s-value text1 :scrolling-window scroll-win1)

  (opal:update scroll-win1)
  (opal:add-component (g-value scroll-win1 :inner-aggregate) text1)

  ;; Create the lower window of the demo.
  (create-instance 'text2 opal:multifont-text
    (:word-wrap-p t)
    (:auto-scroll-p T)
    (:fast-redraw-p :rectangle)
    (:fast-redraw-filling-style opal:motif-gray-fill)
    )
  (create-instance 'scroll-win2 gg:motif-scrolling-window-with-bars
    (:left 0)
    (:top (o-formula (+ (g-value pull-down :height) (gv scroll-win1 :height)
			(* 2 (gvl :border-width)))))
    (:width (o-formula (- (gv win :width) (* 2 (gvl :border-width)))))
    (:height (o-formula (- (gv win :height) (gvl :top)
			   23 ;; height of paren-matching message
			   (* 2 (gvl :border-width)))))
    (:parent-window win)
    (:total-width (o-formula (gv text2 :width) 200))
    (:total-height (o-formula (gv text2 :height) 200))
    (:h-scroll-bar-p nil)
    (:v-scroll-bar-p t)
    )
  (s-value text2 :text-width (o-formula (gv scroll-win2 :clip-window :width)))
  (s-value text2 :scrolling-window scroll-win2)

  (opal:update scroll-win2)
  (opal:add-component (g-value scroll-win2 :inner-aggregate) text2)

  ;; Create a focus interactor so that keyboard events may be entered
  ;; into the text objects.
  (create-instance 'focus-inter inter:focus-multifont-textinter
    (:window `(,win ,(g-value scroll-win1 :clip-window)
		    ,(g-value scroll-win1 :inner-window)
		    ,(g-value scroll-win2 :clip-window)
		    ,(g-value scroll-win2 :inner-window)))
    (:match-obj message))

  ;; Create a selection interactor to handle mouse events on the text
  ;; objects.
  (create-instance 'mouse-inter inter:selection-interactor
    (:window `(,win ,(g-value scroll-win1 :clip-window)
		    ,(g-value scroll-win1 :inner-window)
		    ,(g-value scroll-win2 :clip-window)
		    ,(g-value scroll-win2 :inner-window)))
    (:focus-interactor focus-inter)
    (:text-list `(,text2 ,text1))
    (:start-where `(:list-element-of ,mouse-inter :text-list :type
				     ,opal:multifont-text))
    (:match-obj message))
      
  (inter:set-focus focus-inter text1)
  (opal:update win)

  ;; Currently, open and save remain unimplemented. Their
  ;; corresponding menubar entries have been grayed to reflect this.
  (let ((bar (garnet-gadgets:get-bar-component pull-down "File")))
    (garnet-gadgets:menubar-disable-component
     (garnet-gadgets:get-submenu-component bar "Open"))
    (garnet-gadgets:menubar-disable-component
     (garnet-gadgets:get-submenu-component bar "Save"))
    )
  (Format T "~%Demo-Multifont:
  This creates and edits two multifont text objects within two motif-scrolling-
  window-with-bars.
  Clicking the cursor in the text objects will set the cursor to that object
  Dragging the cursor across text will select the text.
  Selecting any menu item will perform the action designated in that item.

Keyboard commands:
 ^f = forward char    meta-f = forward word     ^d = delete next char
 ^b = backward char   meta-b = backward word    del,bksp,^h = delete prev char
 ^p = prev line    ^n = next line  ^, = beginning of string  ^. = end of string
 ^a = beginning of line   ^e = end of line
 meta-d = delete next word      meta-h = delete prev word  ^u = delete all
 ^k = kill lines  ^u = delete entire string, ^w, CUT = delete selection
  META-w, COPY = copy selection to cut buffer
  ^c = copy entire string to X cut buffer
  ^y, PASTE = yank kill buffer or X cut buffer
  ^Y, ^PASTE = yank X buffer
  meta-y, meta-PASTE = yank kill buffer

The following ones extend the selection while moving
   ^leftarrow, ^rightarrow = prev, next char selecting
   meta-leftarrow, meta-rightarrow = prev, next word selecting
   ^uparrow, ^downarrow = up-line, down-line selecting
   ^HOME ^END = beginning, end of string selecting
   ^* = select all
Font changing:
 ^-shift-B = toggle bold  ^-shift-I = toggle italic
 ^-shift-F = fixed (courier)  ^-shift-T = times (serif)
               ^-shift-H = helvetica (sans-serif)
 ^-shift-< = smaller font  ^-shift-> = bigger font
 ^1 ^2 ^3 ^4 = small, medium, large and very-large fonts~%")


  (unless dont-enter-main-event-loop
    #-cmu (inter:main-event-loop)
    )
  )


;; Do-stop kills the parent window which will destroy all internal garnet
;; objects and gadgets.
(defun Do-Stop ()
   (opal:remove-component top pull-down)
   (opal:destroy pull-down)
   (opal:destroy win)
  ;;for demo-controller
   (unless (and (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
                (Common-Lisp-User::Garnet-Note-Quitted "DEMO-MULTIFONT"))
   )
)

