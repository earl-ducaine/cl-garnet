;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-SCHEMA-BROWSER; Base: 10 -*-
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


;;;  SCHEMA BROWSER INTERFACE
;;;
;;;     This interface can be used to examine the hierarchy of Garnet objects
;;;  by tracing through the IS-A/INSTANCES and PARENT/COMPONENTS links among
;;;  objects.
;;;     When the name of a schema is typed into the labeled box at the top
;;;  of the window, all of the children of that schema appear in the first
;;;  menu.  The radio buttons determine whether the components or the instances
;;;  of the schema are shown.  Leftdown on one of the children in the menu
;;;  causes all of its children to be shown in the next menu.
;;;     When the "Prev" button is pressed, the ancestor of the top-level
;;;  schema becomes the new top-level schema.  This ancestor is found through
;;;  the inverse of the link selected in the radio buttons.
;;;     Middledown on any schema shown in the interface will cause a gray
;;;  feedback box to be drawn around the schema name.  Once a schema has been
;;;  highlighted in this way, clicking on the "PS" button will cause the
;;;  slot values of the schema to be printed in the interpreter, and clicking
;;;  on the "Top" button will cause the highlighted schema to become the new
;;;  top-level object.
;;;
;;;     The following gadget modules are loaded at the top of this file:
;;;  h-scroll-bar, radio-buttons, text-buttons, labeled-box, trill-device,
;;;  error-gadget, and browser-gadget.
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish
;;;


(in-package :DEMO-SCHEMA-BROWSER)

(defvar DEMO-SCHEMA-BROWSER-INIT
  (dolist (file '("radio-buttons-loader" "text-buttons-loader"
		  "labeled-box-loader" "trill-device-loader"
		  "error-gadget-loader" "browser-gadget-loader"))
    (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))



;;   Schemata defined in the DO-GO procedure which are referenced by other
;; functions
;;
(declaim (special SCHEMA-BROWSER CONTROL-PANEL SCHEMA-BROWSER-ERROR-GADGET
		  SCHEMA-BROWSER-CONTROL-BUTTON SCHEMA-BROWSER-WIN
		  SCHEMA-BROWSER-TOP-AGG L-BOX-GRAY-INTER))


(create-instance 'SCHEMA-BROWSER-CONTROL-BUTTON garnet-gadgets:text-button-panel
   (:shadow-offset 5)
   (:gray-width 3)
   (:text-offset 3)
   (:final-feedback-p NIL)
   (:direction :horizontal))


;;;
;;;  DO-GO:  Function to run SCHEMA-BROWSER interface
;;;

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)

  (create-instance 'SCHEMA-BROWSER-WIN inter:interactor-window
     (:left 10)(:top 50)(:width 600)(:height 270)
     (:double-buffered-p double-buffered-p)
     (:title "SCHEMA BROWSER") (:icon-title "Schema-Browser"))
  (s-value SCHEMA-BROWSER-WIN
	   :aggregate
	   (create-instance 'SCHEMA-BROWSER-TOP-AGG opal:aggregate))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-SCHEMA-BROWSER"))
     (g-value schema-browser-win :destroy-hooks)))


  ;; Create SCHEMA-BROWSER schema and add to window
  ;;
  (create-instance 'SCHEMA-BROWSER garnet-gadgets:browser-gadget
     (:left 10)
     (:top 85)
     (:item-to-string-function
      #'(lambda (schema)
	  (if schema
	      (string-capitalize (name-for-schema schema))
	      "")))
     (:menu-items-generating-function
      #'(lambda (schema)
	 (if schema
	  (if (string= (g-value CONTROL-PANEL :generating-control :value)
		       "Components")
	      (g-value schema :components)
	      (get-local-value schema :is-a-inv))))))
  (opal:add-component SCHEMA-BROWSER-TOP-AGG SCHEMA-BROWSER)
  (opal:update SCHEMA-BROWSER-WIN)


  ;; Create CONTROL-PANEL and add to window
  ;;
  (create-instance 'CONTROL-PANEL opal:aggregadget
     (:constant :top :left)
     (:left 10)
     (:top 10)
     (:first-row-bottom (o-formula (+ (gvl :top) (gvl :gray-control :height))))
     (:parts

      ;;     This part is a horizontal panel of two radio buttons.  The choices
      ;; are "Instances" and "Components", which determine how the menu items
      ;; are derived from each item in the top-level SCHEMA-BROWSER's :items
      ;; list.
      ;;     The :selection-function reinitializes the browser to contain only
      ;; the first item in the browser list, since changing from Instances to
      ;; Components (or vice versa) leaves all of the derrived menus
      ;; meaningless.
      `((:generating-control ,garnet-gadgets:radio-button-panel
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:direction :horizontal)
	    (:items ("Instances" "Components"))
	    (:selection-function
	     ,#'(lambda (gadget value)
		  (declare (ignore gadget value))
		  (garnet-gadgets:set-first-item
		   SCHEMA-BROWSER (car (g-value SCHEMA-BROWSER :items))))))

	;;    This part is a horizontal panel of two text buttons, with choices
	;; "PS" and "Top".  When the gray feedback box is over an item, then
	;; clicking on "PS" will print the schema to the interpreter window,
	;; and clicking on "Top" will cause the item to be promoted to the top
	;; of the browser.
	(:gray-control ,SCHEMA-BROWSER-CONTROL-BUTTON
	    (:left ,(o-formula (+ 20 (gvl :parent :left)
				  (gvl :parent :generating-control :width))))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:items ("PS" "Top"))
	    (:selection-function
	     ,#'(lambda (gadget value)
		  (declare (ignore gadget))
		  (let ((coordinate (g-value SCHEMA-BROWSER
					     :additional-selection-coordinate)))
		    (when coordinate
		      ;; Gray box is over an item in one of the menus
		      (if (equal value "Top")
			  (progn
			    (garnet-gadgets:promote-item
			     SCHEMA-BROWSER coordinate)
			    (s-value SCHEMA-BROWSER
				     :additional-selection-coordinate NIL))
			  (ps (g-value SCHEMA-BROWSER :additional-selection)))
		      ;; Gray box is over the labeled-box
		      (if (g-value SCHEMA-BROWSER :gray-feedback :obj-over)
			  (let ((item (car (g-value SCHEMA-BROWSER :items))))
			    (if item
				(when (equal value "PS")
				  (ps item))
				(garnet-gadgets:display-error
				 SCHEMA-BROWSER-ERROR-GADGET
				 (concatenate 'string
					      "Unable to access schema "
					      (g-value CONTROL-PANEL :l-box :value)))))))))))

	(:l-box ,garnet-gadgets:labeled-box
	    (:left ,(o-formula (+ 20 (gvl :parent :gray-control :left)
				  (gvl :parent :gray-control :width))))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:label-string "Object:")
	    ;;     Have to use kr::schema-name instead of kr:name-for-schema so
	    ;; that the package name will be displayed as well as the name of
	    ;; the schema.
	    (:value ,(o-formula (let ((item (car (gv SCHEMA-BROWSER :items))))
				  ; format to make the schema give itself a name
				  (format NIL "~S" item)
				  (if item
				      (prin1-to-string
				       (kr::schema-name item))
				      ""))))
	    (:selection-function
	     ,#'(lambda (gadget string)
		  (declare (ignore gadget))
		  (let* ((value (eval (garnet-utils:verify-binding string)))
			 (item (if (schema-p value)
				   value
				   (garnet-gadgets:display-error
				    SCHEMA-BROWSER-ERROR-GADGET
				    (concatenate 'string
				     "Unable to access schema " string)))))
		    (if item
			(garnet-gadgets:set-first-item SCHEMA-BROWSER item))))))

	(:t-box ,garnet-gadgets:trill-device
	    (:left ,(o-formula (+ 30 (gvl :parent :left))))
	    (:top ,(o-formula (+ 10 (gvl :parent :first-row-bottom))))
	    (:page-trill-p NIL)
	    (:value ,(o-formula (gv SCHEMA-BROWSER :num-menus)))
	    (:selection-function
	     ,#'(lambda (gadget value)
		  (declare (ignore gadget))
		  (s-value SCHEMA-BROWSER :num-menus value)
		  )))

	(:prev ,SCHEMA-BROWSER-CONTROL-BUTTON
	    (:left ,(o-formula (+ 30 (gvl :parent :t-box :left)
				  (gvl :parent :t-box :width))))
	    (:top ,(o-formula (+ 5 (gvl :parent :first-row-bottom))))
	    (:items ("Prev"))
	    (:selection-function
	     ,#'(lambda (gadget value)
		  (declare (ignore gadget value))
		  (let* ((l-box (g-value CONTROL-PANEL :l-box))
			 (string (g-value l-box :value))
			 (browser-items (g-value SCHEMA-BROWSER :items)))
		    (if browser-items
			(let* ((old-top-level-item (car browser-items))
			       (new-top-level-item
				(if (equal (g-value CONTROL-PANEL
						    :generating-control :value)
					   "Components")
				    (g-value old-top-level-item :parent)
				    ;; Added car in next line -- ECP 3/27/92
				    (car (g-local-value old-top-level-item :is-a)))))
			  (if new-top-level-item
			      ;; Add the new item to the browser
			      (garnet-gadgets:push-first-item
			       SCHEMA-BROWSER new-top-level-item)
			      (garnet-gadgets:display-error
			       SCHEMA-BROWSER-ERROR-GADGET
			       (concatenate 'string string
					    " has no predecessor."))))
			;; There is no item to get the predecessor of
			(garnet-gadgets:display-error
			 SCHEMA-BROWSER-ERROR-GADGET "No schema specified."))))))

	(:quit ,SCHEMA-BROWSER-CONTROL-BUTTON
	    (:left ,(o-formula (+ 30 (gvl :parent :prev :left)
				  (gvl :parent :prev :width))))
	    (:top ,(o-formula (+ 5 (gvl :parent :first-row-bottom))))
	    (:items ("Quit Browser"))
	    (:selection-function ,#'do-quit)))))
  (opal:add-component SCHEMA-BROWSER-TOP-AGG CONTROL-PANEL)
  (s-value (g-value CONTROL-PANEL :generating-control) :value "Components")
  (opal:update SCHEMA-BROWSER-WIN)


  (create-instance 'SCHEMA-BROWSER-ERROR-GADGET garnet-gadgets:error-gadget
     (:constant T)
     (:parent-window SCHEMA-BROWSER-WIN))


  (create-instance 'L-BOX-GRAY-INTER inter:menu-interactor
     (:window SCHEMA-BROWSER-WIN)
     (:start-where (list :in-box (g-value CONTROL-PANEL :l-box :frame)))
     (:start-event :middledown)
     (:start-action
      ;; The :obj-over slot of the gray feedback object is set to the FRAME
      ;; part of the labeled box.  (Small abuse of :additional-selection, which
      ;; should ideally be set to the item named by the string in the l-box.
      ;; Here, :additional-selection is just gets NIL, since error checking
      ;; would involve a call to VERIFY-BINDING and maybe fiddling with the
      ;; ERROR-GADGET, etc.)
      #'(lambda (interactor l-box-frame)
	  (declare (ignore interactor))
	  (let ((feedback-obj (g-value SCHEMA-BROWSER :gray-feedback)))
	    (if (equal (g-value feedback-obj :obj-over) l-box-frame)
		(s-value SCHEMA-BROWSER :additional-selection-coordinate NIL)
		(progn
		  (s-value SCHEMA-BROWSER :additional-selection-coordinate NIL)
		  (s-value SCHEMA-BROWSER :additional-selection
			   (car (g-value SCHEMA-BROWSER :items)))
		  (s-value feedback-obj :obj-over l-box-frame))))))
     (:stop-action NIL))

  (opal:update SCHEMA-BROWSER-WIN)

  (format t "~%Demo-Schema-Browser:
      Typing the name of a Garnet schema in the labeled box will cause the
   components of that schema to be displayed in the first menu.
      If one of the items is selected with the left mouse button, then its
   components will be shown in the next menu.
      Pressing on the 'Prev' button will show the parent of the first schema.
      If the 'Instances' button is selected, then instances of the schemas
   will be displayed, rather than components.
      If middledown is used to select an item, then the schema may be printed
   to the shell by pressing 'PS', or promoted to the top level of the browser
   by pressing 'Top'.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

(defun do-stop ()
  (opal:destroy SCHEMA-BROWSER-WIN))

(defun do-quit (gadget value)
  (declare (ignore gadget value))
  (do-stop)
  ;; for demo-controller
  (unless (and (fboundp 'common-lisp-user::Garnet-Note-Quitted)
	       (common-lisp-user::Garnet-Note-Quitted "DEMO-SCHEMA-BROWSER")))
)

