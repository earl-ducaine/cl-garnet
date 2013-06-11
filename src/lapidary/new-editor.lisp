;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (load "new-editor.lisp")
; (lapidary::editor-menu-do-go)

;;; CHANGE LOG
;;;
;;; 08/10/93 bvz - modified the editor menu so that it supported a selection
;;;                  mode in which users could indicate whether Lapidary
;;;                  should select leaves or top-level objects.

(in-package "LAPIDARY")

(defun editor-menu-do-go ()
  
  (editor-menu-do-stop)

  (create-instance 'EDITOR-MENU garnet-gadgets:menubar
		 (:constant '(t))
		 (:build-p t)
		 (:selection-function 'lap-menu-handler)
		 (:items '(
			   ("File" nil  (
					 ("load gadget")
					 ("save gadget")
					 ("add gadget")
					 ("quit")
					 ))
			   ("Edit" nil  (
					 ("make copy" )
					 ("make instance" )
					 ("delete object" )
					 ("delete window")
					 ))
			   ("Properties" nil  (
					       ("filling style" )
					       ("line style")
					       ("draw function" )
					       ("list properties" )
					       ("text properties" )
					       ("name object" )
					       ("parameters")
					       ))
			   ("Arrange" nil  (
					    ("bring to front")
					    ("send to back")
					    ("make aggregadget" )
					    ("ungroup" )
					    ))
			   ("Constraints"   nil  (
						  ("line constraints")
						  ("box constraints" )
						  ("c32")
						  ))
			   ("Other"   nil  (
					    ("clear workspace")	; new
					    ("interactors")
					    )) )))
  (create-instance 'EDITOR-WIN inter:interactor-window
		   (:title "editor menu")
		   (:left (first *editor-window-dimensions*))
		   (:top (second *editor-window-dimensions*))
		   (:width (g-value editor-menu :width)) 
;		   (:width 340)
		   (:height 130))

  (create-instance 'EDITOR-MENU-AGG opal:aggregate)
  (s-value editor-win :aggregate editor-menu-agg)
  (opal:update editor-win)


  (create-instance 'test-build-mode-label opal:text
    (:string "Test/Build Mode:")
    (:left 180)
    (:top 30))

  (create-instance 'test-build-obj garnet-gadgets:radio-button-panel
    (:text-on-left-p nil)
    (:direction :vertical)
    (:selection-function 'test-build-fct)
    (:top (+ (opal:bottom test-build-mode-label) 10))
    (:left 185)
    (:items '("test" "build")))

  (create-instance 'editor-menu-divider-line opal:line
    (:x1 170)
    (:x2 170)
    (:y1 (o-formula (gv editor-menu :height)))
    (:y2 (o-formula (gvl :window :height))))

  (create-instance 'selection-mode-label opal:text
    (:string "Selection Mode:")
    (:left 10)
    (:top 30))

  (create-instance 'selection-mode-gadget garnet-gadgets:radio-button-panel
    (:text-on-left-p nil)
    (:direction :vertical)
    (:selection-function #'(lambda (gadget value)
			     (declare (ignore gadget))
			     (declare (special *selection-info*))
			     (s-value *selection-info* :leaf
				      (string= value "leaves"))))
    (:items '("leaves" "top-level objects"))
    (:left 15)
    (:top (+ 10 (opal:bottom selection-mode-label))))

  (opal:add-component editor-menu-agg editor-menu)
  (opal:notice-items-changed editor-menu)
  (opal:add-components editor-menu-agg test-build-mode-label test-build-obj 
		                      editor-menu-divider-line
                                      selection-mode-label 
				      selection-mode-gadget)

  ;; initialize radio buttons
  (g-value test-build-obj :value)
  (s-value test-build-obj :value "build")

  ;; initialize selection mode
  (g-value selection-mode-gadget :value)
  (s-value selection-mode-gadget :value "top-level objects")
  (s-value *selection-info* :leaf nil)

#|
  (create-instance 'EDITOR-MENU garnet-gadgets:text-button-panel
         (:constant '(t :except :items))
	 (:left 10) (:top 10)
	 (:shadow-offset 5) (:gray-width 3)
	 (:final-feedback-p NIL)
	 (:build-p t)
	 (:rank-margin 8)
	 (:selection-function 'editor-menu-handler)
	 (:items '(
		"filling style" "line style" "draw function" "name object"
		"list properties" "text properties" 
		"make instance" "make copy"
		"parameters" "make aggregadget" "ungroup"
		"save gadget" "load gadget" "add gadget" "line constraints"
		"bring to front" "send to back" "interactors" "c32"
		"test" "clear workspace" "delete object" "delete window" 
		"quit")))
  
  (opal:add-component EDITOR-MENU-AGG EDITOR-MENU)
|#

  (opal:update EDITOR-WIN))

(defun editor-menu-do-stop ()
  (when (boundp 'EDITOR-WIN) (opal:destroy EDITOR-WIN)))
