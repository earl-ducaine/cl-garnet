;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; This file contains code that implements the move/grow interactor
;;; dialog box

(in-package "EDITOR")

(defun movegrow-dialog-box ()
  (create-dialogbox 'movegrow-db `(movegrow-agg 
	   ((inter-name
	     ("Interactor Name:") 
	     (:stop-action #'inter-name-stop-action)
	     (:text-string
	      (formula `(gv ',movegrow-inter-info :inter-name)))
	     (:stop-event '(:leftdown #\RETURN))
	     (:is-a framed-text-with-title))
	    (start-where
	     (("One of this Aggregate"
	       (:is-a check-box-with-text)
	       (:text-string 
		(formula `(gv ',movegrow-inter-info :start-where-string))))
	      ("Single Item:"
	       (:is-a check-box-with-text)
	       (:text-string 
		(formula `(gv ',movegrowinter :start-where-string)))))
	     (:stop-action #'start-where-stop-action)
	     (:selected-obj (formula `(gv ',movegrow-inter-info :start-where)))
	     (:label "Start Where"))
	    (obj-to-change
	     ("Object to Change (if different from start where)")
	     (:is-a check-box-with-text)
	     (:text-string
	      (formula (gv ',movegrow-inter-info :obj-to-change)))
	     (:stop-action #'obj-to-change-stop-action))
	    (interim-feedback
	     (("Feedback Object"
	       (:is-a check-box-with-text)
	       (:text-string 
		(formula `(gv ',movegrowinter :feedback-obj-string))))
	      ("Change Original"
	       (:is-a radio-button))
	      ("Default"
	       (:is-a radio-button)))
	     (:stop-action #'movegrow-interim-feedback-stop-action)
	     (:selected-obj (formula `(gv ',movegrowinter :feedback-obj))))
	    (inter-type
	     ("Grow" "Move" "<Formula>")
	     (:stop-action #'movegrow-inter-type-stop-action)
	     (:selected-obj (formula `(gv ',movegrowinter :inter-type)))
	     (:is-a radio-button))
	    (start-event
	     ((modifiers ("shift" "control" "meta" "any")
			(:is-a check-box)
			(:stop-action #'modifiers-start-evt-stop-action)
			(:choosable :multiple)
			(:selected-obj (formula 
			   `(let ((objlist nil))
			      (dolist (modifier '(:shift-mod-start-evt
						  :control-mod-start-evt
						  :meta-mod-start-evt
						  :any-mod-start-evt))
				(push (gv ',movegrowinter modifier)
				      objlist))
			      objlist)))
			(:label "Modifiers"))
	      (mouse ("left" "middle" "right" "any")
		    (:is-a check-box)
		    (:stop-action #'mouse-start-evt-stop-action)
		    (:choosable :multiple)
		    (:selected-obj (formula 
			   `(let ((objlist nil))
			      (dolist (button '(:left-down-start-evt
						:middle-down-start-evt
						:right-down-start-evt
						:any-down-start-evt))
				(push (gv ',movegrowinter button)
				      objlist))
			      objlist)))
		    (:label "Mouse"))
	      (key 
	       (("key"
		  (:text-string 
		   (formula `(gv ',movegrowinter :key-start)))
		  (:is-a framed-text-with-title))
		("any"
		 (:is-a radio-button))
		("other"
		 (:is-a radio-button)))
	       (:selected-obj (formula `(gv ',movegrowinter :key-start)))
	       (:stop-action #'key-start-evt-stop-action)))
	     (:choosable :none)
	     (:label "Start-Event:"))
	    (grow-parameters
	     ("Width" "Height" "<Formula>")
	     (:is-a check-box)
	     (:choosable :multiple)
	     (:stop-action #'grow-parameters-stop-action)
	     (:selected-obj (formula 
			     `(let ((objlist nil))
				(dolist (dimension '(:grow-width
						     :grow-height
						     :grow-formula))
				  (push (gv ',movegrowinter dimension)
					objlist))
				objlist))))
	    (move-parameters
	     ("Left" "Top" "<Formula>")
	     (:is-a check-box)
	     (:choosable :multiple)
	     (:stop-action #'move-parameters-stop-action)
	     (:selected-obj (formula 
			     `(let ((objlist nil))
				(dolist (dimension '(:move-width
						     :move-height
						     :move-formula))
				  (push (gv ',movegrowinter dimension)
					objlist))
				objlist))))
	    (where-attach
	     ("NW" "N" "NE" "E" "SE" "S" "SW" "W" "Wherehit" "<Formula>")
	     (:stop-action #'where-attach-stop-action)
	     (:label "Attach-Point:")
	     (:selected-obj (formula `(gv ',movegrowinter :where-attach)))
	     (:is-a radio-button)))
	   (:stop-group '(("New" "Abort" "OK" "Remove" "Details" "Extra Actions")
			  (:stop-action #'movegrowinter-stop-action)
			  (:is-a button)))
	   (:choosable :none)
	   (:label "Move/Grow Interactor"))))
  
