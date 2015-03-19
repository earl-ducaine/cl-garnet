;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-SEQUENCE; Base: 10 -*-
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


;;; This demo shows off having one interactor start another one
;;;
;;; Written by Brad Myers
;;;;
;;;; Had some trouble getting this to work (FMG)


(in-package :DEMO-SEQUENCE)

(defparameter *test-debug* t)

;;-----------------------------------
(defparameter bigfnt (create-instance NIL opal:font
			   (:size :large)
			   (:family :serif)))

(defun make-menu (top-agg names x y) 
  (let (menu-name shadow all-items feedback frame prev-item this-item)

    (create-instance 'menu-name opal:aggregate
      (:left x)(:top y)(:width 500) (:height 500)
      (:window (g-value top-agg :window))
      (:overlapping T)) ;**NIL**

    (create-instance 'shadow opal:rectangle
      (:left (o-formula (gvl :parent :left)))
      (:top (o-formula (gvl :parent :top)))
      ;; width and height set later
      (:name :shadow)
      (:filling-style opal:black-fill)
      (:line-style NIL))

    (create-instance 'all-items opal:aggregate
      (:overlapping T) ;**NIL**
      (:name :all-items))

    (create-instance 'frame opal:rectangle
      (:filling-style opal:white-fill)
      (:name :frame)
      (:line-style opal:line-2)
      (:shadow shadow)
      (:left (o-formula (+ 5 (gvl :shadow :left)) 0))
      (:top (o-formula (+ 5 (gvl :shadow :top)) 0))
      (:width (o-formula (gvl :shadow :width) 0))
      (:height (o-formula (gvl :shadow :height) 0)))

    (opal:add-components top-agg menu-name)

    (opal:add-components menu-name shadow frame all-items)

    (create-instance 'feedback opal:rectangle
      (:name :feedback)
      (:draw-function :xor)
      (:fast-redraw-p t)
      (:filling-style opal:black-fill)
      (:line-style NIL)   ; no outline
      (:visible (o-formula (gvl :obj-over)))
      (:left (o-formula (1- (gvl :obj-over :left)) 0))
      (:top (o-formula (1- (gvl :obj-over :top)) 0))
      (:width (o-formula (+ 2 (gvl :obj-over :width)) 0))
      (:height (o-formula (+ 2 (gvl :obj-over :height)) 0)))

    (opal:add-components menu-name feedback)
    
    ; link together
    (s-value menu-name :feedback feedback)
    (s-value menu-name :all-items all-items)
    
    ; create items
    (dolist (label names)
      (setq this-item
	    (create-instance NIL opal:text
	      (:string label)
	      (:font bigfnt)
	      (:prev-item prev-item)
	      (:frame frame)
	      (:left (o-formula
		       (+ (gvl :frame :left)
			  (floor (- (gvl :frame :width)
				    (gvl :width)) 2)) 0))
	      (:top (if prev-item (o-formula
				    (+ (gvl :prev-item :top)
				       (gvl :prev-item :height)
				       4) 0)
			(o-formula (+ 4 (gvl :frame :top)) 0)))))
      (opal:add-components all-items this-item)
      (setf prev-item this-item))

    (s-value shadow :height (formula 
			     `(let ((toth 0))
				(when *test-debug* (format T "menu1 height~%"))
				(dolist (item (g-value ',all-items
						       :components))
				  (setf toth (+ 4 toth (gv item :height))))
				(+ 6 toth)) 0))
    (s-value shadow :width (formula 
			    `(let ((maxw 0))
			       (when *test-debug* (format T "menu1 width~%"))
			       (dolist (item  (g-value ',all-items
						       :components))
				 (setf maxw (MAX maxw (gv item :width))))
			       (+ 8 maxw)) 0))
    
    menu-name))
  

(defun make-fixed-menu1 (agg names x y)
  (let (menu outline-feedback all-items )
    (setf menu (make-menu agg names x y))
    (setf all-items (get-value menu :all-items))
    (create-instance 'outline-feedback opal:rectangle
      (:name :outline-feedback)
      (:draw-function :xor)
      (:fast-redraw-p t)
      (:all-items all-items)
      (:visible (o-formula (gvl :all-items :selected)))
      (:left (o-formula (- (gvl :all-items :selected :left) 2) 0))
      (:top (o-formula (- (gvl :all-items :selected :top) 2) 0))
      (:width (o-formula (+ 2 (gvl :all-items :selected :width)) 0))
      (:height (o-formula (+ 4 (gvl :all-items :selected :height)) 0)))
    (opal:add-components menu outline-feedback)
    (when *test-debug* (format T "done menu1= ~s~%" menu))
    menu))


;;-----------------------------------



;;-----------------------------------

(defparameter aggmenu NIL)
(defparameter aggmain NIL)
(defparameter aggnewobj NIL)
(defvar vpmain NIL)
(defvar vpmenu NIL)
(defvar menu3 NIL)
(defvar inter3 NIL)
(defvar newobjinter NIL)
(defvar textinter NIL)
(defvar text-feedback-obj NIL)
(defparameter second-menu-string NIL)
(defparameter *glo-tone* 0)
(defparameter twop-rect NIL)
(defparameter twop-line NIL)

(defparameter pop-menu NIL)
(defparameter pop-inter NIL)

;;-----------------------------------

(defun Special-Stop-Action (an-interactor final-obj-over)
  (Call-Prototype-Method an-interactor final-obj-over)
  (let ((which-item (g-value final-obj-over :string)))
    (when *test-debug* (format T "Got ~s~%" which-item))
    (cond ((equal which-item "Line")
	   ; start up the new line interactor
	   (s-value newobjinter :line-p t)
	   (inter:start-interactor newobjinter T))
	  ((equal which-item "Rectangle")
	   ; start up the new rectangle interactor
	   (s-value newobjinter :line-p NIL)
	   (inter:start-interactor newobjinter T))
	  ((equal which-item "Text")
	   ; start up the new text
	   (inter:start-interactor textinter T))
	  ((equal which-item "Menu")
	   (s-value pop-menu :visible T)
	   (s-value pop-menu :left (inter::event-x inter::*current-event*))
	   (s-value pop-menu :top (inter::event-y inter::*current-event*))
	   (inter:start-interactor pop-inter NIL))
	  (T (format T "Got a ~s~%" which-item)))))
	  
(defparameter texty 5)

(defun Create-text-obj (feedback)
  (when *test-debug* (format T "Creating new text object~%"))
  (let (obj)
    (create-instance 'obj opal:text
      (:left (g-value feedback :left))
      (:top (g-value feedback :top))
      (:font (g-value feedback :font))
      (:string (g-value feedback :string)))
    (opal:add-component aggnewobj obj)
    (incf texty (g-value obj :height))
    (s-value text-feedback-obj :top texty)
    (s-value (g-value menu3 :all-items) :selected NIL) 
    obj))

(defun Create-New-Obj (interactor point-list)
  (declare (ignore interactor))
  (let (obj)
    (if (g-value newobjinter :line-p)
	(progn
	  (when *test-debug* (format T "creating line; ~s~%" point-list))
	  (create-instance 'obj opal:line
	    (:x1 (first point-list))
	    (:y1 (second point-list))
	    (:x2 (third point-list))
	    (:y2 (fourth point-list))
	    ))
	(progn
	  (when *test-debug* (format T "creating rect; ~s~%" point-list))
	  (create-instance 'obj opal:rectangle
	    (:left (first point-list))
	    (:top (second point-list))
	    (:width (third point-list))
	    (:height (fourth point-list))
	    (:filling-style
	     (case *glo-tone*
	       (0 opal:white-fill)
	       (1 opal:light-gray-fill)
	       (2 opal:gray-fill)
	       (3 opal:dark-gray-fill))))
	  (setq *glo-tone* (if (eq *glo-tone* 3) 0 (1+ *glo-tone*)))))
	  
    (opal:add-component aggnewobj obj)
    (s-value (g-value menu3 :all-items) :selected NIL)
    (when *test-debug* (format T "created ~s~%" obj))
    obj))

;;-----------------------------------

(defun create-pop-menu (stringlist)

  (setq pop-menu (make-fixed-menu1 aggmenu stringlist 0 0))
  (s-value pop-menu :visible NIL)

  (create-instance 'pop-inter inter:menu-interactor 
    (:start-where NIL)
    (:feedback-obj (g-value pop-menu :feedback))
    (:running-where
     `(:element-of ,(g-value pop-menu :all-items)))
    (:window vpmain)
    (:abort-action
     #'(lambda (an-interactor obj-over)
	 (call-prototype-method an-interactor obj-over)
	 ;; (s-value pop-vp :visible NIL)
	 (s-value pop-menu :visible NIL)
	 (s-value (g-value menu3 :all-items) :selected NIL)))
    (:stop-action
     #'(lambda (an-interactor obj-over)
	 (when *test-debug* (format T "stop on ~s~%" obj-over))
	 ;; (s-value pop-vp :visible NIL)
	 (s-value second-menu-string :mode (g-value obj-over :string))
	 (call-prototype-method an-interactor obj-over)
	 (s-value (g-value menu3 :all-items) :selected NIL)
	 (s-value pop-menu :visible NIL)
	 (s-value (g-value pop-menu :all-items) :selected NIL)
	 ))))
		     
;;-----------------------------------


(defun do-stop ()
  (opal:destroy vpmain)
  (setf vpmain nil))

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (create-instance 'vpmain inter:interactor-window
    (:height 360) (:width 400)(:top 100)(:left 100)
    (:title "GARNET SEQUENCING")
    (:double-buffered-p double-buffered-p)
    (:icon-title "Sequence"))

  (opal:update vpmain)

  (setq vpmenu vpmain)
  (s-value vpmain :aggregate 
	   (create-instance 'aggmenu opal:aggregate (:overlapping T)))

  (create-instance 'aggmain opal:aggregate
    (:left 0)(:top 0) (:width 400)(:height 360)
    (:overlapping T))

  ;;; the new objects will be under the menu
  (opal:add-component aggmenu
		      (create-instance 'aggnewobj opal:aggregate
			(:left 0)(:top 0)
			(:width 400)(:height 360)
			(:overlapping T)))

  (setq menu3
	(make-fixed-menu1 aggmenu
			  '("Line" "Rectangle" "Menu" "Text") 5 5))

  (create-instance 'second-menu-string opal:text
    (:string (o-formula (concatenate 'string "Secondary mode is: "
				     (gvl :mode))))
    (:mode "*NONE*")
    (:font bigfnt)
    (:left 5)
    (:top 325))

  (opal:add-component aggmenu second-menu-string)
  (opal:add-component aggmenu aggmain)

  (setq inter3
	(create-instance 'main-menu-inter inter:menu-interactor 
	  (:feedback-obj (g-value menu3 :feedback))
	  (:start-where
	   `(:element-of ,(g-value menu3 :all-items)))
	  (:window vpmenu)
	  (:stop-action `Special-Stop-Action)))
  (opal:add-component aggmain
		      (create-instance 'twop-rect opal:rectangle
			(:draw-function :xor)
			(:fast-redraw-p t)
			(:name "Interim Rect feedback")
			(:left (o-formula (first (gvl :box))))
			(:top (o-formula (second (gvl :box))))
			(:width (o-formula (third (gvl :box))))
			(:height (o-formula (fourth (gvl :box))))
			(:visible NIL)
			(:box '(0 0 0 0))
			(:line-style opal:dashed-line)))
  (opal:add-component aggmain
		      (create-instance 'twop-line opal:line
			(:draw-function :xor)
			(:fast-redraw-p t)
			(:name "Interim Line feedback")
			(:x1 (o-formula (first (gvl :points))))
			(:y1 (o-formula (second (gvl :points))))
			(:x2 (o-formula (third (gvl :points))))
			(:y2 (o-formula (fourth (gvl :points))))
			(:visible NIL)  
			(:points '(0 0 0 0))
			(:line-style opal:dashed-line)))

  (setq newobjinter
	(create-instance 'two-p-inter inter:Two-Point-Interactor
	  (:Window vpmain)
	  (:start-where NIL) ; make sure doesn't start up on its own
	  (:running-where `(:in ,aggmain))
	  (:start-event :leftdown)
	  (:stop-event :any-mousedown)
	  (:abort-event :any-keyboard)
	  (:final-function 'Create-New-Obj)
	  (:line-p T)
	  (:feedback-obj (formula `(if (gvl :line-p) twop-line twop-rect)))
	  (:Min-width NIL)
	  (:Min-height NIL)
	  (:abort-if-too-small NIL)
	  (:abort-action #'(lambda (an-interactor)  ; turn off the outline box
			     (call-prototype-method an-interactor)
			     (s-value (g-value menu3 :all-items) :selected NIL)))
	  ))

  (opal:add-component aggmain
		      (create-instance 'text-feedback-obj
			  opal:cursor-text (:string "")
			  (:cursor-index nil)
			  (:visible NIL)
			  (:left 175)
			  (:top texty)))

  (setq textinter
	(create-instance 'text-inter inter:text-interactor
	  (:feedback-obj text-feedback-obj)
	  (:start-where NIL)  ; don't start by self; wait to be started explicitly
	  (:window vpmain)
	  (:abort-event :control-\g)
	  (:stop-event '(:any-mousedown #\RETURN))
	  (:start-action #'(lambda (inter new-obj start-event)
			     (s-value (g-value inter :feedback-obj)
				      :visible T)
			     (call-prototype-method inter new-obj
						    start-event)))
	  (:abort-action #'(lambda (an-interactor obj ev) ;turn off outline box
			     (call-prototype-method an-interactor obj ev)
			     (s-value (g-value menu3 :all-items) :selected NIL)))
	  (:stop-action
	   #'(lambda (an-interactor obj-over stop-event)
	       ;; call parent to turn off feedback object visibility
	       (call-prototype-method an-interactor obj-over stop-event)
	       (let* ((feedback (g-value an-interactor :feedback-obj)))
		 (create-text-obj feedback)
		 (s-value feedback :string ""))))))

  (create-pop-menu '("First" "Second" "Third" "Fourth" "Fifth" "Sixth" "Seventh"))

  (opal:update vpmenu)
  (opal:update vpmain)
  (Format T "~%Demo-Sequence:
  Press and release on a menu item with the left button to start getting one
  of the items.
  After selecting line or rectangle, start moving mouse to get the new object.
  After selecting on text, start typing.
  After selecting menu, make a selection in the secondary, pop-up menu.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )
