;;;; -*- Mode: Lisp; package: sx -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : pop-up-menu.lisp
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Jul 13 18:26:30 1990
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sun Jun 16 17:53:20 1991
;;;; Update Count    : 188
;;;; 
;;;; PURPOSE
;;;; 	provides a pop-up-menu for the sx, based on garnet.
;;;; TABLE OF CONTENTS
;;;;
;;;;	II.	Popup-soar-status-window
;;;; 
;;;; (C) Copyright 1990, Frank Ritter, all rights reserved.
;;;; The material in this file is made available according to the
;;;; terms of the GNU LGPL, modified by the Lisp LGPL preamble.  Both
;;;; of those documents are available in the doc subdirectory of the
;;;; Garnet distribution.

;;;; 21Jun04 - per agreement with Frank Ritter, the license terms of
;;;; this file are clarified to be LLGPL. [2004/06/21:rpg]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(eval-when (load eval compile)
 ;; make sure to avoid soarsyntax changes
  #+soar5(and (soarsyntax) (soarresetsyntax))
  #+soar5(in-package "SX"))


(defun create-my-pop-up-menu 
     (&key (double-buffered-p default-double-buffer-p)
           items click-window
	   (menu-name 'popup-menu-window)
	   (title "POPUP-MENU")
	   (icon-title "POPUP")
	   (menu-event :ANY-mouseDOWN)
	   (start-event :ANY-mouseDOWN) )  ;had been LEFT
  "create a pop-up-menu MENU-NAME of ITEMS, with TITLE and ICON-TITLE,
bring up on START-EVENT in CLICK-WINDOW, DOUBLE-BUFFERED-P iff T."
  ;; remembers what you previously selected and puts you there
  ;; clips to keep menu on the screen
  #+soar5(soarresetsyntax)
  (let ((sub-menu (intern (format nil "~s-GADGET" menu-name)))
	(menu-inter (intern (format nil "~s-INTER" menu-name))) )
  (if (and (boundp sub-menu) (schema-p (eval sub-menu)))
      (progn (opal:destroy (g-value (eval sub-menu) :selector))
	     (opal:destroy (eval sub-menu))))
  (if (and (boundp menu-name)
	   (schema-p menu-name))
      (opal:destroy menu-name))
  (create-instance menu-name inter:interactor-window
   (:double-buffered-p double-buffered-p)
   (:left 0) (:top 30) (:width 210) (:height 170)
   (:visible nil)
   (:aggregate (create-instance nil opal:aggregate
			        (:overlapping NIL)))
   (:title title)
   (:icon-title icon-title))
  (setf pop-up-menu-agg 
        (g-value (eval menu-name) :aggregate))
  (create-instance sub-menu garnet-gadgets:menu
    (:left 0)    (:top 0)    (:shadow-offset 0)
    (:item-font font-fixed-bold-medium)
    (:title nil)
    (:V-spacing -1) ; default appears to be 0
    (:items items)
    (:interactors
     `((:selector ,inter:menu-interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))  
         (:start-where ,(o-formula (list :element-of
					(gvl :operates-on
					      :menu-item-list))))
	 (:running-where ,(o-formula (list :element-of
					   (gvl :operates-on
						:menu-item-list))))
	 (:start-event ,menu-event)
     	 (:how-set NIL)            
         (:menu ,(o-formula (gvl :operates-on :window))) ;store the window
	 (:feedback-obj ,(o-formula (gvl :operates-on :feedback)))
       	 (:abort-action
	    ,#'(lambda (an-interactor obj-over)
		 (call-prototype-method an-interactor obj-over)
                 (s-value (g-value an-interactor :menu) :visible NIL)))  
         (:stop-action
          ,#'(lambda (interactor obj-under-mouse)
	      (let ((action (g-value obj-under-mouse :action))
		    (gadget (g-value interactor :operates-on))
		    (string (g-value obj-under-mouse :string)))
		(s-value (g-value gadget :feedback) :obj-over NIL)
		(s-value gadget :value-obj obj-under-mouse)
		;; Global function for all items
		(kr-send gadget :selection-function gadget string)
                (s-value (g-value interactor :menu) :visible nil)
		(opal:update (g-value interactor :menu))
        	;; Local function assigned to item
		(when action
		  (funcall action gadget string))
                (print-sx-prompt :newline t))))
            )))   )
  (eval (list 'opal:add-components 'pop-up-menu-agg
       sub-MENU))
  (eval (list 'opal:update menu-name))
  ;; resize menu to fit
  (eval `(s-value ,menu-name :width (g-value ,sub-menu :width)))
  (eval `(s-value ,menu-name :height (g-value ,sub-menu :height)))
  ;; now create inter
  (if (and (boundp menu-inter)
	   (schema-p menu-inter))
      (opal:destroy menu-inter))
  ;; create inter to popup the menu
  (if start-event
      (create-instance menu-inter inter:popup-interactor
         (:start-where t)
         (:start-event start-event)
         (:window click-window)
         (:final-function
          `(lambda (an-interactor object-being-changed)
            (popup-window ,menu-name
             ;; this bit of magic here puts you over last item
          	:y-offset
         	(let* ((previous-object (g-value ,sub-menu :selector
                                              :remembered-last-object)) )
                  (if previous-object ;you've been there before
                      (- (g-value ,menu-name :height)
     		         (g-value previous-object :base-top)
                         (ihalf (g-value ,menu-name :height))
                          ) ;popup's correction
              	      0)))))
         (:continuous nil)
         (:running-where t)))
   ))


;;;
;;;	IV.	Popup-window
;;;

;; these are good for pmaxen, but can we get a more principle values?
;; coming in version 1.4 ...
(defconstant max-display-width 1000)
(defconstant max-display-height 800)

(defun popup-window (possible-window &optional &key
		     (pop-to-last-mouse t)
		     (y-offset 0) (x-offset 0))
  "de-iconify a window and move it to front of display list.
If it is common thing with a window (so far: scrolling-window-with-bars),
gracefully pop it instead."
  (let ( (window (cond ( (null possible-window)
                          (format t "popup-window passed ~s." possible-window)
			  (return-from popup-window nil))
                       ( (is-a-p possible-window inter:interactor-window)
			 possible-window)
                       ( (is-a-p possible-window gg:scrolling-window-with-bars)
			 (g-value possible-window :outer-window))
                       ( (is-a-p possible-window (g-value possible-window :window))
			 (g-value possible-window :window))
                       (t (format t "popup-window passed ~s." possible-window)
			  (return-from popup-window nil)) ))   )
  (s-value window :visible t)
  (if pop-to-last-mouse
      (progn 
        (s-value window :left (inter:clip-and-map
			  (- (+ x-offset (inter:event-x inter:*current-event*))
			     (ihalf (g-value window :width)))
			  0 (- max-display-width (g-value window :width)) ))
        (s-value window :top (inter:clip-and-map
			 (- (+ y-offset (inter:event-y inter:*current-event*))
			    (ihalf (g-value window :height)))
			 0 (- max-display-height (g-value window :height))))
	))
  ;; this deiconifies too
  (setf (xlib:window-priority (g-value window :drawable))
              :above)
  (s-value window :visible t)
  ;; this may be redundent...
  (kr-send window :update-yourself window)
  (opal:update window)  ))
