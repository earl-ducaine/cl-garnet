;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Multifont-Gadget
;;;
;;;  Features and operation of Multifont-Gadget:
;;;     1)  A Multifont-Gadget is a string editor capable of handling multiple
;;;         lines and multiple fonts within a line.  
;;;     2)  To select a portion of text, click and drag the mouse over the
;;;         portion to be selected.
;;;     3)  To move the cursor, click the mouse over the position you want
;;;         the cursor to be.
;;;     4)  To change the current font, use the function key f2 and f3 to
;;;         change italics and bold, f4 and f5 to make the font smaller or
;;;         larger, or f6, f7, or f8 to change to font to fixed, serif, or
;;;         sans-serif.
;;;     5)  Many other operations are available by retrieving the multifont
;;;         object within the multifont-gadget (slot :multifont) and performing
;;;         multifont procedures upon it.
;;;
;;;  Customizable slots:
;;;     1)  Left, top
;;;     2)  Word-Wrap-P -- whether or not to wrap words at :text-width
;;;     3)  Text-Width -- the size to wrap words at if word wrap is on
;;;     4)  Strings -- text to put in box when it is started
;;;     5)  Fill-Background-P -- whether or not to fill background of words
;;;
;;;     NOTE:  Only one multifont-gadget per window permitted as the
;;;            interactors involved grab keystrokes from the window.  More
;;;            than one multifont-gadget in a window will cause keystrokes to
;;;            be received by all multifont-gadgets in the window.
;;;
;;;            This module requires several files which are not loaded by
;;;            default.  Be sure to load the multifont-loader from the Opal
;;;            directory before loading this file, or use the multifont-
;;;            gadget-loader to load all of these files.
;;;
;;;  Multifont Gadget demo:
;;;     This module contains a function which creates a window and a multifont
;;;     gadget.  To run it, enter (gg:multifont-gadget-go).  To stop, enter
;;;     (gg:multifont-gadget-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Richard McDaniel

;;; CHANGE LOG
;;;  2/03/93 Andrew Mickish - :strings ---> :initial-text
;;; 12/14/92 Andrew Mickish - Added type and parameter declarations
;;; 08/20/92 Andrew Mickish - Added kr-send of :selection-function
;;; 04/28/92 Andrew Mickish - Moved load of multifont-loader into multifont-
;;;            gadget-loader.
;;; 03/05/92 RGM - Started
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Multifont-Gadget)))

(create-instance 'MULTIFONT-GADGET opal:aggregadget
   :declare ((:parameters :left :top :initial-text :fill-background-p
			  :word-wrap-p
			  :text-width :stop-event :selection-function)
	     (:type ((or list string) :initial-text)
		    (kr-boolean :fill-background-p :word-wrap-p)
		    ((integer 0) :text-width)
		    ((or null keyword character) :stop-event)
		    ((or null function symbol) :selection-function)))
   (:left 0)
   (:top 0)
   (:initial-text (list ""))
   (:fill-background-p NIL)
   (:word-wrap-p nil)
   (:text-width 300)
   (:stop-event NIL)
   (:selection-function NIL)
   (:parts
      `((:multifont ,opal:multifont-text
           (:left ,(o-formula (gvl :parent :left)))
           (:top ,(o-formula (gvl :parent :top)))
           (:initial-text ,(o-formula (gvl :parent :initial-text)))
           (:fill-background-p ,(o-formula (gvl :parent :fill-background-p)))
           (:word-wrap-p ,(o-formula (gvl :parent :word-wrap-p)))
           (:text-width ,(o-formula (gvl :parent :text-width)))
        )
       )
   )
   (:interactors
      `((:text-inter ,inter:focus-multifont-textinter
           (:window ,(o-formula (gv-local :self :operates-on :window)))
           (:obj-to-change ,(o-formula (gvl :operates-on :multifont)))
	   (:stop-event ,(o-formula (gvl :operates-on :stop-event)))
	   (:final-function
	    ,#'(lambda (an-inter string-object event new-string x y)
		 (declare (ignore string-object event x y))
		 (let ((gadget (g-value an-inter :operates-on)))
		   (kr-send gadget :selection-function
			    gadget new-string))))
        )
        (:mouse-inter ,inter:selection-interactor
           (:window ,(o-formula (gv-local :self :operates-on :window)))
           (:focus-interactor ,(o-formula (gvl :operates-on :text-inter)))
           (:start-where ,(o-formula (list :in (gvl :operates-on :multifont))))
        )
       )
   )
)

#+garnet-test
(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(multifont-gadget-go  multifont-gadget-stop)))

#+garnet-test
(defun multifont-gadget-go ()
   (create-instance 'multifont-gadget-win inter:interactor-window
      (:title "Multifont-gadget Test")
      (:left 340)
      (:top 280)
   )
   (create-instance 'multifont-gadget-top opal:aggregate)
   (s-value multifont-gadget-win :aggregate multifont-gadget-top)
   (create-instance 'gadget-instance garnet-gadgets:multifont-gadget
      (:word-wrap-p t)
      (:fill-background-p t)
      (:text-width (o-formula (gv multifont-gadget-win :width)))
      (:initial-text (list "Here is a multifont gadget" ""
			   "F1: exit window"
			   "F2: toggle italics"
			   "F3: toggle bold"
			   "F4: text smaller"
			   "F5: text bigger"
			   "F6: make text fixed"
			   "F7: make text serif"
			   "F8: make text sans-serif" ""))
   )
   (opal:add-component multifont-gadget-top gadget-instance)
   (opal:update multifont-gadget-win)
#-(and cmu (not mp)) (inter:main-event-loop)
)

#+garnet-test
(defun multifont-gadget-stop ()
  (opal:destroy multifont-gadget-win)
)

