;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This file lists all the commands that the Garnet tour has you type.  This is
;;  useful as a quick reference if you need to restart due to an error.  If you
;;  have this document in a window on the screen, you can use the X cut buffer
;;  to move text from below into your Lisp window.   These commands by
;;  themselves are in /afs/cs/user/bam/garnet/ic/tourcommands.lisp.  Note: do
;;  not just load tourcommands, since it will run all the demos and quickly
;;  quit; just copy the commands one-by-one from the file using the X cut buffer.

;;  This listing does not show the prompts or Lisp's responses to these commands.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  *** Begin here to load the Garnet software.  You will have to replace xxx
;;      with your directory path to Garnet::

(load ":xxx:garnet:garnet-loader")
(garnet-load "demos-src:tour")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  *** Start here after Garnet and the tour software is loaded:

(create-instance 'mywindow inter:interactor-window)
(opal:update mywindow)

(create-instance 'myagg opal:aggregate)
(s-value mywindow :aggregate myagg)
(gv MYWINDOW :aggregate)
(create-instance 'myrect moving-rectangle) ;this is in the USER package
(opal:add-component myagg myrect)
(opal:update mywindow)

(s-value myrect :filling-style opal:gray-fill)
(opal:update mywindow)

(create-instance 'mytext opal:text (:left 200)(:top 80)
	(:string "Hello World"))
(opal:add-component myagg mytext)
(opal:update mywindow)

(s-value mytext :top 40)
(opal:update mywindow)

(s-value mytext :top (o-formula (gv myrect :top)))
(opal:update mywindow)

(s-value myrect :top 50)
(opal:update mywindow)

(create-instance 'mymover inter:move-grow-interactor
	(:start-where (list :in myrect))
	(:window mywindow))

#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished

(create-instance 'mytyper inter:text-interactor
	(:start-where (list :in mytext))
	(:window mywindow)
	(:start-event :rightdown)
	(:stop-event :any-mousedown))
#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished

(s-value mytext :justification :right)
(opal:update mywindow)

(s-value mytext :justification :center)
(opal:update mywindow)

(create-instance 'mybuttons garnet-gadgets:radio-button-panel
		 (:Items '(:Center :left :right))
		 (:left 350)(:top 20))
(opal:add-component myagg mybuttons)
(s-value mybuttons :value :left)          {\it ;do this before attaching to text}
(opal:update mywindow)
#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished


(s-value mytext :justification (o-formula (gv mybuttons :value)))
(opal:update mywindow)
#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished

(s-value mybuttons :direction :horizontal)
(opal:update mywindow)

(s-value mybuttons :direction :vertical)
(opal:update mywindow)

(create-instance 'myslider gg:v-slider (:left 10)(:top 20))
(opal:add-component myagg myslider)
(opal:update mywindow)
#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished

(s-value myrect :filling-style (o-formula
				(opal:halftone (gv myslider :value))))
(opal:update mywindow)
#-(or cmu allegro lucid lispworks)  ;only do this if your Lisp is NOT a
(inter:main-event-loop)             ;recent version of CMU, Allegro, or Lucid.
                                    ;type F1 or ^C to exit when finished

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  *** Start here (after loading the software) to just get Othello to run.
;;      None of the previous is needed for Othello or the editor (except for the
;;      software loading, of course).

(start-othello)

(garnet-load "demos:garnetdraw")
(garnetdraw:do-go)
