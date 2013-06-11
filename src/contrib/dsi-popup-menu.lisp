;;;; -*- Mode: Soar -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : soar-popup-menu.lisp
;;;; Author          : Frank Ritter
;;;; Created On      : Sun Jun 16 17:49:53 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sun Jun 16 17:53:54 1991
;;;; Update Count    : 1
;;;; Soar Version    : 5.2
;;;; TAQL Version    : 3.1.3
;;;; 
;;;; PURPOSE
;;;; 	The DSI's popup-menu.
;;;; TABLE OF CONTENTS
;;;;	I.  Future home of a pull-down menu
;;;; 
;;;; (C) Copyright 1991, Carnegie Mellon University, all rights reserved.
;;;; The material in this file is made available according to the
;;;; terms of the GNU LGPL, modified by the Lisp LGPL preamble.  Both
;;;; of those documents are available in the doc subdirectory of the
;;;; Garnet distribution.
;;;; The above was added per correspondence with Frank Ritter, and
;;;; addresses IP issues with him.  I assume that any such issues with
;;;; CMU are addressed by their open-sourcing of Garnet. [2004/06/21:rpg]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(eval-when (load eval compile)
 ;; make sure to avoid soarsyntax changes
  #+soar5(and (soarsyntax) (soarresetsyntax))
  #+soar5(in-package "SX"))



;;;
;;;	I.  Future home of a pull-down menu
;;; It's pop-up 'till then.


#|
SX		RUN		INSPECT		EDIT (prod)
==============================================================
about		d		status window	Excise
help		run		cs		print
save-win-conf	init		pgs		pbreak
		restart		sp		matches
quit		pop-goal	PS Graph trace	trace production
		excise-chunks

Status window will take up a lot of functionality, showing what the
state is, and also letting users modify the status.

|#


;;;
;;; 	III. create-gtw-popup-menu
;;;

(defun create-gtw-popup-menu ()
 (create-my-pop-up-menu
  :title "Soar Interface MENU"
  :double-buffered-p default-double-buffer-p
  :menu-name 'gtw-popup-menu-window
  :items '(
  ("1 Decision                d | N" (lambda (x y) (d 1)))
  ("1 Elaboration              r | " (lambda (x y) (run 1)))
  ("Set Macrocycle  |(set-macrocycle)" (lambda (x y)
               				 (popup-window macrocycle-window
     					       :pop-to-last-mouse nil)))
  ("Macrocycle             m,SPC| :m" (lambda (x y) (macrocycle)))
  ("-------------------------------" (lambda (x y)
				       (popup-window gtw-popup-menu-window
						     :pop-to-last-mouse nil)))
  ("Examine selected item      e|:e" (lambda (x y) (popup-pscm-examiner)))
  ("Init Soar               i|:init" (lambda (x y) (init-soar)))
  ("Continuous Match set      |:cms" (lambda (x y) (continuous-ms)))
  ("-------------------------------" (lambda (x y)
				       (popup-window gtw-popup-menu-window
						     :pop-to-last-mouse nil)))
  ("Set parameters                 " (lambda (x y)
     				       (popup-window soar-status-window
						     :pop-to-last-mouse nil)))
  ("Set learning           |(learn)" (lambda (x y)
     				       (popup-window learning-window
     					      :pop-to-last-mouse nil)))
  ("Write N1 trace                 " (lambda (x y) (write-n1-trace)))
  ("Take snapshot of screen  |:snap" (lambda (x y) (sx-snapshot :graphic t)))
  ("Toggle taking protocol         " (lambda (x y)
     				       (toggle-taking-protocol :graphic t)))
  ("Load TAQL          |(load-taql)" (lambda (x y) (load-taql)) )
  ("Static display sub-menu        " (lambda (x y) (popup-window static-menu)))
  ("Help                           " (lambda (x y)
				       (sgt-error msg:dsi-help-comment
					     "The DSI is giving you help!")))
  ("Exit menu                      " (lambda (x y) nil))
 )
 :click-window graphic-trace-inner-window
 :icon-title  "DSI Menu"
 :menu-event  :ANY-mouseDOWN
 :start-event :middledown)
)
