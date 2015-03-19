;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-FILE-BROWSER; Base: 10 -*-
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


;;;  FILE BROWSER INTERFACE
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish
;;;


(in-package :DEMO-FILE-BROWSER)

(declaim (special FILE-BROWSER-WIN FILE-BROWSER-TOP-AGG QUIT-BUTTON))

(defvar DEMO-FILE-BROWSER-INIT
  (dolist (file '("text-buttons-loader"
		  "labeled-box-loader"
		  "browser-gadget-loader"))
    (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))


;;   Schemata defined in the DO-GO procedure which are referenced by other
;; functions
;;
(defvar FILE-BROWSER)
(defvar CONTROL-PANEL)
(defvar STATUS)

(defun PATHNAME-TO-STRING-FN (pathname)
  (if pathname
      (let ((file (file-namestring pathname)))
	;; CMU file-namestring can return null.
	(if (or (null file) (string= file "")) ; then pathname is a directory.
                                        ; So strip off the "/", get the directory name,
                                        ; and restore the "/".
	    (let ((directory (string-right-trim "/" (namestring pathname))))
	      (concatenate 'string (file-namestring directory) "/"))
            file))                ; else we already got the file name.
      ""))


(defun DIRECTORY-FN (namestring)
  (let ((dir (directory #+(or cmu) namestring
			#+(or sbcl) (concatenate 'string (namestring namestring) "*")
                        #-(or cmu sbcl) 
                        (concatenate 'string (namestring namestring) "/")
                        #-(or clisp cmu sbcl) :directories #-(or clisp cmu sbcl) t)))
    (if (or (null dir) (equal (car dir) namestring)) NIL dir)))

;;;
;;;  DO-GO:  Function to run FILE-BROWSER interface
;;;

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)

  (create-instance 'FILE-BROWSER-WIN inter:interactor-window
     (:left 20)(:top 50)(:width 600)(:height 270)
     (:double-buffered-p double-buffered-p)
     (:title "FILE BROWSER") (:icon-title "File-Browser"))
  (s-value FILE-BROWSER-WIN
	   :aggregate
	   (create-instance 'FILE-BROWSER-TOP-AGG opal:aggregate))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-FILE-BROWSER"))
     (g-value file-browser-win :destroy-hooks)))

  
  ;; Create FILE-BROWSER schema and add to window
  (create-instance 'FILE-BROWSER garnet-gadgets:browser-gadget
     ;; Why isn't :num-menus constant?  Because that would make the :items
     ;; slot of the :menu-list aggrelist constant, and the positions of the
     ;; scrolling menus would automatically become constant.  We want them
     ;; to move around, so don't let that :items slot become constant.
     (:constant T :except :num-menus)
     (:left 10)
     (:top 85)
     (:num-menus 3)
     (:additional-selection-p NIL)
     (:item-to-string-function #'PATHNAME-TO-STRING-FN)
     (:menu-items-generating-function #'DIRECTORY-FN)
     ;; This modification of :menu-function ensures the synchronization of the
     ;; STATUS message with the feedback objects of the scrolling menu.
     (:menu-function #'(lambda (browser sm-item)
			 (if (g-value sm-item :highlighted)
			     (let ((feed (g-value sm-item :parent :parent
						  :feedback-obj)))
			       (s-value feed :obj-over sm-item)
			       (s-value sm-item :highlighted NIL)
			       (s-value STATUS :visible T)
			       (opal:update FILE-BROWSER-WIN)
			       (s-value sm-item :highlighted T)
			       (s-value feed :obj-over NIL)
			       (garnet-gadgets:browser-menu-fn browser sm-item)
			       (s-value STATUS :visible NIL))
			     (garnet-gadgets:browser-menu-fn
			      browser sm-item)))))
  (opal:add-component FILE-BROWSER-TOP-AGG FILE-BROWSER)
  (opal:update FILE-BROWSER-WIN)


  ;; Create CONTROL-PANEL and add to window
  (create-instance 'CONTROL-PANEL opal:aggregadget
     (:constant :left :top)
     (:left 30)
     (:top 10)
     (:parts

      `((:prev ,garnet-gadgets:text-button-panel
	       (:left ,(o-formula (gvl :parent :left)))
	       (:top ,(o-formula (gvl :parent :top)))
	       (:shadow-offset 5)
	       (:gray-width 3)
	       (:text-offset 3)
	       (:final-feedback-p NIL)
	       (:items ("Prev"))
	       (:selection-function
		,#'(lambda (gadget value)
		     (declare (ignore gadget value))
		     (let* ((items (g-value FILE-BROWSER :items)))
		       (if items
			   (let* ((new-top-level-namestring
				   (directory-namestring
				    (string-right-trim "/" (namestring (car items))))))
			     (if (not (string= "" new-top-level-namestring))
				 ;; Add the new item to the browser
				 (progn
				   (s-value STATUS :visible T)
				   (opal:update FILE-BROWSER-WIN)
				   (garnet-gadgets:push-first-item FILE-BROWSER
				    (pathname new-top-level-namestring))
				   (s-value STATUS :visible NIL)))
			     ))))))

	(:l-box ,garnet-gadgets:labeled-box
		(:left ,(o-formula (+ 20 (gvl :parent :left)
				         (gvl :parent :prev :width))))
		(:top ,(o-formula (gvl :parent :top)))
		(:label-string "Directory:")
		(:value ,(o-formula
			  (let ((items (gv FILE-BROWSER :items)))
			    (if items
				(namestring (car items))
				""))))
		(:selection-function
		 ,#'(lambda (gadget string)
		      (declare (ignore gadget))
		      (when (string= string (string-right-trim "/" string))
			(setq string (concatenate 'string string "/")))
		      (s-value STATUS :visible T)
		      (opal:update FILE-BROWSER-WIN)
		      (garnet-gadgets:set-first-item
		       FILE-BROWSER (pathname string))
		      (s-value STATUS :visible NIL)))))))

  (create-instance 'STATUS opal:text
     (:constant T :except :visible)
     (:left 30)
     (:top (o-formula (+ 10 (opal:gv-bottom CONTROL-PANEL))))
     (:string "Fetching directory information...")
     (:font (create-instance NIL opal:font (:face :italic)))
     (:visible NIL))

  (create-instance 'QUIT-BUTTON garnet-gadgets:text-button-panel
     (:constant T)
     (:left (o-formula (+ 20 (opal:gv-right STATUS))))
     (:top (o-formula (+ 3 (opal:gv-bottom CONTROL-PANEL))))
     (:text-offset 3)
     (:shadow-offset 5)
     (:gray-width 3)
     (:final-feedback-p NIL)
     (:items '("Quit"))
     (:selection-function #'Do-Quit))
  (opal:add-components FILE-BROWSER-TOP-AGG CONTROL-PANEL STATUS QUIT-BUTTON)
  (opal:update FILE-BROWSER-WIN)

  (format t "~%Demo-File-Browser:
   Typing the name of a directory in the labeled box will cause the
   contents of that directory to be displayed in the first menu.
   If one of the items is selected with the left mouse button,
   then its contents will be shown in the next menu.
   Pressing on the 'Prev' button will show the previous directory
   in the hierarchy.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

)


(defun do-stop ()
  (opal:destroy FILE-BROWSER-WIN))


(defun Do-Quit (gadget value)
  (declare (ignore gadget value))
  (do-stop)
   ;;for demo-controller
  (unless (and (fboundp 'common-lisp-user::Garnet-Note-Quitted)
	       (common-lisp-user::Garnet-Note-Quitted "DEMO-FILE-BROWSER")))
)
