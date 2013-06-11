;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet Common-Lisp-User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
         5/22/92 Brad Vander Zanden - Added kr-changes.lisp
	 5/4/92 Ed Pervin - Removed provide statement.
         6/7/89 Brad Vander Zanden - Created
============================================================
|#

(format t "Loading Lapidary...~%")
(setf *load-verbose* t)

;; expand the amount of memory available so that garbage collections do
;; not occur

#+allegro (progn (setf (sys:gsgc-parameter :quantum) 400)
	         (setf (sys:gsgc-parameter :free-bytes-new-pages) 1500000))

#|
#+lucid (change-memory-management :expand 256 :growth-limit 1500)
|#
;; check first to see if place is set
(unless (boundp 'Garnet-Lapidary-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Lapidary-PathName before loading lapidary."))

;;; Load Aggregadgets unless already loaded  (this will load the lower 
;;; layers of Garnet if necessary)
#|
#+cmu
(unless (get :garnet-modules :aggregadgets)
  (load Garnet-Aggregadgets-Loader))

#+(not cmu)
(require 'opal Garnet-Aggregadgets-Loader)
|#

(format t "~%*** Loading default mouse bindings ***~%")
(garnet-load "lapidary:mouse-bindings")


;; load necessary gadgets
(dolist (loader-file '("text-buttons-loader"
			"error-gadget-loader"
                 	"arrow-line-loader"
		 	"labeled-box-loader"
		 	"x-buttons-loader"
	 	 	"v-slider-loader"
		 	"scrolling-labeled-box-loader"
		 	"radio-buttons-loader"
	 	 	"scrolling-window-loader"
		 	"scrolling-menu-loader"
			"menubar-loader"
			"prop-sheet-win-loader"))
  (garnet-load (concatenate 'string "gadgets:" loader-file)))

;;; first load c32
(unless (get :garnet-modules :c32)
  (load garnet-c32-loader))

(unless (get :garnet-modules :debug)
  (garnet-load "debug:debug-loader"))

;;;
;;;     Functions needed from Gilt
(garnet-load "gilt:gilt-functions-loader")

;;; Load the constraint gadget

(defvar Garnet-Constraint-Gadget-Pathname
  (merge-pathnames "lapidary/" Garnet-Binary-Pathname))
(defvar Garnet-Constraint-Gadget-Src
  (merge-pathnames "lapidary/" Garnet-Src-Pathname))

;;; If at cmu, then set up the search lists
#+cmu
(progn
  (setf (ext:search-list "constraint-gadget:")
	(list (namestring Garnet-Constraint-Gadget-PathName)))
  (setf (ext:search-list "constraint-gadget-src:")
	(list (namestring Garnet-Constraint-Gadget-Src))))


(defparameter Garnet-Constraint-Gadget-Loader
  (merge-pathnames "constraint-gadget-loader"
		    Garnet-Constraint-Gadget-PathName))

(load Garnet-Constraint-Gadget-Loader)


;; ---- Load Lapidary itself

(Defparameter Garnet-Lapidary-Files
  '(
        "lapidary-functions-loader" ; only load if not already loaded
	"parameters"
        "defs"
        "macros"
	"lapidary"  
	"dialog-parts2" "event-card" "card" "card1"
	"start-where" "prompt"
        "lapidary-objects"
	"feedback-objs" 
	"support-misc"
	"support-selection1"	"support-selection2"   	"selection"
	"create-object"
	"delete-object"
        "delete-window"
	"move-grow"
	"aggregates"
	"aggparam" "create-parameters"
	"properties"
        "line-imp" "line-props"
	"fill-imp" "fill-props"
	"color-imp" "color-props"
	"shapes"
	"lap-draw"
	"support-menu-editor"		        "new-editor"
        "text"   "text-properties"
        "gadgetcopy"
	"save-link-parameters"
        "lapidary-save"	"lapidary-read"	"support-save-restore" "save-restore"
	"add-gadget"
	"choice-inter" "text-inter"
	"move-grow-box" "support-move-grow-inter" "move-grow-inter" 
	"angle-inter" "two-point-inter"
    "support-inter" "by-demo"
    "interactors" "interactors-menu"
))

(dolist (file Garnet-Lapidary-Files)
  (garnet-load (concatenate 'string "lapidary:" file)))

(setf (get :garnet-modules :lapidary)  t)
;;; (provide 'lapidary)
(setf lapidary::*load-db* nil)

;;; cause the functions in kr to be exported to the user. Otherwise
;;; user created formulas may crash

(use-package :KR)

(format t "...Done Lapidary.~%")

