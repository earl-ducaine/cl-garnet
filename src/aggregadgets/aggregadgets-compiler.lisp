;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                             $

;;; Changes:
;;; 10/2/03 RGA --- New compile/load protocol
;;; 7/28/96 RGA --- changed to use garnet-compile/load
;;; 14-May-93 Mickish     Added new files, split off from others
;;; 20-Jan-92 Mickish     Removed make-package call
;;; 26-Mar-91 Pervin      Load compiled files in Lucid.
;;; 22-Mar-91 Pervin      Added setf of :garnet-modules, and provides at end.
;;; 19-Mar-91 Pervin      Added aggregraphs.
;;; 4-Mar-91 D'Souza      Removed nickname "MO" of Opal.
;;; 5-Jun-90 Richardson   Added lispworks
;;; 8-May-90 Dannenberg   Added new files
;;; 16-Apr-90 Pervin      Changed #+explorer to #+(or allegro explorer)
;;; 12-Apr-90 Mitchell    Added #+allegro (gc t)
;;; 3/22/90 Robert Cook - Define the package "OPAL" for the TI Explorer

(in-package "COMMON-LISP-USER")

(defvar *debug-aggregadgets-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-aggregadgets-mode*
       (and (boundp '*garnet-compile-debug-settings*)
	    *garnet-compile-debug-settings*)
       ;; Global default settings.
       (and (boundp '*default-garnet-proclaim*) 
	    *default-garnet-proclaim*))))


(Defvar Garnet-Aggregadgets-Files
  '(
    "agg-macros"
    "agg-utils"
    "aggregadgets"
    "aggrelists"
    "add-agg"
    "agg-fix-slots"
    "copy-agg"
    "save-agg"
    "string-edit"
    "agg-labels"
    "rectangle-conflict-object"
    "aggregraphs"
    "scalable-aggregraph"
    "scalable-aggregraph-image"
    ))

(eval-when (eval load compile)
  (garnet-mkdir-if-needed Garnet-Aggregadgets-Pathname))

(dolist (file Garnet-Aggregadgets-Files)
  (let ((gfile (concatenate 'string "aggregadgets:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Aggregadgets-Src Garnet-Aggregadgets-Pathname
		   '("aggregadgets-loader.lisp"
		     "aggregraphs-loader.lisp"))

(setf (get :garnet-modules :aggregadgets) t)
(setf (get :garnet-modules :aggregraphs) t)
