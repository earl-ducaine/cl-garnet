;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V3.0: The Garnet Interface Builder
;;; on Jun 20, 1993, 4:29 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     MULTIFONT-TEXT from package OPAL
(dolist (gadget '("text-buttons-loader"
		  ))
  (load (merge-pathnames gadget common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(dolist (file '("gilt-functions-loader"
		"filter-functions-loader"))
  (load (merge-pathnames file common-lisp-user::Garnet-Gilt-PathName)))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")

(defparameter common-lisp-user::*Used-Gilt-Version* "V3.0")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(INTER-AGG-QUERY)))

(defparameter common-lisp-user::*Garnet-Object-Just-Created* 
(create-instance 'INTER-AGG-QUERY OPAL:AGGREGADGET
  (:FUNCTION-FOR-OK NIL)
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 225))
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 610)
  (:WINDOW-HEIGHT 225)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 610))
  (:WINDOW-TITLE "inter-agg-query")
  (:EXPORT-P T)
  (:parts `(
    (0 ,OPAL:multi-TEXT
      (:BOX (100 19 3 3 ))
      (:CONSTANT T)
      (:string ,(o-formula 
	   (format nil "It appears that this interactor should be inserted into
the aggregate named ~S.

-If you would like to see this aggregate highlighted, 
   press the 'highlight aggregate' button.
-If you would like the interactor inserted into the
   aggregate, press the 'insert into aggregate' button.
-If you do not want the interactor inserted into the
   aggregate, press the cancel button." (gvl :parent :agg-to-insert-into))))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 100))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 19)))
    (1 ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:BOX (28 171 535 44 ))
      (:CONSTANT T)
      (:final-feedback-p nil)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 28))
      (:ITEMS ("highlight aggregate" "ok" "cancel" ))
      (:selection-function insert-inter-into-agg-query-fn)
      (:DIRECTION :HORIZONTAL)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 171))))))

)

(defun insert-inter-into-agg-query-fn (gadget value)
  (cond ((string= value "highlight aggregate")
	 (garnet-debug:flash (g-value gadget :parent :agg-to-insert-into)))
	(t
	 (s-value (g-value gadget :window) :visible nil)
	 (opal:update-all)
	 (inter:interaction-complete (string= value "ok")))))
	 
	 
