;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on Apr 11, 1991, 4:52 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                     $

;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     MULTI-TEXT from package OPAL
;;;     LINE from package OPAL
;;;     RECTANGLE from package OPAL
;;;     X-BUTTON-PANEL from package GARNET-GADGETS
;;;     LABELED-BOX from package GARNET-GADGETS


#||
(dolist (gadget '("labeled-box-loader"
		  "x-buttons-loader"
		  "text-buttons-loader"
		  ))
  (load (common-lisp-user::garnet-pathnames gadget
			 common-lisp-user::Garnet-Gadgets-PathName)))
;;;
;;;     Functions needed from Gilt
(load (common-lisp-user::garnet-pathnames "gilt-functions-loader"
			 common-lisp-user::Garnet-Gilt-PathName))
||#


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "C32")

(defun Write-Obj (obj)
  (if (stringp obj) obj
      (let ((kr::*print-as-structure* NIL))
	(write-to-string obj))))

(defun get-one-line-values (aggrel)
  (let (val1 val2)
    (opal:do-components aggrel
      #'(lambda (obj)
	  (when (g-value obj :active)
	    (push (g-value obj :name1) val1)
	    (push (g-value obj :name2) val2))))
    (list val1 val2)))

(create-instance 'one-line-prompt OPAL:AGGREGADGET
      (:BOX '(18 152 37 25))
      (:LEFT (o-formula (FIRST (GVL :BOX))))
      (:TOP (o-formula (SECOND (GVL :BOX))))
      (:middle-string " referred to as:")
      (:name1 :left)
      (:name2 (o-formula (gvl :string-val :value)))
      (:active T)
      (:parts `((:enabler ,GARNET-GADGETS:X-BUTTON
		 (:string "")
		 (:LEFT ,(o-formula (gvl :parent :left)))
		 (:TOP ,(o-formula (gvl :parent :top)))
		 (:selection-function ,#'(lambda(gadget val)
				      (s-value (g-value gadget :parent) :active
					       val))))
		(:string-val ,GARNET-GADGETS:LABELED-BOX
		 (:MIN-WIDTH 20)
		 (:LABEL-STRING ,(o-formula
				  (concatenate 'simple-string
					       (write-obj
						(gvl :parent :name1))
					       (gvl :parent :middle-string))))
		 (:LEFT ,(o-formula (+ (gvl :parent :left) 42)))
		 (:TOP ,(o-formula (+ (gvl :parent :top) 3))))
		(:legal-p ,OPAL:RECTANGLE
		 (:DRAW-FUNCTION :AND)
		 (:FILLING-STYLE ,OPAL:GRAY-FILL)
		 (:LINE-STYLE NIL)
		 (:visible ,(o-formula (not (gvl :parent :active))))
		 (:LEFT ,(o-formula (gvl :parent :string-val :left)))
		 (:TOP ,(o-formula (gvl :parent :string-val :top)))
		 (:WIDTH ,(o-formula (gvl :parent :string-val :width)))
		 (:HEIGHT ,(o-formula (gvl :parent :string-val :height)))))))
       
(create-instance 'c32-bold-font OPAL:FONT
		 (:FACE :BOLD))

(create-instance 'c32-title-font OPAL:FONT
		 (:SIZE :LARGE)
		 (:FACE :BOLD-ITALIC))


(defun Init-Value (obj val)
  (g-value obj :value)
  (s-value obj :value val))


(defun Set-one-line-prompts (aggrel oldnames newnames)
  (s-value aggrel :items (length oldnames))
  (opal:notice-items-changed aggrel)
  (do* ((oldns oldnames (cdr oldns))
	(oldn (car oldns)(car oldns))
	(newns newnames (cdr newns))
	(newn (write-obj (car newns))(write-obj (car newns)))
	(gadgets (get-value aggrel :components) (cdr gadgets))
	(gadget (car gadgets)(car gadgets))
	)
       ((null oldns))
    (s-value gadget :name1 oldn)
    (init-value (g-value gadget :string-val) newn)
    (s-value gadget :active T)
    (init-value (g-value gadget :enabler) T)))
