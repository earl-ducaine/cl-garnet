;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The "Value Filter" extension to Gilt is a tool for assigning dependencies
;;; among objects.  With the Value Control module, the values returned by
;;; gadgets can be defined by a user-supplied expression.  The Enable Control
;;; module allows the user to define expressions that regulate whether gadgets
;;; are enabled and may be operated.  The Error Checking module allows the
;;; definition of an error handling routine, complete with a customized
;;; error dialog box.
;;;
;;; Designed by Brad Myers
;;; Implemented by Andrew Mickish

;;
;;  Enable Control module
;;

;;; CHANGE LOG:
;;; 03/08/93 Brad Myers - invoke from menubar
;;; 02/02/93 Andrew Mickish - opal:set-strings ---> opal:set-text


(in-package "GILT")

(declaim (special ENABLE-CONTROL))


(defun Set-Enable-Slot (expr en-con)
  (let* ((obj (g-value en-con :for-object))
	 (constant (g-value obj :constant)))
    (s-value obj :active-p (formula expr))
    (if (or (equal '(t) constant) (equal t constant))
	(s-value obj :constant '(t :except :active-p)))))

(defun Enable-Control-OK-Fn (en-con values)
  (let ((expr (g-value en-con :filter-expr)))
    (unless (string= "" (g-value en-con :filter-text-obj :value))
      (Set-Enable-Slot expr en-con)))
  (if (string= "OK" (gilt:value-of :ok-apply-cancel values))
      (Close-ENCON-Win en-con)))

(defun Enable-Control-Cancel-Fn (en-con values)
  (declare (ignore values))
  (Close-ENCON-Win en-con))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ENCON-Foreground-Color* (create-instance NIL opal:color
				   (:red .8) (:green .8) (:blue 1)))
(defvar *ENCON-Foreground-Fill* (create-instance NIL opal:default-filling-style
				 (:foreground-color *ENCON-Foreground-Color*)))

(defvar *ENCON-Highlight-Color* (create-instance NIL opal:color
				  (:red .75) (:green .8) (:blue 1)))
(defvar *ENCON-Highlight-Fill* (create-instance NIL opal:default-filling-style
				 (:foreground-color *ENCON-Highlight-Color*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level function to display an enable-control panel for an object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *encon-position-list* NIL)
(defparameter *Used-ENCON-Wins* NIL)

;; These position functions are used to position the next enable control
;; menu appropriately with respect to the menus that are already on the
;; screen.
;;
(defun Next-ENCON-Position-Index ()
  (if *encon-position-list* (1+ (reduce 'max *encon-position-list*)) 0))
(defun Next-ENCON-Left (i)
  (+ (g-value ENABLE-CONTROL :window-left) (* 30 i)))
(defun Next-ENCON-Top (i)
  (+ (g-value ENABLE-CONTROL :window-top) (* 30 i)))

(defun Close-ENCON-Win (encon)
  (inter:abort-interactor (g-value encon :filter-text-obj :parent :edit-it))
  (setf *encon-position-list* (remove (g-value encon :position-index)
				      *encon-position-list*))
  (push (g-value encon :window) *Used-ENCON-Wins*))

(defun Destroy-Encon-Wins ()
  (dolist (ec (g-value ENABLE-CONTROL :is-a-inv))
    (opal:destroy (g-value ec :window)))
  (setf *Used-ENCON-Wins* NIL)
  (setf *encon-position-list* NIL))

(defun Set-ENCON-Win-Position (encon-win)
  (let ((encon (car (g-value encon-win :aggregate :components)))
	(i (next-encon-position-index)))
    (push i *encon-position-list*)
    (s-value encon-win :left (next-vc-left i))
    (s-value encon-win :top (next-vc-top i))
    (s-value encon :position-index i)))
  
(defun Pop-Used-ENCON-Win (for-obj title)
  (let ((encon-win (pop *Used-ENCON-Wins*)))
    (when encon-win
      (Set-ENCON-Win-Position encon-win)
      (s-value encon-win :title title)
      (s-value encon-win :visible T)
      (let ((encon (car (g-value encon-win :aggregate :components))))
	(s-value encon :for-object for-obj)
	(opal:update encon-win)
	encon))))

(defun Create-ENCON-Win (for-obj title)
  (let* ((i (car (push (next-encon-position-index) *encon-position-list*)))
	 (en-con (create-instance NIL Enable-Control
		    (:position-index i)
		    (:for-object for-obj)
		    (:window-title title)))
	 (en-con-win (show-in-window en-con (next-encon-left i)
				     (next-encon-top i)))
	 (filter-win (Generate-Enable-Expr-Win en-con en-con-win))
	 ;; In this editable-text object, there are two strings.  One is in
	 ;; the :string slot, which is displayed and edited by the user.
	 ;; The other is in the :value slot, which is set when the user has
	 ;; finished editing the string in the :string slot.  This allows
	 ;; dependencies to be set up on the :value, which is always a
	 ;; complete expression, versus the dynamic one in :string.
	 (filter-str-agg (create-instance NIL editable-text
			   (:enable-control en-con)
			   (:scrolling-window filter-win)
			   (:final-function
			    #'(lambda (i o e string x y)
				(declare (ignore i e x y))
				(s-value o :user-edited-p T)
				(s-value o :value
					 (opal:text-to-string string))))))
	 (filter-text-obj (g-value filter-str-agg :string)))
    (opal:update filter-win)
    (opal:add-component (g-value filter-win :inner-aggregate) filter-str-agg)
    (s-value en-con :filter-text-obj filter-text-obj)
    (s-value en-con :scrolling-window filter-win)
    en-con))
	

(defun Show-Enable-Control (for-obj)

  ;; Create the enable control menu
  (let* ((title (concatenate 'string
			     "Enable Control:  " (name-for-schema for-obj)))
	 (en-con (or (Pop-Used-ENCON-Win for-obj title)
		     (Create-ENCON-Win for-obj title)))
	 (filter-text-obj (g-value en-con :filter-text-obj)))
    (gg:scroll-win-to (g-value en-con :scrolling-window) 0 0)
    ;; Set the initial filter function string
    (let ((e-val (get-value for-obj :active-p)) ;; e-val = enabled value
	  e-string)
      (if e-val
	  (let ((expr (if (formula-p e-val)
			  (kr::a-formula-lambda e-val)
			  e-val)))
	    (setq e-string (if (stringp expr) expr
			       (write-to-string expr :pretty T
						:case :downcase))))
	  (setq e-string ""))

      (opal:set-text filter-text-obj e-string)
      (s-value filter-text-obj :value e-string))
    en-con))


(defun Enable-Control-Func (&rest args)
  (declare (ignore args))
  (let* ((objs (g-value *Selection-Obj* :value))
	 (obj (car objs))
	 (found-ec NIL))
    (cond ((null objs) (Gilt-Error "Nothing selected"))
	  ((cdr objs) (Gilt-Error "Only one object can be selected"))
	  ((progn
	     (dolist (ec (g-value ENABLE-CONTROL :is-a-inv))
	       (if (and (eq obj (g-value ec :for-object))
			(g-value ec :window)
			(g-value ec :window :visible))
		   (setf found-ec ec)))
	     found-ec)
	   (Gilt-Error (format NIL "An enable control panel is already
being used for ~S" obj)))
          (T (Show-Enable-Control obj)))))



;;
;;  Motif Enable Control objects
;;

(in-package "GILT")

(create-instance 'ENABLE-CONTROL opal:aggregadget
  (:window-left 300) (:window-top 400)
  (:window-width 495) (:window-height 225)
  (:window-title "Enable Control")
  (:window-background-color *ENCON-Foreground-Color*)
  (:width (o-formula (gvl :window :width) 495))
  (:height (o-formula (gvl :window :height) 225))
  (:for-object NIL)
  (:slot-to-reference :selected)
  (:filter-expr (o-formula (let* ((string (gvl :filter-text-obj :value))
				 expression error-p)
			    (multiple-value-setq (expression error-p)
			      (gg:careful-read-from-string string
							   *error-gadget*))
			    (unless error-p expression))))
  (:function-for-ok #'Enable-Control-OK-Fn)
  (:function-for-cancel #'Enable-Control-Cancel-Fn)
  (:parts `(
    (:title ,OPAL:TEXT
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(opal:get-standard-font NIL :bold-italic :large))
      (:BOX (4 4 361 20 ))
      (:STRING ,(o-formula (concatenate 'simple-string
			    "Change my Enable for \""
			    (Get-User-Name (gvl :parent :for-object))
			    "\"")))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 4))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 4)))
    (:ok-apply-cancel ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
     (:GILT-REF "TYPE-OKCANCEL")
     (:DIRECTION :HORIZONTAL)
     (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
     (:ITEMS ("OK" "Apply" "Cancel" ))
     (:LEFT 300) (:TOP 35)
     (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
     (:interactors
      (:press (:key :omit))))
    (:expression-label ,OPAL:TEXT
      (:LEFT 12) (:TOP 78)
      (:STRING "Expression:")
      (:FONT ,(opal:get-standard-font NIL :bold NIL)))
    (:insert-ref ,GARNET-GADGETS:MOTIF-TEXT-BUTTON
     (:selection-function Insert-Ref-Obj-Into-Str)
     (:string "Use Value of Object")
     (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
     (:GILT-REF "TYPE-MOTIF-TEXT-BUTTON")
     (:LEFT ,(o-formula (- (opal:gv-right (gvl :parent :ok-apply-cancel))
			   (gvl :width))))
     (:TOP ,(o-formula (+ 5 (opal:gv-bottom
			     (gvl :parent :ok-apply-cancel)))))
     (:interactors
      (:press (:key :omit))))
    (:result ,gg:motif-scrolling-labeled-box
     (:left 12) (:top 195) (:width 460)
     (:label-string "Resulting Enable Value:")
     (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
     (:value ,(o-formula
	       (let ((for-obj (gvl :parent :For-Object))
		     (expr (gvl :parent :filter-expr)))
		 (if (and for-obj (schema-p for-obj))
		     (prin1-to-string (gg:Careful-Eval-Formula-Lambda
				       expr *error-gadget* NIL for-obj
				       :value kr::*current-formula* NIL))
		     ;; else if no object, use ""
		     ""))))
     (:parts (:label-text :frame
	      (:field-text :modify
			   (:interactors
			    ((:text-edit :omit)))))))
	    )))

(defun Generate-Enable-Expr-Win (en-con en-con-win)
  (create-instance NIL gg:motif-scrolling-window-with-bars
    (:parent-window en-con-win)
    (:enable-control en-con)
    (:left 12) (:top 94) (:width 460) (:height 90)
    (:total-width 600)
    (:total-height (o-formula (+ 15 (gvl :enable-control :filter-text-obj
					 :height)) 29))
    (:foreground-fill *ENCON-Highlight-Fill*)
    (:foreground-color *ENCON-Highlight-Color*)
    (:parts
     `((:v-scroll :modify
	(:interactors
	 (:slide :jump (:key :omit))))
       (:h-scroll :modify
	(:interactors
	 (:slide :jump (:key :omit))))))))