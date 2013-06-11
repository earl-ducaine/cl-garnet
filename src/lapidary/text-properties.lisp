;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; This file provides the functions that handle aggregadgets in Lapidary
;;;
;;; Changes:
;;; 16-Jan-93 Mickish - Changed :value of labeled-boxes from NIL to "";
;;;             Correspondingly set :value to "" in Wipe-File-Panel and
;;;             selection-function.
;;; 12-May-92 ECP  Added :ok-cancel to text-properties-menu
;;;

(in-package "LAPIDARY")


(defparameter *font-hash-table* NIL)

;;;**************
;;;  APPLY-FONT
;;;**************

(defun APPLY-FONT (font)
  (let ((selections (g-value *SELECTION-INFO* :selected)))
    (if selections
	(mapcar #'(lambda (obj)
		    (when (is-a-p obj opal:text)
		      (destroy-constraint obj :font)
		      (s-value obj :font font)))
		selections))))


;;;**********************
;;;  GET-FONT-FROM-SPEC
;;;**********************

;; A font specification is a list of three atoms, where the first atom is
;; the family, the second atom is the size, and the third atom is the face.
;;
(defun GET-FONT-FROM-SPEC (spec)
  (unless *font-hash-table* (setf *font-hash-table*
				  (make-hash-table :test #'equal)))
  (let ((entry (gethash spec *font-hash-table*)))
    (if entry
	entry
	(setf (gethash spec *font-hash-table*)
	      (create-instance NIL opal:font
                 (:family (first spec))
		 (:size (second spec))
		 (:face (third spec)))))))




;;;****************************************
;;;  Wipe functions and associated macros
;;;****************************************

(defmacro FORMULA-BUTTON ()
  `(g-value TEXT-PROPERTIES-MENU :font-agg :contents :formula-button))
(defmacro STANDARD-FONT-PANEL ()
  `(g-value TEXT-PROPERTIES-MENU :font-agg :contents :standard-font-panel))
(defmacro FONT-FROM-FILE-PANEL ()
  `(g-value TEXT-PROPERTIES-MENU :font-agg :contents :font-from-file-panel))
(defmacro FONT-NAME ()
  `(g-value (FONT-FROM-FILE-PANEL) :name-box))
(defmacro FONT-PATH ()
  `(g-value (FONT-FROM-FILE-PANEL) :path-box))
(defmacro DEFAULT-PATH ()
  `(g-value (FONT-FROM-FILE-PANEL) :default-button))

(defun WIPE-STANDARD-PANEL ()
  (declare (special text-properties-menu))
  (let ((panels (cdr (g-value (STANDARD-FONT-PANEL) :components))))
    (mapcar #'(lambda (panel)
		(s-value panel :value NIL))
	    panels)))

(defun WIPE-FILE-PANEL ()
  (declare (special text-properties-menu))
  (let ((components (cdr (g-value (FONT-FROM-FILE-PANEL) :components))))
    (mapcar #'(lambda (component)
		(s-value component :value ""))
	    components)))

(defun WIPE-FORMULA-BUTTON ()
  (declare (special text-properties-menu))
  (s-value (FORMULA-BUTTON) :value NIL))



(defun make-text-properties-db ()

  (create-instance 'TEXT-PROPERTIES-WIN inter:interactor-window
   (:title "text properties")
   (:left 550) (:top 100) (:width 470) (:height 380) 
   (:visible nil))

  (create-instance 'TEXT-PROPERTIES-MENU opal:aggregadget
   (:left 10) (:top 10)
   (:constant '(:left top))
   (:parts
    `((:title ,opal:text
	  (:constant (t))
          (:left ,(o-formula (gvl :parent :left)))
	  (:top ,(o-formula (gvl :parent :top)))
	  (:string "Text-Properties")
	  (:font ,*very-large-bold-italic-serif-font*))
      (:ok ,garnet-gadgets:text-button
        (:constant (t))
	(:button-width 44)
	(:left ,(o-formula (- (opal:gv-right (gvl :parent :font-agg))
			      (gvl :width))))
        (:top 10)
        (:gray-width 3) (:shadow-offset 5) 
	(:text-offset 2)
        (:selection-function
           ,#'(lambda (button string)
                (declare (ignore string))
                (s-value (g-value button :window) :visible nil)))
        (:string "OK")
        (:final-feedback-p nil))
      (:font-agg ,opal:aggregadget
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom (gvl :parent :title)))))
	  (:parts
	   ((:font-box ,TITLED-FRAME
		(:constant (t))
		(:string ":font")
		(:width ,(o-formula (gvl :parent :parent :string-agg
					 :string-box :width))))

    ;; This :contents is a part of the :font-agg, and its parts are all of
    ;; the panels used to generate new fonts.
    ;;
    (:contents ,opal:aggregadget
	(:left ,(o-formula (opal:gv-center-x-is-center-of
		   (gvl :parent :parent :string-agg :contents))))
	(:top ,(o-formula (+ 20 (gvl :parent :top))))
	(:width ,(o-formula (MAX (gvl :standard-font-panel :width)
				 (gvl :font-from-file-panel :width))))
	(:parts
	 ((:standard-font-panel ,opal:aggregadget
	      (:constant (:left :top :width :height))
	      (:left ,(o-formula (gvl :parent :left)))
	      (:top ,(o-formula (gvl :parent :top)))
	      (:width ,(o-formula (+ 20 (MAX (gvl :family-panel :width)
					 (gvl :size-panel :width)
					 (gvl :face-panel :width)))))
	      (:font-spec (:fixed :medium :roman))
	      (:parts
	       ((:title ,opal:text
		    (:constant (t))
		    (:left ,(o-formula (gvl :parent :left)))
		    (:top ,(o-formula (gvl :parent :top)))
		    (:string "Standard Fonts")
		    (:font ,*bold-font*))
		(:family-panel ,FONT-PARAMETER-PANEL
		    (:constant (t :title :items :rank :width :height))
		    (:top ,(o-formula (+ 5 (opal:gv-bottom
					    (gvl :parent :title)))))
		    (:title "Family") (:rank 0)
		    (:value ,(o-formula (first (gvl :parent :font-spec))))
		    (:items (:serif :sans-serif :fixed)))
		(:size-panel ,FONT-PARAMETER-PANEL
		    (:constant (t :title :items :rank :width :height))
		    (:top ,(o-formula (+ 5 (opal:gv-bottom 
				      (gvl :parent :family-panel)))))
		    (:title "Size") (:rank 1)
		    (:value ,(o-formula (second (gvl :parent :font-spec))))
		    (:items (:small :medium :large :very-large)))
		(:face-panel ,FONT-PARAMETER-PANEL
		    (:constant (t :title :items :rank :width :height))
		    (:top ,(o-formula (+ 5 (opal:gv-bottom
					    (gvl :parent :size-panel)))))
		    (:title "Face") (:rank 2)
		    (:value ,(o-formula	(third (gvl :parent :font-spec))))
		    (:items (:roman :italic :bold :bold-italic))))))


  (:font-from-file-panel ,opal:aggregadget
      (:left ,(o-formula (gvl :parent :left)))
      (:top ,(o-formula (+ 10 (opal:gv-bottom
			       (gvl :parent :standard-font-panel)))))
      (:width ,(o-formula (+ (gvl :default-button :width)
			     (gvl :path-box :width))))
      (:parts
       ((:title ,opal:text
	    (:constant (t))
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:string "Font From File")
	    (:font ,*bold-font*))
	(:name-box ,garnet-gadgets:labeled-box
	    (:constant (t))
	    (:left ,(o-formula (+ 20 (gvl :parent :left))))
	    (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :title)))))
	    (:width ,(o-formula (+ (gvl :min-frame-width)
		      (gvl :label-offset) (gvl :label-text :width))))
	    (:label-string "Font-Name")
	    (:label-font ,opal:default-font)
	    (:min-frame-width 100)
	    (:value "")
	    (:selection-function
	     ,#'(lambda (gadget font-name)
		  (let* ((file-panel (g-value gadget :parent))
			 (font-path (g-value file-panel :path-box :value)))

		    ;; White-out other panels
		    (wipe-standard-panel) (wipe-formula-button)

		    (if (and font-path (string/= font-path ""))
			(let ((font (create-instance NIL opal:font-from-file
						     (:font-path font-path)
						     (:font-name font-name))))
			  (apply-font font)
			  (s-value text-properties-win :value font))
			(if (g-value file-panel :default-button :value)
			    (let ((font (create-instance NIL opal:font-from-file
							 (:font-name font-name))))
			      (apply-font font)
			      (s-value text-properties-win :value font))))))))
			
	(:default-button ,garnet-gadgets:radio-button
	    (:constant (t))
	    (:left ,(o-formula (+ 20 (gvl :parent :left))))
	    (:top ,(o-formula (+ 10 (opal:gv-bottom (gvl :parent :name-box)))))
	    (:font ,opal:default-font)
	    (:string "Default Font Path")
	    (:selection-function
	     ,#'(lambda (gadget value)
		  (declare (ignore value))
		  (let* ((file-panel (g-value gadget :parent))
			 (font-name (g-value file-panel :name-box :value)))

		    ;; White-out other panels
		    (wipe-standard-panel) (wipe-formula-button)
		    (s-value (g-value file-panel :path-box) :value "")
				 
		    (when (and font-name (string/= font-name ""))
		      (let ((font (create-instance NIL opal:font-from-file
						   (:font-name font-name))))
			(apply-font font)
			(s-value text-properties-win :value font)))))))

	(:path-box ,garnet-gadgets:labeled-box
	    (:constant (t))
	    (:left ,(o-formula (+ 15 (opal:gv-right
				      (gvl :parent :default-button)))))
	    (:top ,(o-formula (opal:gv-center-y-is-center-of
			       (gvl :parent :default-button))))
	    (:width ,(o-formula (+ (gvl :min-frame-width)
				(gvl :label-offset) (gvl :label-text :width))))
	    (:label-string "or  Font-Path")
	    (:label-font ,opal:default-font)
	    (:min-frame-width 100)
	    (:value "")
	    (:selection-function
	     ,#'(lambda (gadget font-path)
		  (if (and font-path (string/= font-path ""))
		      (let* ((file-panel (g-value gadget :parent))
			     (font-name (g-value file-panel :name-box :value)))

			;; White-out other panels
			(wipe-standard-panel) (wipe-formula-button)
			(s-value (g-value file-panel :default-button)
				 :value NIL)

			(when (and font-name (string/= font-name ""))
			  (let ((font (create-instance NIL opal:font-from-file
						       (:font-path font-path)
						       (:font-name font-name))))
			    (apply-font font)
			    (s-value text-properties-win :value font)))))))))))


  (:formula-button ,garnet-gadgets:radio-button
	      (:constant (t))
	      (:left ,(o-formula (gvl :parent :left)))
	      (:top ,(o-formula (+ 10 (opal:gv-bottom
				       (gvl :parent :font-from-file-panel)))))
	      (:string "<Formula>")
	      (:font ,*bold-font*)
	      (:selection-function
	       ,#'(lambda (gadget value)
		    (declare (ignore value))
;		    (reset-undo)
		    (if (g-value *selection-info* :selected)
			(progn
			  (gg:c32 (car (g-value *selection-info* :selected))
			       :font)
			  ;; White-out other panels
			  (wipe-standard-panel) (wipe-file-panel))
		        ;; else, tell the user there must be a selection
		        ;; first
		        (progn
			  (s-value gadget :value nil)
			  (lapidary-error "make make a selection first"))))))


  (:unconstrain-font-button ,garnet-gadgets:text-button
;	      (:constant (t))
	      (:left ,(o-formula (+ 15 (opal:gv-right
					(gvl :parent :formula-button)))))
	      (:top ,(o-formula (+ 10 (opal:gv-bottom
				       (gvl :parent :font-from-file-panel)))))
	      (:gray-width 3) (:shadow-offset 5) (:text-offset 2)
	      (:final-feedback-p NIL)
	      (:string "Unconstrain")
	      (:selection-function
	       ,#'(lambda (gadget value)
		    (declare (ignore gadget value))
		    (unconstrain-fn :font :type opal:text)
		    (wipe-formula-button)

		    ;; Set up standard font panel to show characteristics of
		    ;; font of current selection.
		    (let* ((selection
			    (car (g-value *SELECTION-INFO* :selected)))
			  (font (g-value selection :font))
			  (panel (STANDARD-FONT-PANEL)))
		     (s-value panel
			      :font-spec
			      (list (g-value font :family)
				    (g-value font :size)
				    (g-value font :face)))
		     (mark-as-changed panel :font-spec))))
	      )))))))

   (:string-agg ,opal:aggregadget
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom (gvl :parent :font-agg)))))
	  (:parts
	   ((:string-box ,TITLED-FRAME
		(:constant (t))
		(:string ":string"))
	    (:contents ,garnet-gadgets:text-button-panel
		(:constant (t))
                (:left ,(o-formula (+ 20 (gvl :parent :left))))
		(:top ,(o-formula (+ 20 (gvl :parent :top))))
		(:direction :horizontal)
		(:gray-width 3) (:shadow-offset 5) (:text-offset 2)
		(:final-feedback-p NIL)
		(:items (("Generate Text from Formula"
			  ,#'(lambda (gadget value)
			       (declare (ignore value))
		    (if (g-value *selection-info* :selected)
			(gg:c32 (car (g-value *selection-info* :selected))
			     :string)
		        ;; else, tell the user there must be a selection
		        ;; first
		        (progn
			  (s-value gadget :value nil)
			  (lapidary-error "make make a selection first")))))

			 ("Remove Text Formula"
			  ,#'(lambda (gadget value)
			       (declare (ignore gadget value))
			       (unconstrain-fn :string :type opal:text))))))
	    ))))))

;; initialize the value slot of text-properties-win
(s-value text-properties-win :value 
	 (get-font-from-spec
	  (g-value text-properties-menu :font-agg :contents
			       :standard-font-panel :font-spec)))

(create-instance 'TEXT-PROPERTIES-TOP-AGG opal:aggregate)
(opal:add-component TEXT-PROPERTIES-TOP-AGG TEXT-PROPERTIES-MENU)
(s-value TEXT-PROPERTIES-WIN :aggregate TEXT-PROPERTIES-TOP-AGG)
(opal:update TEXT-PROPERTIES-WIN))
