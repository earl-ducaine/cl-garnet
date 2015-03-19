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

;;; 
;;;  Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
         8/13/93  Andrew Mickish - Adjusted Horiz-Choice-List's constants
          3/4/93  Brad Myers - made work in immediate mode 
         3/03/92  Andrew Mickish - Fixed reference to :interim-selected slot
                    in Pop-Up-From-Icon gadget
         2/14/92  Brad Myers - maybe-constant slot
	 11/9/90  Brad Myers - started
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Horiz-Choice-List Pop-Up-From-Icon)))

(defparameter value-bold-font (opal:get-standard-font NIL :bold NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special gadget which is a list of items, allow the user to press on
;;; an item to change the value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'Horiz-Choice-List opal:aggregadget
      (:maybe-constant '(:left :top :items))
      (:left 0)
      (:top 0)
      (:items '("one" "two" "three"))
      (:value NIL) ; the result string
      (:height (o-formula (+ 3 (gvl :border :height))))
      (:selection-function NIL)
      (:parts
       `((:shadow ,opal:rectangle
	          (:Constant (:filling-style :line-style))
		  (:filling-style ,opal:black-fill)
		  (:line-style NIL)
		  (:left ,(o-formula (+ 2 (gvl :parent :left))))
		  (:top ,(o-formula (+ 2 (gvl :parent :top))))
		  (:width ,(o-formula (+ 3 (gvl :parent :list :width))))
		  (:height ,(o-formula (gvl :parent :border :height))))
	 (:border ,opal:rectangle
	          (:Constant (:filling-style :line-style))
		  (:filling-style ,opal:white-fill)
		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (gvl :parent :top)))
		  (:width ,(o-formula (+ 3 (gvl :parent :list :width))))
		  (:height ,(o-formula (1+ (gvl :parent :list :height)))))
	 (:list ,opal:aggrelist
		(:items ,(o-formula (gvl :parent :items)))
	        (:Constant (T :except :width :height :visible :items
			      :left :top))
		(:left ,(o-formula (+ 2 (gvl :parent :left))))
		(:top ,(o-formula (gvl :parent :top)))
		(:direction :horizontal)
		(:h-spacing 10)
		(:item-prototype (,opal:text
			 (:value ,(o-formula (nth (gvl :rank)
						  (gvl :parent :items))))
			 (:string ,(o-formula (let ((val (gvl :value)))
						(if (stringp val)
						    val
						    (format NIL "~s" val)))))
			 (:font ,(o-formula (if (gvl :selected) value-bold-font
						opal:default-font)))
			 (:selected ,(o-formula 
				      (equal (gvl :value)
					     (gvl :parent :parent :value)))))))))
      (:interactors 
       `((:pick-one ,inter:button-interactor
		    (:start-event :leftdown)
		    (:start-where ,(o-formula
				    (list :element-of (gvl :operates-on :list))))
		    (:window ,(o-formula (gv-local :self :operates-on :window)))
		    (:how-set :set)
		    (:continuous NIL)
		    (:final-function
		     ,#'(lambda(inter obj)
			  (let ((gadget (g-value inter :operates-on))
				(val (g-value obj :value)))
			    (s-value gadget :value val)
			    (kr-send gadget :selection-function
				     gadget val))))))))

(create-instance 'Pop-Up-From-Icon opal:aggregadget
      (:maybe-constant '(:left :top :icon-image :pop-up-function))
      (:left 0)
      (:top 0)
      (:icon-image (opal:read-image (merge-pathnames "pop-up-icon.bm" 
						     common-lisp-user::Garnet-Bitmap-Pathname)))
      (:interim-selected NIL) ; set by interactor
      (:pop-up-function NIL) ; put a function here to pop-up the menu or whatever,
			     ; It should stuff its results into the value field
			     ; It takes this gadget as a parameter
      (:value NIL) ; the result value
      (:parts
       `((:shadow ,opal:rectangle
                  (:constant (:filling-style :line-style))
		  (:filling-style ,opal:black-fill)
		  (:line-style NIL)
		  (:left ,(o-formula (+ 2 (gvl :parent :left))))
		  (:top ,(o-formula (+ 2 (gvl :parent :top))))
		  (:width ,(o-formula (gvl :parent :bitmap :width)))
		  (:height ,(o-formula (gvl :parent :bitmap :height))))
	 (:bitmap ,opal:bitmap
		  (:image ,(o-formula (gvl :parent :icon-image)))
		  (:left ,(o-formula (let ((left (gvl :parent :left)))
				       (if (gvl :parent :interim-selected)
					   (+ left 2) left))))
		  (:top ,(o-formula (let ((top (gvl :parent :top)))
				       (if (gvl :parent :interim-selected)
					   (+ top 2) top)))))))
      (:interactors 
       `((:pick-it ,inter:button-interactor
		    (:start-event :leftdown)
		    (:start-where ,(o-formula
				    (list :in (gvl :operates-on))))
		    (:window ,(o-formula (gv-local :self :operates-on :window)))
		    (:final-function
		     ,#'(lambda(inter obj)
			  (declare (ignore obj))
			  (let ((gadget (g-value inter :operates-on)))
			    (kr-send gadget :pop-up-function gadget))))))))
