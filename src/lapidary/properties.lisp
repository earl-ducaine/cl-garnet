;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "LAPIDARY")

;=============================================================================
;  The idea:
;
;      For every menu, generate a list of buttons to appear in the menu.
;  This list is then plugged into the :parts slot of an aggrelist to create
;  the menu.
;      To generate the list of buttons, call MAKE-BUTTON-LIST with parameters
;  described in the comments for MAKE-BUTTON-LIST and MAKE-BUTTON-PARTS.
;      The reason that specification lists are constructed and passed is that
;  it avoids duplication of objects -- if objects were naively created and
;  then instantiated again in the :parts slot of the aggrelists, we would
;  end up with about twice as many objects as we want.
;
;=============================================================================


;; **************************************************************************
;;    Global Menu Code
;;
;;    Everything between here and the next row of '**' can be put in a
;;  top-level file which all of the Lapidary "properties" menus use.
;;


;;  PROPERTIES-BUTTON
;;    This schema is used in the "properties" menus, in conjunction with
;;  the functions MAKE-BUTTON-LIST and MAKE-BUTTON-PARTS.  Instances of this
;;  schema should be customized to have a :label part that is an object to
;;  be displayed within the button.
;;
(create-instance 'PROPERTIES-BUTTON opal:aggregadget
   (:floating-left (o-formula (+ (gvl :left)
				 (if (gvl :interim-selected) 3 0))))
   (:floating-top (o-formula (+ (gvl :top)
				(if (gvl :interim-selected) 3 0))))
   (:width (o-formula (gvl :parent :button-width)))
   (:height (o-formula (gvl :parent :button-height)))
   (:parts
    `((:shadow ,opal:rectangle
	   (:left ,(o-formula (+ 3 (gvl :parent :left))))
	   (:top ,(o-formula (+ 3 (gvl :parent :top))))
	   (:width ,(o-formula (- (gvl :parent :width) 3)))
	   (:height ,(o-formula (- (gvl :parent :height) 3)))
	   (:filling-style ,opal:black-fill)
	   (:line-style NIL))
      (:gray-outline ,opal:rectangle
	   (:left ,(o-formula (gvl :parent :floating-left)))
	   (:top ,(o-formula (gvl :parent :floating-top)))
	   (:width ,(o-formula (- (gvl :parent :width) 3)))
	   (:height ,(o-formula (- (gvl :parent :height) 3)))
	   (:filling-style ,opal:gray-fill))
      (:feedback ,opal:rectangle
	   (:left ,(o-formula (+ 3 (gvl :parent :floating-left))))
	   (:top ,(o-formula (+ 3 (gvl :parent :floating-top))))
	   (:width ,(o-formula (- (gvl :parent :width) 9)))
	   (:height ,(o-formula (- (gvl :parent :height) 9)))
	   (:filling-style ,(o-formula (if (gvl :parent :selected)
					   opal:black-fill
					   opal:white-fill)))))))


;;  MAKE-BUTTON-LIST
;;    This function takes a list of lists, where each sublist is of the
;;  form  (object [:param-name param-value]).  The function returns a
;;  list of "properties menu" button definitions, one definition for each
;;  sublist.  The list returned from this function can be used immediately
;;  as the :parts list of an aggrelist.  (See MAKE-BUTTON-PART for syntax of
;;  sublists).
;;
#|
(defun MAKE-BUTTON-LIST (specifications-list)
  (mapcar #'(lambda (args)
	      (eval `(make-button-part ,@args)))
	  specifications-list))
|#
(defun MAKE-BUTTON-LIST (specifications-list)
  (let (parts)
    (dolist (spec specifications-list)
	    (push (eval `(make-button-part ,@spec)) parts))
    (reverse parts)))


;;  MAKE-BUTTON-PART:
;;    This function takes an object that will appear as the label of the button,
;;  and returns a list.  The list is intended as an element for the :parts
;;  list in an aggrelist.  (If you take several elements returned from this
;;  function and put them all in a list, then you can insert that list in
;;  the :parts slot of an aggrelist).
;;
(defun MAKE-BUTTON-PART (object &key (string "")
				     (line-style opal:default-line-style)
				     (filling-style NIL)
				     (value nil)
				     (name nil))
  ; Since all objects of the same type will have the same formulas, generate
  ; the appropriate list of slots for the label based on the type of object
  ; that is passed
  (let
   ((slots
     (cond

      ;; OPAL:TEXT
      ((equal object opal:text)
       `((:constant (t :except :left :top))
	 (:left ,(o-formula (- (+ (gvl :parent :floating-left)
				  (floor (gvl :parent :gray-outline :width) 2))
			       (floor (gvl :width) 2))))
	 (:top ,(o-formula (- (+ (gvl :parent :floating-top)
				 (floor (gvl :parent :gray-outline :height) 2))
			      (floor (gvl :height) 2))))
	 (:string ,string)
	 (:name ,name)
	 (:value ,value)
	 (:draw-function :xor)))

      ;; OPAL:LINE
      ((equal object opal:line)
       `((:constant (t :except :x1 :y1 :x2 :y2))
	 (:x1 ,(o-formula (- (+ (gvl :parent :floating-left)
				(floor (gvl :parent :gray-outline :width) 2))
			     25)))
	 (:y1 ,(o-formula (+ (gvl :parent :floating-top)
			     (floor (gvl :parent :gray-outline :height) 2))))
	 (:x2 ,(o-formula (+ (gvl :x1) 50)))
	 (:y2 ,(o-formula (gvl :y1)))
	 (:draw-function :xor)
	 (:value ,value)
	 (:name ,name)
	 (:line-style ,line-style)))

      ;; OPAL:RECTANGLE
      ((equal object opal:rectangle)
       `((:constant (t :except :left :top))
	 (:left ,(o-formula (- (+ (gvl :parent :floating-left)
				  (floor (gvl :parent :gray-outline :width) 2))
			       (floor (gvl :width) 2))))
	 (:top ,(o-formula (- (+ (gvl :parent :floating-top)
				 (floor (gvl :parent :gray-outline :height) 2))
			      (floor (gvl :height) 2))))
	 (:width 50) (:height 20)
	 (:line-style ,(if (equal filling-style opal:black-fill)
			   *white-line-style*
			   opal:default-line-style))
	 (:draw-function ,(if filling-style :copy :xor))
	 (:value ,value)
	 (:name ,name)
	 (:filling-style ,filling-style)))

      ;; OPAL:ROUNDTANGLE
      ((equal object opal:roundtangle)
       `((:constant (t :except :left :top))
	 (:left ,(o-formula (- (+ (gvl :parent :floating-left)
				  (floor (gvl :parent :gray-outline :width) 2))
			       (floor (gvl :width) 2))))
	 (:top ,(o-formula (- (+ (gvl :parent :floating-top)
				 (floor (gvl :parent :gray-outline :height) 2))
			      (floor (gvl :height) 2))))
	 (:width 50) (:height 20)
	 (:filling-style ,filling-style)
	 (:value ,value)
	 (:name ,name)
	 (:draw-function :xor)))


      ;;  OPAL:CIRCLE
      ((equal object opal:circle)
       `((:constant (t :except :left :top))
	 (:left ,(o-formula (- (+ (gvl :parent :floating-left)
				  (floor (gvl :parent :gray-outline :width) 2))
			       (floor (gvl :width) 2))))
	 (:top ,(o-formula (- (+ (gvl :parent :floating-top)
				 (floor (gvl :parent :gray-outline :height) 2))
			      (floor (gvl :height) 2))))
	 (:width 20) (:height 20)
	 (:value ,value)
	 (:name ,name)
	 (:draw-function :xor)))

      ;; GARNET-GADGETS:ARROW-LINE
      ((equal object garnet-gadgets:arrow-line)
       `((:constant (t :except :x1 :y1 :x2 :y2))
	 (:x1 ,(o-formula (- (+ (gvl :parent :floating-left)
				(floor (gvl :parent :gray-outline :width) 2))
			     25)))
	 (:y1 ,(o-formula (+ (gvl :parent :floating-top)
			     (floor (gvl :parent :gray-outline :height) 2))))
	 (:x2 ,(o-formula (+ (gvl :x1) 50)))
	 (:y2 ,(o-formula (gvl :y1)))
	 (:filling-style ,opal:black-fill)
	 (:value ,value)
	 (:name ,name)
	 (:parts
	  ((:line :modify
		  (:draw-function :xor)
		  (:x2 ,(o-formula (- (gv (kr-path 0 :parent :arrowhead)
					  :connect-x) 1))))
	   (:arrowhead :modify 
		       (:line-style nil)
		       (:draw-function :xor))))))
      ;; GARNET-GADGETS:DOUBLE-ARROW-LINE
      ((equal object garnet-gadgets:double-arrow-line)
       `((:constant (t :except :x1 :y1 :x2 :y2))
	 (:x1 ,(o-formula (- (+ (gvl :parent :floating-left)
				(floor (gvl :parent :gray-outline :width) 2))
			     25)))
	 (:y1 ,(o-formula (+ (gvl :parent :floating-top)
			     (floor (gvl :parent :gray-outline :height) 2))))
	 (:x2 ,(o-formula (+ (gvl :x1) 50)))
	 (:y2 ,(o-formula (gvl :y1)))
	 (:filling-style ,opal:black-fill)
	 (:value ,value)
	 (:name ,name)
	 (:parts
	  ((:line :modify
		  (:draw-function :xor)
		  (:x2 ,(o-formula (- (gv (kr-path 0 :parent :arrowhead2)
					  :connect-x) 1))))
	   (:arrowhead1 :modify 
			(:draw-function :xor)
			(:line-style nil))
	   (:arrowhead2 :modify 
			(:line-style nil)
			(:draw-function :xor)))))))))

    ; Construct a list which, when evaluated in the :parts slot of an
    ; aggrelist, will create an instance of a PROPERTIES-BUTTON with the
    ; object displayed as the :label of the button.
    (cons NIL (cons PROPERTIES-BUTTON (list (cons :parts
              (list (list (cons :label (cons object slots))))))))
    ))


(create-instance 'LAPIDARY-MENU-BUTTON-INTER inter:menu-interactor
   (:window (o-formula (gvl :operates-on :window)))
   (:start-where (o-formula (list :element-of (gvl :operates-on))))
   (:how-set :set))

;;
;; **************************************************************************
