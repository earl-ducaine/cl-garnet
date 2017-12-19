;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ARITH; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.
;;;
;;; This file is a sample of a visual programming arithmetic expression
;;; editor created with Garnet.
;;;
;;; (demo-arith:Do-Go) to start and (demo-arith:Do-Stop) to stop
;;;
;;; Designed and implemented by Brad A. Myers

(in-package :demo-arith)

;;;  Load text-buttons-loader, graphics-loader, and arrow-line-loader
;;;  unless already loaded
(defvar DEMO-ARITH-INIT
  (progn
    ;;;  Load ps-loader.
    (common-lisp-user::garnet-load "ps:ps-loader")
    (dolist (gadget '(;; "text-buttons-loader"
		      "arrow-line-loader"
		      "scrolling-window-loader"))
      (common-lisp-user::garnet-load (concatenate 'string "gadgets:" gadget)))
    ;;; Load gesture-loader
    (common-lisp-user::garnet-load "gestures:gesture-loader")))


;;; Global variables

(defparameter *Mode-Menu* NIL) ; menu of object types to create
(defparameter *Selection-Obj* NIL) ; the object that holds the selection
(defparameter *Objs-Agg* NIL) ; aggregate to hold the created objects

(declaim (special TEXT-EDIT NUMBER-BOX ARITH-BOX PLUS-BOX MINUS-BOX
		  TIMES-BOX DIVIDE-BOX TOP-WIN SCROLL-WIN TOP-AGG
		  MYARROWLINE MYLINEFEEDBACK))

;;; Utility Functions

(defun Init-Slot (obj slot new-val)
  ;; need to do this to set up the dependencies
  (g-value obj slot)
  (s-value obj slot new-val))

; convert s to an integer or return NIL
(defun Make-Number (s)
  (let* ((sym (read-from-string s))
     (number (when (numberp sym) sym)))
    number))

; check if a function
(defun My-Function-p (s)
  (if (symbolp s)
      ; need all 3 tests to do it right!
      (fboundp s)
      (functionp s)))

(defun Protected-Divide (&rest args)
  (if (null args)
      '**
      (progn
    (dolist (i args)
      (when (zerop i)
        (return-from Protected-Divide '**)))
    (apply '/ args))))

;;; First create the prototypes for the box and lines
(create-instance 'MYARROWLINE garnet-gadgets:arrow-line
  ;; set this with the object this arrow is from
  (:from-obj NIL)
  ;; set this with the object this arrow is from
  (:to-obj NIL)
  ;;  (:x1 (o-formula (opal:gv-right (gvl :from-obj))))
  ;;  (:y1 (o-formula (opal:gv-center-y (gvl :from-obj))))
  ;;  (:x2 (o-formula (gvl :to-obj :left)))
  ;;  (:y2 (o-formula (opal:gv-center-y (gvl :to-obj))))
  (:xy1 (o-formula (xy-obj-edge (gvl :from-obj) (gvl :to-obj) (gvl :xy1))
		   (list 0 0)))
  (:xy2 (o-formula (xy-obj-edge (gvl :to-obj) (gvl :from-obj) (gvl :xy2))
		   (list 0 0)))
  (:x1 (o-formula (first (gvl :xy1))))
  (:y1 (o-formula (second (gvl :xy1))))
  (:x2 (o-formula (first (gvl :xy2))))
  (:y2 (o-formula (second (gvl :xy2))))
  (:value (o-formula (gvl :from-obj :value)))
  (:open-p NIL)
  (:visible (o-formula (and (gvl :from-obj)(gvl :to-obj))))
  ;; so that the selection object will know what kind this is
  (:line-p T))

;;; Potentially, we want to be able to use this in macros.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; compute the diameter of a circle that enscribes a rectangles with
  ;; both figures having coincident centers, and width and height
  ;; being the demensions of the rectangle.
  (defun compute-diameter (width height)
    (let* ((padding (/ width 3))
	   (x (+ padding (/ width 2)))
	   (y (+ padding (/ height 2)))
	   (result (floor (sqrt (+ (expt x 2) (expt y 2))))))
      (format t "width: ~s, height: ~s, result:~s~%"
	      (expt x 2)
	      (expt y 2)
	      result)
      result)))

(create-instance 'arith-box opal:aggregadget
  (:box (list 20 20 NIL NIL)) ; this will be set by the
  ;; interactors with the position of this object.
  (:left (o-formula (first (gvl :box))))
  (:top (o-formula (second (gvl :box))))
  (:width 30) (:height 30)
  (:lines-to-me NIL)   ;Keep track of lines pointing
  (:lines-from-me NIL) ;to me, in case I am deleted.
  (:editable NIL)
  ;; so that the selection object will know what kind this is
  (:line-p NIL)
  ;; the function to execute or numerical value
  (:func T)
  (:func-to-execute (o-formula (gvl :func)))
  (:string (o-formula (symbol-name (gvl :func))))
  (:value (o-formula (let ((func (gvl :func-to-execute))
			   in-vals val final-val)
		       (dolist (i (gvl :lines-to-me))
			 (setq val (gv i :value))
			 (if (numberp val)
			     (push val in-vals)
			     (setq final-val '**)))
		       (or final-val
			   (and in-vals (if (my-function-p func)
					    (apply func in-vals)))
			   '**))))
  (:parts
   `((:frame ,opal:circle
	     (:left ,(o-formula (first (gvl :parent :box))))
	     (:top ,(o-formula (second (gvl :parent :box))))
	     ;; Make the circle diameter large enough to enscribe the
	     ;; box in which the symbol sits.
	     (:diam ,(o-formula  (compute-diameter (gvl :parent :width)
						   (gvl :parent :height))))
	     ;; (:diam ,(o-formula (max (gvl :parent :width)
	     ;; 			     (gvl :parent :height))))
	     (:width ,(o-formula (gvl :diam)))
	     (:height ,(o-formula (gvl :diam))))
     (:label ,opal:text
	     (:string ,(o-formula (gvl :parent :string) ""))
	     (:actual-heightp T)
	     (:font ,(create-instance NIL opal:font
		       (:size :very-large) (:face :bold)))
	     ;;center me in the box
	     (:left ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
	     (:top ,(o-formula (opal:gv-center-y-is-center-of (gvl :parent))))))))

(create-instance 'plus-box arith-box (:func '+))
(create-instance 'minus-box arith-box (:func '-))
(create-instance 'times-box arith-box (:func '*))
(create-instance 'divide-box arith-box
   (:func '/)
   (:func-to-execute 'Protected-Divide))

(create-instance 'number-box arith-box
   (:editable (o-formula (null (gvl :lines-to-me))))
   (:width (o-formula (+ 10 (gvl :label :width))))
   (:height (o-formula (+ 10 (opal:string-height
                  (gvl :label :font)
                  "0" :actual-heightp T))))
   (:string (o-formula (let ((val (gvl :value)))
             (if (numberp val)
                 (if (integerp val)
                 (format NIL "~a" val)
                 ; else floating point
                 (if (zerop val)
                     "0.0"
                     (format NIL "~0,2F" val)))
                 ; else not a number
                 (symbol-name val)))))
   (:func 0) ; set by text-interactor if string value edited
   (:value (o-formula (let ((in-lines (gvl :lines-to-me)))
            (cond ((null in-lines) (gvl :func))
                  ((> (length in-lines) 1) ">1")
                  (T (gv (car in-lines) :value))))))
   (:parts `((:frame ,opal:roundtangle
          (:width ,(o-formula (gvl :parent :width)))
          (:height ,(o-formula (gvl :parent :height)))
          :inherit (:left :top :line-style :filling-style))
         (:label ,opal:cursor-text
          (:cursor-index NIL)
          :inherit (:string :font :left :top)))))

(defun Set-String-Value (inter obj event string x y)
  (declare (ignore event x y))
  (let (num)
  (if (and (g-value obj :parent :editable)
       (setq num (Make-Number string)))
      (s-value (g-value obj :parent) :func num)
      ; else bad number or can't be edited
      (progn
    (inter:beep)
    (s-value obj :string (g-value inter :original-string))
    (inter:abort-interactor inter)))))


;;;------------------------------------------------------------------------
;;;Create main menu object
;;;------------------------------------------------------------------------

;; Create an arrow, a number-box and one of each kind of operator, and put
;; them in a menu, with an
;; interactor and feedback object to show which is selected.
;; Agg is the top level aggregate to put the menu in, and window is the window.
(defun create-mode-menu (agg window)
    (setq *Mode-Menu*
      (create-instance NIL opal:aggregadget
         (:selected (o-formula (gvl :items :selected)))
         (:line-p (o-formula (gvl :selected :line-p)))
         (:parts
          `((:items ,opal:aggregadget
	     (:parts
	      ((:plus ,plus-box (:box (20 10 NIL NIL))
		      (:constant T))
	       (:minus ,minus-box (:box (55 10 NIL NIL))
		       (:constant T))
	       (:times ,times-box (:box (90 10 NIL NIL))
		       (:constant T))
	       (:divide ,divide-box (:box (125 10 NIL NIL))
			(:constant T))
	       (:number ,number-box (:box (20 60 NIL NIL))
			(:constant T))
	       (:arrow ,garnet-gadgets:arrow-line
		       (:constant T)
		       (:x1 20)(:y1 130)(:x2 140)(:y2 130)
		       (:line-p T)
		       (:open-p NIL)
		       (:point-in-gob
			,(g-value opal:aggregate :point-in-gob))))))
	    (:feedback ,opal:rectangle
	     (:line-style NIL)
	     (:obj-over NIL)
	     (:filling-style ,opal:black-fill)
	     (:left ,(o-formula (- (gvl :obj-over :left) 4)))
	     (:top ,(o-formula (- (gvl :obj-over :top) 4)))
	     (:width ,(o-formula (+ (gvl :obj-over :width) 8)))
	     (:height ,(o-formula (+ (gvl :obj-over :height) 8)))
	     (:visible ,(o-formula (gvl :obj-over)))
	     (:draw-function :xor)
	     (:fast-redraw-p T))))
         (:interactors
          `((:select-it ,inter:button-interactor
	     (:continuous NIL)
	     (:window ,window)
	     (:how-set :set)
	     (:start-where ,(o-formula (list :element-of
					     (gvl :operates-on :items))))
	     (:start-event :any-mousedown)
	     (:final-feedback-obj
	      ,(o-formula (gvl :operates-on :feedback))))))))
    (opal:add-component agg *Mode-Menu*)
    (let ((init-val (g-value *Mode-Menu* :items :number)))
      (Init-Slot *Mode-Menu* :selected init-val)
      (Init-Slot (g-value *Mode-Menu* :feedback) :obj-over init-val)))

;;This creates the menu of commands.
;;The menu is stored into the aggregate agg.  Returns the menu created.
(defun create-menu (agg)
  (let ((menu (create-instance NIL Garnet-gadgets:Text-Button-Panel
		(:constant T)
		(:items '(("Delete" Delete-Object)
			  ("Clear Workspace" Delete-All)
			  ("PostScript Contents" PostScript-Contents)
			  ("PostScript Window" PostScript-Window)
			  ("Quit" Do-Quit)))
		(:left 10) (:top 175)
		(:font opal:default-font)
		(:shadow-offset 5)
		(:final-feedback-p NIL))))
    (opal:add-components agg menu)
    menu))

;;;********************************************************************
;;;Create a selection object and the interactors to manipulate it.
;;; Also, allow objects to be moved
;;;********************************************************************

(defun Create-Selection-Obj (agg-to-put-it-in window)
  (setq *Selection-Obj*
    (create-instance NIL opal:aggregadget
       (:obj-over NIL)
       (:visible (o-formula (gvl :obj-over)))
       (:line-p (o-formula (gvl :obj-over :line-p)))
       (:parts
        `((:rect ,opal:rectangle
           (:line-style NIL)
           (:obj-over ,(o-formula (if (gvl :parent :line-p)
                      NIL
                      (gvl :parent :obj-over))))
           (:filling-style ,opal:black-fill)
           (:left ,(o-formula (- (gvl :obj-over :left) 2)))
           (:top  ,(o-formula (- (gvl :obj-over :top) 2)))
           (:width  ,(o-formula (+ (gvl :obj-over :width) 4)))
           (:height ,(o-formula (+ (gvl :obj-over :height) 4)))
           (:visible ,(o-formula (gvl :obj-over)))
           (:draw-function :xor))
          (:line ,opal:line
           (:obj-over ,(o-formula (when (gvl :parent :line-p)
                    (gvl :parent :obj-over))))
           (:line-style ,(create-instance NIL opal:line-style
			   (:constant T)
			   (:line-thickness 11)))
           (:x1 ,(o-formula (gvl :obj-over :x1)))
           (:y1 ,(o-formula (gvl :obj-over :y1)))
           (:x2 ,(o-formula (gvl :obj-over :x2)))
           (:y2 ,(o-formula (gvl :obj-over :y2)))
           (:visible ,(o-formula (gvl :obj-over)))
           (:draw-function :xor))))))
  (opal:add-component agg-to-put-it-in *Selection-Obj*)

  (create-instance 'SELECTOR inter:move-grow-interactor
     (:window window)
     (:start-where `(:element-of ,*objs-agg* :type ,arith-box))
     (:feedback-obj NIL)
     (:final-function
      #'(lambda (inter obj points)
      (declare (ignore inter points))
      (s-value *Selection-Obj* :obj-over obj))))
  (create-instance 'HELP-SELECTION inter:button-interactor
     (:window window)
     (:start-where `(:element-of-or-none ,*objs-agg*))
     (:feedback-obj NIL)
     (:continuous NIL)
     (:final-function
      #'(lambda (inter obj)
      (declare (ignore inter))
      (unless (is-a-p obj arith-box) ; then taken care of
                    ; by other interactor
        (s-value *Selection-Obj* :obj-over (if (eq obj :none)
                           NIL
                           obj)))))))

;;;********************************************************************
;;;Procedures to do the work
;;;********************************************************************

; xy-obj-edge returns a list that contains the x and y coordinate of the
; point that lies on the edge of obj1 and on the line that goes from the
; center of obj1 to obj2.
;
; Parameters:
;     obj1 - object we are drawing from
;     obj2 - object we are drawing to
;     xy   - list of points we are calculating... (destructive)
(defun xy-obj-edge (obj1 obj2 xy)
  (let* ((xradius (- (opal:gv-right obj1) (opal:gv-center-x obj1)))
	 (yradius (- (opal:gv-bottom obj1) (opal:gv-center-y obj1)))
	 (deltax (- (opal:gv-center-x obj2) (opal:gv-center-x obj1)))
	 (deltay (- (opal:gv-center-y obj2) (opal:gv-center-y obj1)))
	 (dist (isqrt (+ (* deltax deltax) (* deltay deltay))))
	 (x-change 0)
	 (y-change 0))

    (if (zerop dist)
	(progn
	  (setf x-change 0)       ; if centers on top of each other
	  (setf y-change 0))
				  ; else normal case
	(if (not (is-a-p obj1 number-box))
            ; for circular objects:
  	    ;   theta = asin (abs (deltay) / dist)
            ;   x-change = xradius * cos (theta)
            ;   y-change = yradius * sin (theta)
	    ;   Use similar triangles instead!!!
	    (progn
	      (setf x-change (round (* deltax xradius) dist))
	      (setf y-change (round (* deltay yradius) dist)))

	    ; else for a rectangle:
  	    ;   the triangle we need sides of is similar triangle to triangle
	    ;   with sides: line between centers, deltax, and deltay. assume
	    ;   intersect is on top or bottom edge, so that the
	    ;   y-intersect = yradius. if x-change > xradius then assumpition
	    ;   is wrong and intersect is on left or right edge, so
	    ;   x-intersect = xradius. Do similarly for y-intersect.
	    (progn
	      (cond ((zerop deltay)       ; aligned horizontally
		     (setf x-change xradius)
		     (setf y-change 0))

		    ((zerop deltax)       ; aligned vertically
		     (setf x-change 0)
		     (setf y-change yradius))

		    ((<= (setf y-change
			       (abs (round (* deltay xradius) deltax)))
			 yradius)         ; goes through left or right
		     (setf x-change xradius))

		    (t                    ; goes through top or bottom
		     (setf x-change (abs (round (* deltax yradius) deltay)))
		     (setf y-change yradius)))

	      (when (< deltax 0) (setf x-change (- 0 x-change)))
	      (when (< deltay 0) (setf y-change (- 0 y-change))))))

    (setf (first xy) (+ (opal:gv-center-x obj1) x-change))
    (setf (second xy) (+ (opal:gv-center-y obj1) y-change))
    xy))                  ; return the point


;;;Delete-Line is called from delete object to delete lines
(defun Delete-Line(line-obj)
  (let ((from-obj (g-value line-obj :from-obj))
    (to-obj (g-value line-obj :to-obj)))
    ;;remove this line from the boxes' lists
    (s-value from-obj :lines-from-me
         (remove line-obj (g-value from-obj :lines-from-me)))
    (s-value to-obj :lines-to-me
         (remove line-obj (g-value to-obj :lines-to-me)))
    (opal:destroy line-obj)))

;;;Delete-object is called from the main menu routine
(defun Delete-Object (g v)
  (declare (ignore g v))
  (let ((selected-obj (g-value *Selection-Obj* :obj-over)))
    (if selected-obj
      (progn
    ;;first turn off selection
    (s-value *Selection-Obj* :obj-over NIL)
    (inter:abort-interactor text-edit) ; just in case running
    ;;now delete object
    (if (g-value selected-obj :line-p)
        ;;then deleting a line
        (Delete-Line selected-obj)
        ;;else deleting a box
        (progn
          ;;first delete all lines to this box
          (dolist (line-at-box (g-value selected-obj :lines-from-me))
        (delete-line line-at-box))
          (dolist (line-at-box (g-value selected-obj :lines-to-me))
        (delete-line line-at-box))
          ;;now delete the box
          (opal:destroy selected-obj))))
    ;;else nothing selected
    (inter:beep))))

(defun Delete-All (g v)
  (declare (ignore g v))
  (s-value *Selection-Obj* :obj-over NIL)
  (dolist (obj (copy-list (g-value *objs-agg* :components)))
    (opal:destroy obj)))

(defun Do-Quit (g v)
  (declare (ignore g v))
  (opal:destroy TOP-WIN)
  ;;for demo-controller
  (unless (and (fboundp 'common-lisp-user::Garnet-Note-Quitted)
	       (common-lisp-user::Garnet-Note-Quitted "DEMO-ARITH"))))

(defun PostScript-Window (g v)
  (declare (ignore g v))
  (s-value SCROLL-WIN :top 0)
  (opal:make-ps-file TOP-WIN "arith-window.ps" :landscape-p T)
  (s-value SCROLL-WIN :top -2))

(defun PostScript-Contents (g v)
  (declare (ignore g v))
  ;; Deselect the feedback object before printing, then restore
  (let ((selected-obj (g-value *Selection-Obj* :obj-over)))
    (s-value *Selection-Obj* :obj-over NIL)
    (opal:make-ps-file (g-value SCROLL-WIN :inner-window)
               "arith-contents.ps")
    (s-value *Selection-Obj* :obj-over selected-obj)))

;;;Create a new object.  Get the type of object to create from the *mode-menu*.
;;;This procedure is called as the final-function of the two-point interactor.
(defun Create-New-Obj (inter point-list)
  (declare (ignore inter))
  (let ((line-p (g-value *Mode-Menu* :line-p))) ;create a line or rectangle

    (if line-p
    ;;then create a line, first have to find the objects where the line
    ;; is drawn
    (let ((from-box (opal:point-to-component *objs-agg* (first point-list)
                      (second point-list) :type arith-box))
          (to-box (opal:point-to-component *objs-agg* (third point-list)
                      (fourth point-list) :type arith-box))
          new-line)
      ;;If one end of the arrow is not inside a box, or is from and to
      ;; the same box, or if more than one to a number box, then beep.
      (if (or (null from-box)(null to-box)(eq from-box to-box)
          (and (is-a-p to-box number-box)
               (g-value to-box :lines-to-me))) ; if already exists a
                               ; line to that box
          (inter:beep)
          ;; else draw the arrow.
          (progn
        (setf new-line (create-instance NIL myarrowline
                        (:from-obj from-box)
                        (:to-obj to-box)))
        ;;keep track in case boxes are deleted so can delete this line.
        (push new-line (g-value from-box :lines-from-me))
        (push new-line (g-value to-box :lines-to-me))

        (opal:add-component *objs-agg* new-line))))
    ;;else, create a new box
    (let* ((typ (car (g-value *Mode-Menu* :selected :is-a)))
           (new-obj (create-instance NIL typ
           (:box (copy-list point-list))))) ;have to copy this list
      (opal:add-component *objs-agg* new-obj)))))


;;; Mode-Menu-Select changes the selection on the *Mode-Menu*
;;; from the current selection to the given item.
;;;
(defun Mode-Menu-Select (item)
    ;; unselect the current menu item if there is one selected
    (if (g-value *Mode-Menu* :items :selected)
        (s-value (g-value *Mode-Menu* :items :selected) :selected NIL)
    )

    ;; select the new item
    (s-value (g-value *Mode-Menu* :items item) :selected T)
    (s-value (g-value *Mode-Menu* :items)
             :selected (g-value *Mode-Menu* :items item))

    ;; turn on the feedback for the new item
    (s-value (g-value *Mode-Menu* :feedback)
             :obj-over (g-value *Mode-Menu* :items item))
)


;;; Interprets the gesture and either creates a new object or
;;; performs a command (i.e. delete.)
;;;
(defun handle-gesture (inter first-obj-over class-name attribs
                       points nap dist)
    (declare (ignore inter first-obj-over points dist nap))
;;    (format T "~s with probability of ~s and distance of ~s~%~%"
;;            class-name nap dist)

    (let ((new-obj nil))
        (case class-name
            (:NUMBER-BOX
                (setf new-obj
                      (create-instance NIL number-box (:func 0)
                          (:box (list (inter:gest-attributes-minx attribs)
                                      (inter:gest-attributes-miny attribs)
                                      NIL NIL))))
                (opal:add-component *objs-agg* new-obj)

                ;; make the new number-box and the menu item selected
                (s-value *Selection-Obj* :obj-over new-obj)
                (Mode-Menu-Select :number)
            )
            (:PLUS
                (setf new-obj
                      (create-instance NIL plus-box (:func '+)
                          (:box (list (inter:gest-attributes-minx attribs)
                                      (inter:gest-attributes-miny attribs)
                                      NIL NIL))))
                (opal:add-component *objs-agg* new-obj)

                ;; make the new plus-box and the menu item selected
                (s-value *Selection-Obj* :obj-over new-obj)
                (Mode-Menu-Select :plus)
            )
            (:TIMES
                ; check if the bounding box of this gesture entirely
                ; covers the bounding box of a box or arrow. If not,
                ; this is a create times box gesture,
                ; otherwise this is a delete operation.
                (let ((to_delete (opal:components-in-rectangle
                                    *objs-agg*
                                    (inter:gest-attributes-miny attribs)
                                    (inter:gest-attributes-minx attribs)
                                    (inter:gest-attributes-maxy attribs)
                                    (inter:gest-attributes-maxx attribs)
                                    :intersect T)))
                    (if (null to_delete)
                        (progn      ; create multiply
                            (setf new-obj
                                  (create-instance NIL times-box (:func '*)
                                      (:box (list
                                        (inter:gest-attributes-minx attribs)
                                        (inter:gest-attributes-miny attribs)
                                        NIL NIL))))
                            (opal:add-component *objs-agg* new-obj)

                            ;; make the new times-box and the
                            ;; menu item selected
                            (s-value *Selection-Obj* :obj-over new-obj)
                            (Mode-Menu-Select :times)
                        )
                                    ; else delete gesture
                        (dolist (cur to_delete)
                            ; only delete object that haven't been deleted
                            (when (kr:schema-p cur)
                                (s-value *Selection-Obj* :obj-over cur)
                                (Delete-Object NIL NIL)
                            )
                        )
                    )
                )
            )
            ((:MINUS :ARROW :DIVIDE)
                ; first have to find the objects where the line is drawn
                (let ((from-box (opal:point-to-component *objs-agg*
                                    (inter:gest-attributes-startx attribs)
                                    (inter:gest-attributes-starty attribs)
                                    :type arith-box))
                      (to-box (opal:point-to-component *objs-agg*
                                  (inter:gest-attributes-endx attribs)
                                  (inter:gest-attributes-endy attribs)
                                  :type arith-box))
                      new-line)

                    ; if neither end is inside a box, then either NOT an
                    ; arrow or a BAD arrow, otherwise treat as an arrow.
                    (if (and (null from-box) (null to-box))
                        (case class-name
                            (:MINUS
                                (setf new-obj
                                      (create-instance NIL minus-box (:box
                                       (list
                                        (inter:gest-attributes-minx attribs)
                                        (inter:gest-attributes-miny attribs)
                                        NIL NIL))))
                                (opal:add-component *objs-agg* new-obj)

                                ;; make the new minus-box and the menu
                                ;; item selected
                                (s-value *Selection-Obj* :obj-over new-obj)
                                (Mode-Menu-Select :minus)
                            )
                            (:DIVIDE
                                (setf new-obj
                                      (create-instance NIL divide-box (:box
                                       (list
                                        (inter:gest-attributes-minx attribs)
                                        (inter:gest-attributes-miny attribs)
                                        NIL NIL))))
                                (opal:add-component *objs-agg* new-obj)

                                ;; make the new divide-box and the menu
                                ;; item selected
                                (s-value *Selection-Obj* :obj-over new-obj)
                                (Mode-Menu-Select :divide)
                            )
                            (:ARROW
                                (inter:beep)
                            )
                        )

                        ; else, if one end of the arrow isn't in a box,
                        ; or if one end of the arrow is from and to the
                        ; same box, or if more than one to a number box,
                        ; then beep
                        (if (or (null from-box) (null to-box)
                                (eq from-box to-box)
                                (and (is-a-p to-box number-box)
                                     (g-value to-box :lines-to-me)))
                                     ; if already exists a line to that box
                            (inter:beep)
                            ; else draw the arrow
                            (progn
                                (setf new-line
                                      (create-instance NIL myarrowline
                                                       (:from-obj from-box)
                                                       (:to-obj to-box)))
                                ; keep track in case boxes are deleted so
                                ; can delete this line
                                (push new-line
                                      (g-value from-box :lines-from-me))
                                (push new-line
                                      (g-value to-box :lines-to-me))
                                (opal:add-component *objs-agg* new-line)
                            )
                        )
                    )
                )
            )
            (otherwise
                (format T "unrecognized gesture...~%~%")
            )
        )
    )
)


;;;********************************************************************
;;;Main procedures
;;;********************************************************************

(defun Do-Go (&key dont-enter-main-event-loop (double-buffered-p T))
  (let (work-win work-agg)
    ;; Create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
       (:double-buffered-p double-buffered-p)
       (:left 280) (:top 120)
       (:width 700) (:height 400)
       (:title "GARNET Arithmetic Editor")
       (:icon-title "Arith"))

    ;; Create the top level aggregate in the window
    (s-value TOP-WIN
	     :aggregate
	     (create-instance 'TOP-AGG opal:aggregate
	       (:left 0)(:top -2)
	       (:width (o-formula (gvl :window :width)))
	       (:height (o-formula (gvl :window :height)))))
    (opal:update TOP-WIN)

    ;; If we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
      (pushnew
       #'(lambda (win)
	   (declare (ignore win))
	   (common-lisp-user::Garnet-Note-Quitted "DEMO-ARITH"))
       (g-value top-win :destroy-hooks)))


    ;; Create window for the work area
    (create-instance 'SCROLL-WIN garnet-gadgets:scrolling-window-with-bars
       (:left 175) (:top -2) ;no extra border at the top
       (:width (o-formula (- (gvl :window :parent :width) (gvl :left))))
       (:height (o-formula (gvl :window :parent :height) 150))
       (:total-width 1000) (:total-height 1000)
       (:scroll-on-left-p NIL)
       (:double-buffered-p double-buffered-p)
       (:border-width 2)
       (:parent-window TOP-WIN))
    (opal:update SCROLL-WIN)

    (setq work-agg (g-value SCROLL-WIN :inner-aggregate))
    (setq work-win (g-value SCROLL-WIN :inner-window))

    ;;;create an aggregate to hold the user-created objects
    (setq *objs-agg* (create-instance NIL opal:aggregate
            (:left 0)(:top 0)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))
    (opal:add-component work-agg *objs-agg*)

    ;;;create menus
    (create-mode-menu TOP-AGG TOP-WIN)
    (create-menu TOP-AGG)

    ;;;create a graphics selection object
    (Create-Selection-Obj work-agg work-win)

    ;;;Create an interactor to edit the text of the labels
    (create-instance 'TEXT-EDIT inter:text-interactor
       (:active (o-formula (and (gv *selection-obj* :obj-over)
				(gv *selection-obj* :obj-over :editable))))
       (:start-event :any-keyboard)
       (:start-where T)
       (:obj-to-change (o-formula (gv *selection-obj* :obj-over :label)))
       (:stop-event '(:any-mousedown #\return))
       (:window work-win)
       (:start-action #'(lambda (inter obj event)
			  (call-prototype-method inter obj event)
			  (s-value obj :string
				   (make-string 1 :initial-element
						(inter:event-char event)))
			  (s-value obj :cursor-index 1)))
       (:final-function 'Set-String-Value))

    (create-instance 'MYLINEFEEDBACK opal:line
       (:points (list 0 0 10 10))
       (:obj-over NIL)
       (:visible (o-formula (gvl :obj-over)))
       (:x1 (o-formula (first (gvl :points))))
       (:y1 (o-formula (second (gvl :points))))
       (:x2 (o-formula (third (gvl :points))))
       (:y2 (o-formula (fourth (gvl :points))))
       (:draw-function :xor)
       (:fast-redraw-p T)
       (:line-style opal:dashed-line))
    (opal:add-component work-agg MYLINEFEEDBACK)

    ;;;create an interactor to create the new objects
    (create-instance 'CREATOR inter:two-point-interactor
       (:start-event :rightdown)
       (:line-p (o-formula (gv *Mode-Menu* :line-p)))
       (:continuous (o-formula (gvl :line-p))) ; if line-p then continous
       (:feedback-obj MYLINEFEEDBACK)
       (:start-where T)
       (:window work-win)
       (:final-function #'Create-New-Obj))

    ;;; create a gesture interactor to allow a different way
    ;;; to make new objects
    (create-instance 'GESTURE-CREATOR inter:gesture-interactor
       (:start-event :middledown)
       (:start-where T)
       (:window work-win)
       (:classifier (inter:gest-classifier-read
                        (merge-pathnames "demo-arith.classifier" common-lisp-user::Garnet-Gesture-Data-Pathname)))
       (:final-function #'handle-gesture)
       (:max-dist-to-mean 20)
       (:min-non-ambig-prob .4)
    )


    ;;;Now, add the aggregates to the window and update
    (opal:update TOP-WIN)  ;;will also update work-win

  ;;** Do-Go **
    (Format T "~%Demo-Arith:
  Press with left button on top menu to change modes (box or line).
  Press with left button on bottom menu to execute a command.
  Press with right button in work window to create a new object
        of the current mode.
  Boxes can be created anywhere, but lines must start and stop inside boxes.
  Press with left button on text string to start editing that string.
        While editing a string, type RETURN or press a mouse button to stop.
  Press with left button in work window to select an object,
        continue to hold and move to move the object.
  While moving a box or typing a string, hit ^G or ^g to abort.
  Pressing the print buttons will generate postscript files named
        arith-window.ps and arith-contents.ps in the current working directory.

  By pressing the middle button, some simple single-pathed gestures can
  also be used to create and delete objects. In addition to those shown
  below, an arrow can be indicated by drawing a line between two objects.

     NUMBER           PLUS          MINUS       TIMES          DIVIDE
                                               (DELETE)

    /------             |                      \\     /              /
    |      \\            |         ---------     \\   /              /
    |      |            |                        \\ /              /
    |      |        ---------                     X              /
    |      |            |  /                     / \\            /
    \\      |            | /                     /   \\          /
     -----/             |/                     /     \\        /
                                               -------


  ~%")

    )

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )

(defun Do-Stop ()
  (when (schema-p *DRAW-AGG*)
    (opal:destroy TOP-WIN)))
