;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MGE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|   Mini-Graphical-Editor
     by David S. Kosbie

Changes:
 06/19/94 Marty Geier - changed what I just changed back to what it was because
                        of detrimental effects it had
 06/19/94 Marty Geier - Changed :fast-redraw-p t calls to :rectangle, and
                        took out :draw-function :xor's
 05/30/94 Marty Geier - changed window position in do-go 
 12/7/93 Brad Myers - added color if color screen
 04/10/92 Andrew Mickish - Changed Destroy-Piece-Component to just call
             opal:destroy; added some car's when accessing :is-a slots
 03/25/92 Andrew Mickish - Get-Values ---> G-Value
 10/13/91 Andrew Mickish  Redefined *piece1-copy*, *piece2-copy*, and
             *proto-line* to be unnamed objects.
  4/09/90 Pervin/Cook Changed (delete piece piece-list) to
             (setf piece-list (delete piece piece-list)).
 11/13/89 Ed Pervin  Changed ext::beep to inter:beep and
                     (in-package 'MGE :use '(LISP KR)) to present
                     form.

This file was designed as a companion to the Demo-Othello program.  However,
it has an external interface which allows it to be used by any application.

(mge:do-go)	Starts the Mini-Graphical-Editor
(mge:do-stop)	Stops the Editor
(mge:create-piece left top width height player)
		Creates an instance of one of the two game pieces.  This will
		always be an aggregate.  You can change the top/left/width/
		height of the created instance, and the subcomponents will
		adjust themselves correctly.  The "player" argument should be
		either 1 or 2.
		Furthermore, as edits are made of the two game pieces, all the
		instances will inherit the changes -- even if these changes
		are in the actual structure of the aggregate!
(mge:destroy-piece piece)
		Destroys an instance of a game piece.  You should not destroy
		them yourself -- you should always use one of the supplied
		destroy functions.
(mge:destroy-all-pieces (&optional player))
		Destroys all the instances.  If you supply the optional
		"player" argument (of 1 or 2), then only the instances of
		that game piece will be destroyed.
|#


(in-package :MGE)


(declaim (special PIECE1-AGG PIECE2-AGG BOX2 TOP-AGG W PIECES-AGG
		  DTEXT DBUTTON-INTER OBJMENU-FEEDBACK-OBJ OBJMENU-INTER
		  LINEMENU-FEEDBACK-OBJ LINEMENU-INTER FILLMENU-FEEDBACK-OBJ
		  FILLMENU-INTER FEEDBACK-AGG NEW-OBJ-FEEDBACK NEW-OBJ-INTER
		  SEL-FEEDBACK MAIN-AGG LINE-FEEDBACK INTER1 INTER2
		  PIECE1-AGG PIECE2-AGG BASE-CIRCLE1 BASE-CIRCLE2 LABEL1 LABEL2
		  BOX1 BOX2 PIECES-AGG W TOP-AGG))


(defparameter *window-width*	220)
(defparameter *window-height*	760)

(defparameter *max-piece-size*	180)
(defparameter *piece1-left*	 15)
(defparameter *piece1-top*	 23)
(defparameter *piece2-left*	 15)
(defparameter *piece2-top*	(+ *piece1-top* *max-piece-size* 28))

(defparameter *piece1-copy* NIL)
(defparameter *piece2-copy* NIL)

(defparameter *piece1-list* NIL)
(defparameter *piece2-list* NIL)

(defmacro half(n)
  `(round (/ ,n 2.0)))

(defparameter *shadow-size* 3)
(defparameter *dbutton-width* 75)
(defparameter *dbutton-height* 19)
(defvar *delete-button* NIL)  ;; set later

(defparameter *left-menu-y-indent* 3)
(defparameter *left-menu-x-indent* 5)
(defparameter *menu-item-y-indent* 5)
(defparameter *menu-item-x-indent* 7)
(defparameter *left-menus-width* 80)
(defparameter *left-menu-item-interspacing* 3)
(defparameter *menu-item-height* 30)
(defvar *object-menu* NIL)  ;; set later
(defvar *line-menu* NIL)    ;; set later

(defparameter *right-menu-y-indent* *left-menu-y-indent*)
(defparameter *right-menu-x-indent* 20)
(defparameter *right-menu-item-interspacing* 4)
(defvar *vstripe-fill* NIL)
(defvar *hstripe-fill* NIL)
(defvar *slant1-fill* NIL)
(defvar *slant2-fill* NIL)
(defvar *fill-menu* NIL)

(defvar *fill-choice* NIL)
(defvar *line-choice* NIL)
(defvar *object-choice* NIL)
(defvar *proto-line* NIL)

(defparameter size 7)  ; should be odd
(defparameter sizeD2 (floor size 2))

(defvar feedback-agg NIL) ;; set later

(defvar main-agg NIL)
(defvar sel-feedback NIL)
(defvar line-feedback NIL)
(defvar inter1 NIL)
(defvar inter2 NIL)
(defvar selected-piece-agg NIL)
(defparameter *selection-choice* NIL)

(defun Make-Button (button-contents left top width height)
  (let* ((result (create-instance NIL opal:aggregate
			(:top top)
			(:left left)
			(:width width)
			(:height height)))
	 (frame (create-instance NIL opal:rectangle
		   (:top (o-formula (+ top (if (gv result :interim-selected)
						    *shadow-size*
						    0))))
		   (:left (o-formula (+ left (if (gv result :interim-selected)
						    *shadow-size*
						    0))))
		   (:width (- width *shadow-size*))
		   (:height (- height *shadow-size*))
		   (:filling-style opal:white-fill)))
	 (shadow (create-instance NIL opal:rectangle
		   (:top (+ top *shadow-size*))
		   (:left (+ left *shadow-size*))
		   (:width (- width *shadow-size*))
		   (:height (- height *shadow-size*))
		   (:filling-style opal:black-fill)
		   (:visible (o-formula (not (gv result :interim-selected))))))
	 )
	(s-value button-contents :frame frame)
	(s-value result :frame frame)
	(s-value button-contents :button result)
	(s-value result :contents button-contents)
	(unless (is-a-p button-contents opal:text)
	  (s-value button-contents :width
		(- width *shadow-size* (* 2 *menu-item-x-indent*)))
	  (s-value button-contents :height
		(- height *shadow-size* (* 2 *menu-item-y-indent*))))
	(if (is-a-p button-contents opal:circle)
	  (progn
	   (s-value button-contents :radius
		  (o-formula (min (gvl :width) (gvl :height))))
	   (s-value button-contents :left
	          (o-formula (let* ((frame (gvl :frame))
				    (fleft (gv frame :left))
				    (fwidth (gv frame :width))
				    (mywidth (gvl :radius)))
			       (+ fleft (half (- fwidth mywidth))))))
	   (s-value button-contents :top
	         (o-formula (let* ((frame (gvl :frame))
				   (ftop  (gv frame :top ))
				   (fheight (gv frame :height))
				   (myheight (gvl :radius)))
			      (+ ftop  (half (- fheight myheight)))))))
	  (progn
	   (s-value button-contents :left
	          (o-formula (let* ((frame (gvl :frame))
				    (fleft (gv frame :left))
				    (fwidth (gv frame :width))
				    (mywidth (gvl :width)))
			       (+ fleft (half (- fwidth mywidth))))))
	   (s-value button-contents :top
	         (o-formula (let* ((frame (gvl :frame))
				   (ftop  (gv frame :top ))
				   (fheight (gv frame :height))
				   (myheight (gvl :height)))
			      (+ ftop  (half (- fheight myheight))))))))
	(opal:add-component result shadow)
	(opal:add-component result frame)
	(opal:add-component result button-contents)
	result))

(defun destroy-piece-component (player component)
  (declare (ignore player))
  (opal:destroy component)
  ; amickish 4/10/92 - opal:destroy now destroys instances also
    #|
  (let* ((copy-agg (if (eq player 1) *piece1-copy* *piece2-copy*))
	 (piece-copy-component (car (g-value component :is-a-inv)))
	 (piece-list (list component piece-copy-component))
	 parent)
    (opal:remove-component (if (eq player 1) piece1-agg piece2-agg) component)
    (opal:remove-component copy-agg piece-copy-component)
    (dolist (endpiece-component (g-value piece-copy-component :is-a-inv))
      (push endpiece-component piece-list)
      (if (and (setq parent (g-value endpiece-component :parent))
	       (not (eq parent endpiece-component)))
         (opal:remove-component parent endpiece-component)
	))
    (dolist (thou-must-die piece-list)
	(opal:destroy thou-must-die))
    |#
    )

(defun Dbutton-Stop-Action (interactor obj-under-mouse)
  (call-prototype-method interactor obj-under-mouse)
  (when *selection-choice*
	(s-value sel-feedback :obj-over NIL)
	(destroy-piece-component (if (eq selected-piece-agg piece1-agg) 1 2)
				 *selection-choice*))
  (setq *selection-choice* NIL)
)

(defun Make-Delete-Button ()
  (create-instance 'dtext opal:text (:string "Delete"))
  (setq *delete-button* (make-button dtext
			     (half (- *window-width* *dbutton-width*))
			     (+ (g-value box2 :top) (g-value box2 :height) 5)
			     *dbutton-width*
		             *dbutton-height*))
  (opal:add-component top-agg *delete-button*)
  (create-instance 'dbutton-inter inter:button-interactor
	(:window w)
	(:start-where (list :in-box *delete-button*))
	(:stop-action 'Dbutton-Stop-Action))
)

(defun Make-Menu (contents-list left top width each-height x-indent y-indent
		  menu-item-interspacing)
  (let ((result (create-instance NIL opal:aggregate))
	(real-top top)
	(frame (create-instance NIL opal:rectangle
			(:left   left)
			(:top    top )
			(:width  width)))
	items this-item value-selection-alist)
    (incf top y-indent)
    (incf left x-indent)
    (decf width (* 2 x-indent))
    (dolist (this-content contents-list)
	(opal:add-component result
	  (setq this-item
		(make-button this-content left top width each-height)))
	(push this-item items)
	(push (cons (g-value this-content :value) this-item)
	      value-selection-alist)
	(incf top (+ each-height menu-item-interspacing)))
    (s-value result :items items)
    (s-value result :value-selection-alist value-selection-alist)
    (s-value frame :height (- (+ top (- y-indent
					menu-item-interspacing))
			      real-top))
    (opal:add-component result frame)
    result))

(defun Objmenu-Stop-Action (interactor obj-under-mouse)
 (let ((old-object-choice *object-choice*))
   ;; dzg - changed for KR 1.5.2
  (setq *object-choice* (car (g-value obj-under-mouse :contents :is-a)))
  (call-prototype-method interactor obj-under-mouse)
  (unless (eq old-object-choice *object-choice*)
	(opal:update w))
 )
)

(defun Make-Object-Menu ()
 (let ((menu-top (+ (g-value *delete-button* :top)
		     (g-value *delete-button* :height)
		     5)))
   (setq *object-menu*
     (make-menu
	(list	(create-instance NIL opal:rectangle (:value opal:rectangle))
		(create-instance NIL opal:roundtangle (:value opal:roundtangle))
		(create-instance NIL opal:circle (:value opal:circle))
		(create-instance NIL opal:oval (:value opal:oval)))
	10
	menu-top
	*left-menus-width*
	*menu-item-height*
	*left-menu-x-indent*
	*left-menu-y-indent*
	*left-menu-item-interspacing*))
   (s-value *object-menu* :selected
      (first (g-value *object-menu* :components)))
   (setq *object-choice* (car (g-value *object-menu* :selected :contents :is-a)))
   (create-instance 'objmenu-feedback-obj opal:rectangle
	(:draw-function :xor)  
	(:line-style NIL)
	(:filling-style opal:black-fill)
	;;(:fast-redraw-p T)
	(:visible (o-formula (if (gv *object-menu* :selected) T)))
	(:frame  (o-formula (gv *object-menu* :selected :frame)))
	(:left   (o-formula (gvl :frame :left)))
	(:top    (o-formula (gvl :frame :top)))
	(:width  (o-formula (gvl :frame :width)))
	(:height (o-formula (gvl :frame :height))))
   (opal:add-component *object-menu* objmenu-feedback-obj)
   (opal:add-component top-agg *object-menu*)
   (create-instance 'objmenu-inter inter:menu-interactor
	(:window w)
	(:start-where (list :list-element-of *object-menu* :items))
	(:stop-action #'Objmenu-Stop-Action))
 )
)

(defun LineMenu-Stop-Action (interactor obj-under-mouse)
 (let* ((real-line-style (g-value obj-under-mouse :contents :line-style))
	(new-line-choice (if (eq real-line-style opal:default-line-style)
				NIL
				real-line-style)))
			   
  (if (or *fill-choice* new-line-choice)
    (progn
  	(call-prototype-method interactor obj-under-mouse)
  	(if (and *selection-choice*
	  	 (not (eq (g-value *selection-choice* :line-style)
			  new-line-choice)))
		(s-value *selection-choice* :line-style new-line-choice))
	(unless (eq *line-choice* new-line-choice)
  		(setq *line-choice* new-line-choice)
		(opal:update w)))
    (progn
	(inter:beep)
	(format t "*** Cannot have both Line and Fill Styles set to NIL!~%")
	(funcall 'inter::menu-int-abort-action interactor obj-under-mouse)))
 )
)

(defun Make-Line-Menu ()
 (let ((menu-top (+ (g-value *object-menu* :top)
		     (g-value *object-menu* :height)
		     9)))
   (setq *proto-line* (create-instance NIL opal:line
			 (:x1 (o-formula (gvl :left)))
			 (:y1 (o-formula (+ (gvl :top) (half (gvl :height)))))
			 (:x2 (o-formula (+ (gvl :left) (gvl :width))))
			 (:y2 (o-formula (gvl :y1)))))
   (setq *line-menu*
    (make-menu
	(list	(create-instance NIL opal:text
			(:string "none")
			(:value opal:default-line-style)
			(:line-style opal:default-line-style))
		(create-instance NIL *proto-line*
			(:value opal:dashed-line)
			(:line-style opal:dashed-line))
		(create-instance NIL *proto-line*
			(:value opal::line-0)
			(:line-style opal::line-0))
		(create-instance NIL *proto-line*
			(:value opal:line-2)
			(:line-style opal:line-2))
		(create-instance NIL *proto-line*
			(:value opal:line-4)
			(:line-style opal:line-4)))
	10
	menu-top
	*left-menus-width*
	*menu-item-height*
	*left-menu-x-indent*
	*left-menu-y-indent*
	*left-menu-item-interspacing*))
   (s-value *line-menu* :selected
      (third (g-value *line-menu* :components)))
   (setq *line-choice* (g-value *line-menu* :selected :contents :line-style))
   (create-instance 'linemenu-feedback-obj opal:rectangle
	(:draw-function :xor)
	(:line-style NIL)
	(:filling-style opal:black-fill)
	;;(:fast-redraw-p T)
	(:visible (o-formula (if (gv *line-menu* :selected) T)))
	(:frame  (o-formula (gv *line-menu* :selected :frame)))
	(:left   (o-formula (gvl :frame :left)))
	(:top    (o-formula (gvl :frame :top)))
	(:width  (o-formula (gvl :frame :width)))
	(:height (o-formula (gvl :frame :height))))
   (opal:add-component *line-menu* linemenu-feedback-obj)
   (opal:add-component top-agg *line-menu*)
   (create-instance 'linemenu-inter inter:menu-interactor
	(:window w)
	(:start-where (list :list-element-of *line-menu* :items))
	(:stop-action #'LineMenu-Stop-Action))
 )
)

(defun FillMenu-Stop-Action (interactor obj-under-mouse)
 (let ((new-fill-choice (g-value obj-under-mouse :contents :filling-style)))
  (if (or new-fill-choice *line-choice*)
    (progn
      (call-prototype-method interactor obj-under-mouse)
      (if (and *selection-choice*
	      (not (eq (g-value *selection-choice* :filling-style)
			new-fill-choice)))
	    (s-value *selection-choice* :filling-style new-fill-choice))
      (unless (eq *fill-choice* new-fill-choice)
      		(setq *fill-choice* new-fill-choice)
		(opal:update w)))
    (progn
	(inter:beep)
	(format t "*** Cannot have both Line and Fill Styles set to NIL!~%")
	(funcall 'inter::menu-int-abort-action interactor obj-under-mouse)))
 )
)

(defun Make-Fill-Menu ()
 (let* ((menu-top (g-value *object-menu* :top))
        (menu-left (+ 20 *left-menus-width*))
        (menu-width (- *window-width* menu-left 10)))
   (setq *vstripe-fill* (opal:make-filling-style '((0 1 1 0))))
   (setq *hstripe-fill* (opal:make-filling-style '((0) (1) (1) (0))))
   (setq *slant1-fill*  (opal:make-filling-style '(
	(1 0 0 1)
	(1 1 0 0)
	(0 1 1 0)
	(0 0 1 1))))
   (setq *slant2-fill*  (opal:make-filling-style '(
	(1 0 0 1)
	(0 0 1 1)
	(0 1 1 0)
	(1 1 0 0))))
   (setq *fill-menu*
     (make-menu
	(append (list
		 (create-instance NIL opal:text
			(:string "none")
			(:value NIL)
			(:filling-style NIL))
		 (create-instance NIL opal:rectangle
			(:value opal:white-fill)
			(:filling-style opal:white-fill))
		 (create-instance NIL opal:rectangle
			(:value opal:black-fill)
			(:filling-style opal:black-fill))
		 (create-instance NIL opal:rectangle
			(:value opal:gray-fill)
			(:filling-style opal:gray-fill)))
		(if (g-value opal:color :color-p)
		    (list
		     (create-instance NIL opal:rectangle
			(:value opal:red-fill)
			(:filling-style opal:red-fill))
		     (create-instance NIL opal:rectangle
			(:value opal:blue-fill)
			(:filling-style opal:blue-fill))
		     (create-instance NIL opal:rectangle
			(:value opal:green-fill)
			(:filling-style opal:green-fill))
		     (create-instance NIL opal:rectangle
			(:value opal:purple-fill)
			(:filling-style opal:purple-fill))
		     (create-instance NIL opal:rectangle
			(:value opal:yellow-fill)
			(:filling-style opal:yellow-fill))
		     )
		    (list
		     (create-instance NIL opal:rectangle
			(:value *vstripe-fill*)
			(:filling-style *vstripe-fill*))
		     (create-instance NIL opal:rectangle
			(:value *hstripe-fill*)
			(:filling-style *hstripe-fill*))
		     (create-instance NIL opal:rectangle
			(:value *slant1-fill*)
			(:filling-style *slant1-fill*))
		     (create-instance NIL opal:rectangle
			(:value *slant2-fill*)
			(:filling-style *slant2-fill*))
		     (create-instance NIL opal:rectangle
			(:value opal:diamond-fill)
			(:filling-style opal:diamond-fill)))))
	menu-left
	menu-top
	menu-width
	*menu-item-height*
	*right-menu-x-indent*
	*right-menu-y-indent*
	*right-menu-item-interspacing*))
   (s-value *fill-menu* :selected
	(first (g-value *fill-menu* :components)))
   (setq *fill-choice*
	(g-value *fill-menu* :selected :contents :filling-style))
   (create-instance 'fillmenu-feedback-obj opal:polyline
	(:draw-function :xor)
	(:line-style opal:line-2)
	;;(:fast-redraw-p T)
	(:visible (o-formula (if (gv *fill-menu* :selected) T)))
	(:point-list
	 (o-formula
	  (let* ((selected    (gv *fill-menu* :selected))
		 (right  (- (gv selected :left) 3))
		 (top    (+ (gv selected :top) 2))
		 (bottom (- (+ top (gv selected :height)) 6))
		 (left   (- right 10))
		 (midleft (+ left (half (- right left))))
		 (midtop (+ top (half (- bottom top)))))
	   (list left midtop midleft bottom right top)))))
   (opal:add-component *fill-menu* fillmenu-feedback-obj)
   (opal:add-component top-agg *fill-menu*)
   (create-instance 'fillmenu-inter inter:menu-interactor
	(:window w)
	(:start-where (list :list-element-of *fill-menu* :items))
	(:stop-action #'FillMenu-Stop-Action))
 )
)

(defun fix-piece-component (piece-component)
 (let ((parent (g-value piece-component :parent)))
  (s-value piece-component :left   (o-formula (first (gvl :box))))
  (s-value piece-component :top    (o-formula (second (gvl :box))))
  (s-value piece-component :width  (o-formula (third (gvl :box))))
  (s-value piece-component :height (o-formula (fourth (gvl :box))))
  (s-value piece-component :percents
   (o-formula
    (let* ((agg-left   (gv parent :left))
	   (agg-top    (gv parent :top))
	   (agg-width  (gv parent :width))
	   (agg-height (gv parent :height))
	   (my-box     (gvl :box))
	   (my-left    (first my-box))
	   (my-top     (second my-box))
	   (my-width   (third my-box))
	   (my-height  (fourth my-box)))
	(list (/ (- my-left agg-left) agg-width)
	      (/ (- my-top  agg-top ) agg-height)
	      (/ my-width agg-width)
	      (/ my-height agg-height)))))))

(defun add-to-piece-copy (player new-component)
  (let ((copy-agg (if (eq player 1) *piece1-copy* *piece2-copy*))
	(piece-list (if (eq player 1) *piece1-list* *piece2-list*))
	temp)
     (opal:add-component copy-agg
	(setq temp (create-instance NIL new-component
	  (:percents (o-formula (gv (car (gvl :is-a)) :percents)))
	  (:box
	    (o-formula
	      (let* ((my-parent (gvl :parent))
		     (pleft   (gv my-parent :left))
		     (ptop    (gv my-parent :top ))
		     (pwidth  (gv my-parent :width))
		     (pheight (gv my-parent :height))
		     (percents (gvl :percents)))
		(list (+ pleft (truncate (* (first percents) pwidth)))
		      (+ ptop  (truncate (* (second percents) pheight)))
		      (truncate (* (third percents) pwidth))
		      (truncate (* (fourth percents) pheight)))))))))
				;; Now go add it to all the REAL pieces...
     (dolist (endpiece piece-list)
	(opal:add-component endpiece (create-instance NIL temp)))))

(defun add-piece-component (player new-component)
  (opal:add-component (if (eq player 1) piece1-agg piece2-agg) new-component)
  (fix-piece-component new-component)
  (add-to-piece-copy player new-component))


(defun create-piece (left top width height player)
  (if (eq player 1)
    (unless *piece1-copy* (error "No initialize 1"))  ;;(create-piece-copy 1))
    (unless *piece2-copy* (error "No initialize 2"))) ;;(create-piece-copy 2)))
  (let ((result (create-instance NIL opal:aggregate
			(:player player)
			(:left left)
			(:top top)
			(:width width)
			(:height height)))
	(piece-copy (if (eq player 1) *piece1-copy* *piece2-copy*)))
   (dolist (sub-piece (g-value piece-copy :components))
	(opal:add-component result (create-instance NIL sub-piece)))
   (if (eq player 1)
     (push result *piece1-list*)
     (push result *piece2-list*))
   result))

(defun destroy-piece (piece)
  (let* ((player (g-value piece :player))
	 (piece-list (if (eq player 1) *piece1-list* *piece2-list*)))
    (setf piece-list (delete piece piece-list))
    (opal:destroy piece)))

(defun destroy-all-pieces (&optional player)
  (when (or (null player) (eq player 1))
	(dolist (piece *piece1-list*)
		(opal:destroy piece))
	(setq *piece1-list* NIL))
  (when (or (null player) (eq player 2))
	(dolist (piece *piece2-list*)
		(opal:destroy piece))
	(setq *piece2-list* NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  New Object Interactors ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Create-New-Object (interactor point-list)
 (declare (ignore interactor))
 (if point-list
     (let* ((top (second point-list))
	    (piece2-top (g-value piece2-agg :top))
	    (player (if (< top piece2-top) 1 2)))
       (add-piece-component player
			    (create-instance NIL *object-choice*
			      (:box point-list)
			      (:line-style *line-choice*)
			      (:filling-style *fill-choice*))))))

(defun Make-New-Object-Inter ()
  (opal:add-component top-agg (create-instance 'feedback-agg opal:aggregate))
  (create-instance 'new-obj-feedback opal:rectangle
	(:fast-redraw-p T)
	(:draw-function :xor)
	(:left   (o-formula (first (gvl :box))))
	(:top    (o-formula (second (gvl :box))))
	(:width  (o-formula (third (gvl :box))))
	(:height (o-formula (fourth (gvl :box))))
	(:visible NIL)
	(:box '(0 0 0 0))
	(:line-style opal:dashed-line))
  (opal:add-component feedback-agg new-obj-feedback)
  (create-instance 'new-obj-inter inter:two-point-interactor
	(:window w)
	(:start-where (list :element-of pieces-agg))
	(:start-event :rightdown)
	(:running-where '(:in *))
	(:final-function #'Create-New-Object)
	(:line-p NIL)
	(:feedback-obj new-obj-feedback)
	(:abort-if-too-small T)
	(:Min-width 10)
	(:Min-height 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  Selection Interactors ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Much of this code is stolen from DEMO-GROW.LISP
;;; ********************************************************************
;;; ;;; ** Should use the graphics-selection object from the garnet gadgets set
;;; ** instead of repeating the code here
;;; The main create procedure
;;; ********************************************************************


;;; ** Should use the graphics-selection object from the garnet gadgets set
;;; ** instead of repeating the code here
(defun create-show-selection ()
  (let (obj)
    (create-instance 'sel-feedback opal:aggregate
			 (:name "Top level selection aggregate")
			 (:left (o-formula (- (gvl :obj-over :left) sizeD2) 0))
			 (:resize-rule :never)
			 (:top (o-formula (- (gvl :obj-over :top) sizeD2) 0))
			 (:width (o-formula (+ (gvl :obj-over :width)
						  size) 0))
			 (:height (o-formula (+ (gvl :obj-over :height)
						   size) 0))
			 (:visible (o-formula (gvl :obj-over)))
			 (:obj-over NIL)
			 (:overlapping NIL))
    (do ((x '(l m1 m2 r r r r m2 m1 l l l) (cdr x))
	 (y '(t t t t m1 m2 b b b b m2 m1) (cdr y))
	 (attach '(:nw :n :n :ne :e :e :se :s :s :sw :w :w) (cdr attach))
	 (grow-p '(t nil t t nil t t t nil t t nil) (cdr grow-p)))
	((null x)) ; end test
      (setq obj
	    (create-instance NIL opal:rectangle
		  (:visible (o-formula (gv sel-feedback :obj-over)))
		  (:name "Selection rectangle")
	          (:left (case (car x)
			   (l (o-formula (gv sel-feedback :left)))
			   (m1 (o-formula
				 (+ (gv sel-feedback :left)
				    (floor (gv sel-feedback :width) 3)
				    (- sizeD2))))
			   (m2 (o-formula
				 (+ (gv sel-feedback :left)
				    (floor (* 2 (gv sel-feedback :width)) 3)
				    (- sizeD2))))
			   (r (o-formula (+ (gv sel-feedback :left)
						(gv sel-feedback :width)
						(- size))))))
	          (:top (case (car y)
			  ((t) (o-formula (gv sel-feedback :top)))
			  (m1 (o-formula
			        (+ (gv sel-feedback :top)
				   (floor (gv sel-feedback :height) 3)
				   (- sizeD2))))
			  (m2 (o-formula
			        (+ (gv sel-feedback :top)
				   (floor (* 2 (gv sel-feedback :height)) 3)
				   (- sizeD2))))
			  (b (o-formula (+ (gv sel-feedback :top)
					   (gv sel-feedback :height)
					      (- size))))))
		  (:width size) (:height size)
		  (:where-attach (car attach))
		  (:draw-function :xor)
		  (:grow-p (car grow-p))
		  (:filling-style (if (car grow-p)
				      opal:black-fill NIL))
		  (:line-style (if (car grow-p)
				     NIL opal:thin-line))))
      (opal:add-components sel-feedback obj))
    sel-feedback))

(defun create-objs ()
    (create-instance 'main-agg opal:aggregate)
    (create-show-selection)
    (s-value main-agg :sel-feedback sel-feedback)
    (create-instance 'line-feedback opal:rectangle
			   (:draw-function :xor)
			   (:name "Interim feedback")
			   (:left (o-formula (first (gvl :box))))
			   (:top (o-formula (second (gvl :box))))
			   (:width (o-formula (third (gvl :box))))
			   (:height (o-formula (fourth (gvl :box))))
			   (:visible (o-formula (gvl :obj-over)))
			   (:obj-over NIL)
			   (:box '(0 0 0 0))
			   (:line-style opal:dashed-line)
			   (:filling-style NIL)
			   (:fast-redraw-p T))
    (s-value main-agg :line-feedback line-feedback)
    (opal:add-components main-agg sel-feedback line-feedback)
    main-agg)

;;; ********************************************************************
;;; Main procedures
;;; ********************************************************************

(defun set-selection (menu new-value)
  (let ((selection (cdr (assoc new-value
			       (g-value menu :value-selection-alist)))))
    (if selection (s-value menu :selected selection))))

(defun Process-Selection (obj-selected)
  (setq *object-choice* (car (g-value obj-selected :is-a)))
  (setq *line-choice* (g-value obj-selected :line-style))
  (setq *fill-choice* (g-value obj-selected :filling-style))
  (set-selection *object-menu* *object-choice*)
  (set-selection *line-menu* (or *line-choice* opal:default-line-style))
  (set-selection *fill-menu* *fill-choice*)
  (setq *selection-choice* obj-selected)
)

(defun dims-inside-agg (dims agg)
  (let* ((obj-left   (first dims))
	 (obj-top    (second dims))
	 (obj-right  (+ obj-left (third dims)))
	 (obj-bottom (+ obj-top (fourth dims)))
	 (agg-left   (g-value agg :left))
	 (agg-top    (g-value agg :top))
	 (agg-right  (+ agg-left (g-value agg :width)))
	 (agg-bottom (+ agg-top (g-value agg :height))))
   (and	(>= obj-left agg-left)
	(<= obj-right agg-right)
	(>= obj-top agg-top)
	(<= obj-bottom agg-bottom))))

;;; ** Should use the graphics-selection object from the garnet gadgets set
;;; ** instead of repeating the code here

(defun Make-Selection-Inter()
  (create-objs)
  (opal:add-component top-agg main-agg)
;;First, the button interactor to cause objects to be selected.
  (Create-Instance 'inter1 inter:button-interactor
	(:Window w)
	(:continuous NIL)  ; selected immediately when press
	(:start-where `(:leaf-element-of-or-none ,pieces-agg))
	(:stop-action #'(lambda(interactor objUnderMouse)
			    (declare (ignore interactor))
	     (if (eq :none objUnderMouse)
	       (progn
		(setq *selection-choice* NIL)
		(s-value sel-feedback :obj-over NIL))
	       (progn
		(setq selected-piece-agg (g-value objUnderMouse :parent))
		(s-value pieces-agg :selected-piece-agg `(,selected-piece-agg))
		(Process-Selection objUnderMouse)
		(s-value sel-feedback :obj-over objUnderMouse))))))
  (Create-Instance 'inter2 inter:Move-Grow-Interactor
	(:window w)
	(:continuous T)
	(:waiting-priority inter:high-priority-level)
	(:min-height 10)
	(:min-width 20)
	(:start-where `(:element-of ,sel-feedback))
	(:running-where `(:list-element-of ,pieces-agg :selected-piece-agg))
	(:outside NIL) ; goes back to original position if go outside
	(:feedback-obj line-feedback)
;; the next two fields cause the object to be changed to be the object that the
;; feedback boxes are defined over, rather than the feedback object itself.  The
;; :first-obj-over slot is set by the interactor with the object the mouse
;; pressed on, which will be one of the 8 selection boxes.
	(:obj-to-change (o-formula (gv inter2 :first-obj-over :parent
					  :obj-over)))
	(:attach-point (o-formula (gv inter2 :first-obj-over :where-attach)))
	(:grow-p (o-formula (gv inter2 :first-obj-over :grow-p)))
	(:start-action
	 #'(lambda (interactor objbeingchanged newsize)
	     (Call-Prototype-Method interactor objbeingchanged newsize)
	     (s-value (g-value interactor :first-obj-over) :visible NIL)))
	(:abort-action
	 #'(lambda (interactor objbeingchanged)
	     (s-value (g-value interactor :first-obj-over) :visible T)
	     (Call-Prototype-Method interactor objbeingchanged)
	     ))
	(:stop-action
	 #'(lambda (interactor objbeingchanged newsize)
	     (s-value (g-value interactor :first-obj-over) :visible T)
	     (if (dims-inside-agg newsize selected-piece-agg)
		(progn
			(setq *selection-choice* NIL)
	     		(s-value (g-value (g-value interactor :first-obj-over)
					  :parent)
		      		  :obj-over NIL)
	     		(Call-Prototype-Method interactor objbeingchanged
						newsize))
		(progn
			(inter:beep)
			(funcall 'inter::move-grow-int-abort-action
			  	  interactor objbeingchanged)))
	     )))
)


(defun go-initialize()
  (setq *piece1-copy* (create-instance NIL opal:aggregate
			(:top    *piece1-top*)
			(:left   *piece1-left*)
			(:width  *max-piece-size*)
			(:height *max-piece-size*)))
  (setq *piece2-copy* (create-instance NIL opal:aggregate
			(:top    *piece2-top*)
			(:left   *piece2-left*)
			(:width  *max-piece-size*)
			(:height *max-piece-size*)))
  (create-instance 'piece1-agg opal:aggregate
	(:top    *piece1-top*)
	(:left   *piece1-left*)
	(:width  *max-piece-size*)
	(:height *max-piece-size*))
  (add-piece-component 1
	(create-instance 'base-circle1 opal:circle
			(:line-style opal::line-0)
			(:box `(,*piece1-left* ,*piece1-top*
				,*max-piece-size* ,*max-piece-size*))))
  (create-instance 'piece2-agg opal:aggregate
	(:top    *piece2-top*)
	(:left   *piece2-left*)
	(:width  *max-piece-size*)
	(:height *max-piece-size*))
  (add-piece-component 2
	(create-instance 'base-circle2 opal:circle
			(:box `(,*piece2-left* ,*piece2-top*
				,*max-piece-size* ,*max-piece-size*))
			(:line-style NIL)
			(:filling-style opal:black-fill)))
  (create-instance 'label1 opal:text
	(:left (o-formula
		(+ *piece1-left* (half (- *max-piece-size* (gvl :width))))))
	(:top (o-formula (- *piece1-top* (gvl :height) 5)))
	(:string "Piece 1"))
  (create-instance 'label2 opal:text
	(:left (o-formula
		(+ *piece2-left* (half (- *max-piece-size* (gvl :width))))))
	(:top (o-formula (- *piece2-top* (gvl :height) 5)))
	(:string "Piece 2"))
  (create-instance 'box1 opal:rectangle
	(:left (- *piece1-left* 5))
	(:top  (- *piece1-top* 5))
	(:width (+ *max-piece-size* 10))
	(:height (+ *max-piece-size* 10))
	(:line-style opal:line-4))
  (create-instance 'box2 box1
	(:left (- *piece2-left* 5))
	(:top  (- *piece2-top* 5)))
  (create-instance 'pieces-agg opal:aggregate)
  (opal:add-components pieces-agg piece1-agg piece2-agg)
)

(defun editor-show-window ()
  (create-instance 'w inter:interactor-window
        (:title "Othello Editor")
	(:aggregate (create-instance 'top-agg opal:aggregate))
        (:top 40)
	(:width *window-width*)
	(:height *window-height*))
  (opal:update w)
  (opal:add-components top-agg label1 label2 box1 box2 pieces-agg)

  (Make-Delete-Button)
  (Make-Object-Menu)
  (Make-Line-Menu)
  (Make-Fill-Menu)
  (Make-New-Object-Inter)
  (Make-Selection-Inter))


(defun do-go()
  (go-initialize)
  (editor-show-window)
  T)

(defun do-stop ()
  (opal:destroy w))

(setf (get :garnet-modules :mge) t)
