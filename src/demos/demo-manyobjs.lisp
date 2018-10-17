;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MANYOBJS; Base: 10 -*-


;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.
;;;
;;; This file contains demo code that creates a lot of objects, for
;;; testing if Opal and the Interactors can handle lots of objects.
;;;
;;; Designed and implemented by Brad A. Myers


;;; This is intended as a test and demonstration of the Garnet
;;; system. To start:
;;;
;;; (do-go :number-of-rectangles n)
;;;
;;; To stop:
;;;
;;; (do-stop)


(in-package :demo-manyobjs)


(defvar *test-debug* nil)

;; The window that this will all be in
(defparameter *win* nil)

;; Dummy object
(defparameter *gloswitch* nil)

;; User can change this to opal:roundtangle, etc.
(defparameter *objtype* opal:rectangle)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

(defun x-center-me-in (obj)
  (+ (gv obj :left)
     (floor (- (gv obj :width)(gvl :width)) 2)))

(defun y-center-me-in (obj)
  (+ (gv obj :top)
     (floor (- (gv obj :height)(gvl :height)) 2)))

(create-instance 'boxandarrow opal:aggregadget
  (:line-type opal:thin-line)
  (:fill-type opal:no-fill)
  (:prev-item nil)
  (:name "obj1")
  (:parts
   `((:outline
      ,*objtype*
      (:box ,(o-formula
	      (progn
		;; just set up a dependency
		(gv gloswitch :selected)
		(list
		 (random
		  (- (the integer (g-value win :width)) 50)) ;left
		 (random
		  (- (the integer (g-value win :height)) 50)) ;top
		 50 50))))
      (:left ,(o-formula (first (gvl :box))))
      (:top ,(o-formula (second (gvl :box))))
      (:width 50)
      (:height 50)
      (:line-style ,(o-formula (gvl :parent :line-type)))
      (:filling-style ,(o-formula (gvl :parent :fill-type)))
      (:select-outline-only nil)
      ;; (:draw-function :xor)
      ;; (:fast-redraw-p t)
      )
     (:arrow ,opal:line
	     (:visible ,(o-formula (gvl :parent :prev-item)))
	     (:x1 ,(o-formula (opal:gv-center-x
			       (gvl :parent :outline))))
	     (:y1 ,(o-formula (opal:gv-center-y
			       (gvl :parent :outline))))
	     (:x2 ,(o-formula (opal:gv-center-x
			       (gvl :parent :prev-item :outline))))
	     (:y2 ,(o-formula (opal:gv-center-y
			       (gvl :parent :prev-item :outline))))
	     ;; (:draw-function :xor)
	     ;; (:fast-redraw-p t)
	     )
     (:label ,opal:text
	     (:string ,(o-formula (gvl :parent :name)))
	     (:left
	      ,(o-formula (x-center-me-in (gvl :parent :outline))))
	     (:top
	      ,(o-formula (y-center-me-in (gvl :parent
					       :outline))))
	     ;; (:draw-function :xor)
	     ;; (:fast-redraw-p t)
	     ))))

(defparameter *agg* nil)

(defun do-go (&key (number-of-rectangles 5) dont-enter-main-event-loop
		double-buffered-p)
  "creates number-of-rectangles rectangles attached by lines.  lots of
   formulas. good values of number-of-rectangles are 3..50"
  (let (obj)
    (create-instance 'win inter:interactor-window
      (:left 10) (:top 40)
      (:width 550) (:height 450)
      (:title "garnet many objects") (:icon-title "many")
      (:double-buffered-p double-buffered-p)
      (:aggregate
       (create-instance 'agg opal:aggregate
	 (:left 0)(:top 0)
	 ;; TODO EED, why not use the parent window's width?
	 (:width (o-formula (g-value win :width) 550))
	 (:height (o-formula (g-value win :height) 450)))))
    ;; if we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::garnet-note-quitted)
      (pushnew
       #'(lambda (win)
	   (declare (ignore win))
	   (common-lisp-user::garnet-note-quitted "demo-manyobjs"))
       (g-value win :destroy-hooks)))
    (create-instance 'gloswitch opal:rectangle
      (:left 0)
      (:top 0)
      (:width 20)
      (:height 20)
      (:filling-style opal:gray-fill))
    (opal:add-component agg gloswitch)
    (let (prev)
      (dotimes (i number-of-rectangles)
	(setq obj
	      (create-instance nil boxandarrow
		(:prev-item prev)
		(:name (concatenate 'string "obj" (prin1-to-string i)))))
	(opal:add-component agg obj)
	(setq prev obj)))
    (create-instance 'inter1 inter:move-grow-interactor
      (:start-where `(:leaf-element-of ,agg :type ,*objtype*))
      (:window win))
    (create-instance 'inter2 inter:button-interactor
      (:window win)
      (:start-where `(:in ,gloswitch))
      (:start-event :rightdown)
      (:continuous nil)
      (:how-set :toggle))
    (opal:update win)
    (format
     t
     (str "~%demo-manyobjs:"
	  "press on any object with the left button to start moving"
	  " it. Press on gray rectangle with right button to generate"
	  "new random locations for all objects. execute the"
	  "function (demo-manyobjs:move n) to move an object n"
	  "times (e.g., 100)~%"))
    (unless dont-enter-main-event-loop
      (inter:main-event-loop))))

;; (defun move (n)
;;   (let* ((obj (third (g-value agg :components)))
;; 	 (outline (g-value obj :outline))
;; 	 (box (g-value outline :box)))
;;     (dotimes (i n)
;;       (setf (first box) (* i 4))
;;       (mark-as-changed outline :box)
;;       (opal:update win))))

(defun move (n)
  (let ((outline (g-value (third (g-value agg :components)) :outline)))
    (dotimes (i n)
      (s-value outline :left (* i 4))
      (opal:update win))))

(defun do-stop ()
  (opal:destroy win))
