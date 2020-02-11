;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;


(in-package :opal)

(declaim (notinline bottom))
(declaim (notinline right))

;;; Some premature optimization.
(declaim (inline q-min))
(defun q-min (x y)
  "Two-argument fixnum version of min."
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x y))
  (if (< x y) x y))


(declaim (inline q-max))
(defun q-max (x y)
  "Two-argument fixnum version of max."
  (if (< x y) y x))


(declaim (inline q-abs))
(defun q-abs (x)
  "Fixnum version of abs."
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x))
  (if (< x 0) (- x) x))


(create-instance 'GRAPHIC-QUALITY NIL)

;; (create-instance 'FONT graphic-quality
;;   :declare ((:type (font-family :family)
;; 		   (font-face :face)
;; 		   (font-size :size))
;; 	    (:type (fixnum :max-char-ascent :max-char-descent :font-height))
;; 	    (:maybe-constant :family :face :size))
;;   (:ps-font-name (o-formula (ps-font-name (gvl :family) (gvl :face))))
;;   (:ps-font-size (o-formula (ps-font-size (gvl :size))))
;;   (:family :fixed)
;;   (:face :roman)
;;   (:size :medium)
;;   (:char-width (o-formula (when (eq (gvl :family) :fixed)
;; 			    (gem:text-width (gv gem:device-info :current-root)
;; 					    (gv :self) "X"))))
;;   (:max-char-ascent
;;    (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
;;                 (if root (gem:max-character-ascent root (gv :self)) 0))))
;;   (:max-char-descent
;;    (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
;;                 (if root (gem:max-character-descent root (gv :self)) 0))))
;;   (:font-height (o-formula (+ (gvl-fixnum :max-char-ascent)
;; 			      (gvl-fixnum :max-char-descent))))
;;   )

;; (create-instance 'DEFAULT-FONT FONT
;;    (:constant T))

(defvar *Font-Table* (make-array '(3 4 4)
      :initial-contents '(((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil))
                          ((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil))
                          ((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil)))))


(defun GET-STANDARD-FONT (family face size)
  "
Get-Standard-Font returns a font object.  If this function is called multiple
times with the same font specification, the same object will be returned, thus
avoiding wasted objects.
    Allowed values:
    family -- :fixed, :serif, :sans-serif, or NIL (NIL == :fixed)
    face   -- :roman, :italic, :bold, :bold-italic, or NIL (NIL == :roman)
    size   -- :small, :medium, :large, :very-large, or NIL (NIL == :medium)"
  (let ((family-num (case (or family (setf family :fixed))
		      (:fixed 0)
		      (:serif 1)
		      (:sans-serif 2)
		      (t (error "Invalid font family -- ~S" family))))
	(face-num (case (or face (setf face :roman))
		    (:roman 0)
		    (:italic 1)
		    (:bold 2)
		    (:bold-italic 3)
		    (t (error "Invalid font face -- ~S" face))))
	(size-num (case (or size (setf size :medium))
		    (:small 0)
		    (:medium 1)
		    (:large 2)
		    (:very-large 3)
		    (t (error "Invalid font size -- ~S" size)))))
    (or (aref *Font-Table* family-num face-num size-num)
	(setf (aref *Font-Table* family-num face-num size-num)
	      (create-instance nil FONT
		(:constant T)
		(:standard-p T)
		(:family family)
		(:face face)
		(:size size))))))

(create-instance 'line-style nil
  (:constant T))

(create-instance 'DEFAULT-LINE-STYLE line-style
  (:constant T))


(create-instance 'VIEW-OBJECT NIL
  :declare ((:type (fixnum :left :top)
		   (fixnum :width :height :hit-threshold)
		   (known-as-type :known-as)
		   (kr-boolean :visible))
	    (:update-slots :visible :fast-redraw-p)
	    (:local-only-slots (:window nil) (:parent nil))
	    (:sorted-slots :is-a :left :top :width :height :visible :line-style
			   :filling-style :draw-function :components :parent)
	    (:ignored-slots :depended-slots :update-slots :update-slots-values)
	    )
  (:left 0)
  (:top 0)
  (:width 0)
  (:height 0)
  (:hit-threshold 0)
  (:visible (o-formula (let ((parent (gvl :parent)))
			    (or (null parent) (gv parent :visible)))
                       t))
  ;; The following are the controls for the schema printer
  (:limit-values '((:is-a-inv 5)))
  (:global-limit-values 5))

(create-instance 'AGGREGATE view-object
  :declare (:type (list :components)
		  (fixnum :left :top :width :height))
  (:components)
  (:update-slots NIL) ; New update does not use AGGREGATE'S visible!
  (:left (o-formula
          (let ((min-x 32767))     ;(min-x 999999))
	    (dolist (child (gv-local (gv :self) :components))
	      (when (gv child :visible)
		(setf min-x (q-min min-x (gv child :left)))))
	    (if (= min-x 32767) 0 min-x))
	  0))
  (:top (o-formula
	 (let ((min-y 32767))     ; (min-y 999999)
	   (dolist (child (gv-local (gv :self) :components))
	     (when (gv child :visible)
	       (setf min-y (q-min min-y (gv child :top)))))
	   (if (= min-y 32767) 0 min-y))
	 0))
  (:width (o-formula
	   (let ((max-x -32767)     ; (max-x -999999)
		 (min-x (gvl :left)))
	     (declare (fixnum max-x min-x))
	     (dolist (child (gv-local (gv :self) :components))
	       (when (gv child :visible)
		 (setf max-x (q-max max-x (+ (or (gv-fixnum child :left) 0)
					     (or (gv-fixnum child :width) 0))))))
	     (q-max 0 (- max-x min-x)))))
  (:height (o-formula
	    (let ((max-y -32767)        ; (max-y -999999)
		  (min-y (gvl :top)))
	      (dolist (child (gv-local (gv :self) :components))
                 (when (gv child :visible)
                   (setf max-y (q-max max-y (+ (or (gv child :top) 0)
					     (or (gv child :height) 0))))))
	      (q-max 0 (- max-y min-y)))))

  (:visible (o-formula (let ((parent (gvl :parent)))
			    (or (null parent) (gv parent :visible)))
                       t))

)

(create-instance 'GRAPHICAL-OBJECT view-object
  :declare ((:type (fixnum :top :left :width :height)
		   ((or (is-a-p line-style) null) :line-style)
		   ((or (is-a-p filling-style) null) :filling-style)
		   ((member :copy :xor :no-op :or :clear :set :copy-inverted
			    :invert :and :equiv :nand :nor :and-inverted
			    :and-reverse :or-inverted :or-reverse)
		    :draw-function))
	    (:update-slots :visible :fast-redraw-p :line-style :filling-style
			   :draw-function))
  (:top 0)
  (:left 0)
  (:width 20)
  (:height 20)
  (:draw-function :copy)
  (:line-style default-line-style)
  (:filling-style nil)
  (:select-outline-only nil))

(create-instance 'RECTANGLE graphical-object
  :declare ((:parameters :left :top :width :height :line-style :filling-style
			 :draw-function :visible)
	    (:type (fixnum :left :top :width :height))
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :visible)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :line-style :filling-style :draw-function))
  )
