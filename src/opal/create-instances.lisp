;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Opal:Create-Instances.Lisp
;;
;;  This file contains all the calls to KR:Create-Instance which are in Opal.
;;  They appear in the order in which they are listed in the overall Opal
;;  hierarchy, which is listed first.  Please keep it that way!
;;  NOTE:  the first entry of ":update-slots" MUST be :visible (unless the
;;    value is NIL), elsewise the update algorithm will break!


;;; The Opal Hierarchy
;;

;; opal:GRAPHIC-QUALITY
;; 	opal:FONT
;; 		opal:DEFAULT-FONT
;; 	opal:COLOR
;; 		opal:WHITE
;; 		opal:BLACK
;; 		opal:RED
;; 		opal:GREEN
;; 		opal:BLUE
;; 		opal:YELLOW
;; 		opal:CYAN
;; 		opal:ORANGE
;; 		opal:PURPLE
;; 		opal:MOTIF-GRAY
;; 		opal:MOTIF-BLUE
;; 		opal:MOTIF-ORANGE
;; 		opal:MOTIF-GREEN
;; 	opal:LINE-STYLE
;; 		opal:DEFAULT-LINE-STYLE
;; 		opal:THIN-LINE
;; 		opal:LINE-0
;; 		opal:LINE-1
;; 		opal:LINE-2
;; 		opal:LINE-4
;; 		opal:LINE-8
;; 		opal:DOTTED-LINE
;; 		opal:DASHED-LINE
;; 		opal:RED-LINE
;; 		opal:GREEN-LINE
;; 		opal:BLUE-LINE
;; 		opal:YELLOW-LINE
;; 		opal:ORANGE-LINE
;; 		opal:CYAN-LINE
;; 		opal:PURPLE-LINE
;; 		opal:WHITE-LINE
;; 	opal:FILLING-STYLE
;; 		opal:DEFAULT-FILLING-STYLE
;; 		opal:WHITE-FILL
;; 		opal:LIGHT-GRAY-FILL
;; 		opal:GRAY-FILL
;; 		opal:DARK-GRAY-FILL
;; 		opal:BLACK-FILL
;; 		opal:RED-FILL
;; 		opal:GREEN-FILL
;; 		opal:BLUE-FILL
;; 		opal:YELLOW-FILL
;; 		opal:ORANGE-FILL
;; 		opal:CYAN-FILL
;; 		opal:PURPLE-FILL
;; 		opal:MOTIF-GRAY-FILL
;; 		opal:MOTIF-BLUE-FILL
;; 		opal:MOTIF-ORANGE-FILL
;; 		opal:MOTIF-GREEN-FILL
;; 	opal:FONT-FROM-FILE
;;   opal:VIEW-OBJECT
;; 	opal:AGGREGATE
;; 		opal:MULTIFONT-TEXT
;; 		opal:AGGREGADGET
;; 		opal:AGGRELIST
;; 	opal:GRAPHICAL-OBJECT
;; 		opal:LINE
;; 		opal:RECTANGLE
;; 			opal:ROUNDTANGLE
;; 		opal:ARC
;; 			opal:OVAL
;; 			opal:CIRCLE
;; 		opal:MULTIPOINT
;; 			opal:POLYLINE
;;                               opal:ARROWHEAD
;; 		opal:TEXT
;; 			opal:CURSOR-TEXT
;;                       opal:MULTI-TEXT
;;                               opal:CURSOR-MULTI-TEXT
;; 		opal:BITMAP
;; 			opal::WHITE-FILL-BITMAP
;; 			opal::LIGHT-GRAY-FILL-BITMAP
;; 			opal::GRAY-FILL-BITMAP
;; 			opal::DARK-GRAY-FILL-BITMAP
;; 			opal:ARROW-CURSOR
;; 			opal:ARROW-CURSOR-MASK
;; 			opal:PIXMAP
;; 		opal:VIRTUAL-AGGREGATE
;; 	opal:WINDOW


(in-package :opal)


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
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x y))
  (if (< x y) y x))


(declaim (inline q-abs))
(defun q-abs (x)
  "Fixnum version of abs."
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x))
  (if (< x 0) (- x) x))

;;; Graphic-Quality Hierarchy
;;

(create-instance 'GRAPHIC-QUALITY NIL)

(define-method :destroy-me graphic-quality (quality)
  (destroy-schema quality))

(define-method :destroy graphic-quality (quality)
  (dolist (instance (copy-list (g-local-value quality :is-a-inv)))
    (destroy instance))
  (destroy-me quality))


(create-instance 'FONT-FROM-FILE graphic-quality
  :declare ((:parameters :font-path :font-name)
	    (:type ((or string cons) :font-name)
		   ((or null string) :font-path)
		   (fixnum :max-char-ascent :max-char-descent :font-height))
	    (:ignored-slots :display-xfont-plist))
  (:xfont (o-formula (fff-to-xfont (gvl :font-from-file)
				   (gv gem:DEVICE-INFO :current-root))))
  (:max-char-ascent
   (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
                (if root (gem:max-character-ascent root (gv :self)) 0))))
  (:max-char-descent
   (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
                (if root (gem:max-character-descent root (gv :self)) 0))))
  (:font-height (o-formula (+ (gvl-fixnum :max-char-ascent)
			      (gvl-fixnum :max-char-descent))))
  (:display-xfont-plist NIL)
  (:font-path NIL)
  (:font-name "")
  ;; Can't transport machine-dependent font info, so just use Courier
  (:ps-font-name "/Courier")
  (:ps-font-size (o-formula (gvl :font-height))))


(define-method :initialize font-from-file (fff)
  (s-value fff :font-from-file fff))

(setf (gethash '(:fixed :roman :medium) *font-hash-table*)
      (create-instance 'default-font-from-file font-from-file
	(:font-name (o-formula (gem:make-font-name
				(gv gem:DEVICE-INFO :current-device)
				'(:fixed :roman :medium))))))

(defun fff-to-xfont (fff root-window)
  (gem:font-to-internal root-window fff))


(create-instance 'FONT graphic-quality
  :declare ((:type (font-family :family)
		   (font-face :face)
		   (font-size :size))
	    (:type (fixnum :max-char-ascent :max-char-descent :font-height))
	    (:maybe-constant :family :face :size))
  (:ps-font-name (o-formula (ps-font-name (gvl :family) (gvl :face))))
  (:ps-font-size (o-formula (ps-font-size (gvl :size))))
  (:family :fixed)
  (:face :roman)
  (:size :medium)
  (:xfont (o-formula (fff-to-xfont (gvl :font-from-file)
				   (gv gem:device-info :current-root))))
  (:char-width (o-formula (when (eq (gvl :family) :fixed)
			    (gem:text-width (gv gem:device-info :current-root)
					    (gv :self) "X"))))
  (:max-char-ascent
   (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
                (if root (gem:max-character-ascent root (gv :self)) 0))))
  (:max-char-descent
   (o-formula (let ((root (gv gem:DEVICE-INFO :current-root)))
                (if root (gem:max-character-descent root (gv :self)) 0))))
  (:font-height (o-formula (+ (gvl-fixnum :max-char-ascent)
			      (gvl-fixnum :max-char-descent))))
  (:font-from-file
   (o-formula
    (let ((key (list (gvl :family) (gvl :face) (gvl :size))))
      (or (gethash key *font-hash-table*)
	  (let* ((root-window (gv gem:DEVICE-INFO :current-device))
                 (font-name (gem:make-font-name root-window key)))
	    (if (gem:font-name-p root-window font-name)
                (setf (gethash key *font-hash-table*)
		      (create-instance NIL font-from-file
		        (:font-name font-name)))
	        (progn
		  (warn "~A not allowed for :~A slot of font; substituting default-font."
			(car font-name)
			(cdr font-name))
		  default-font-from-file))))))))

(create-instance 'DEFAULT-FONT FONT
   (:constant T))

(create-instance 'CURSOR-FONT FONT-FROM-FILE
  (:constant T)
  (:font-name "cursor"))

;; Used in multifonts
(defvar *Font-Table* (make-array '(3 4 4)
      :initial-contents '(((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil))
                          ((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil))
                          ((nil nil nil nil) (nil nil nil nil)
                           (nil nil nil nil) (nil nil nil nil)))))

;; Fetch a font from the font table corresponding to the attribute parameters.
;;
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

(setf (aref *Font-Table* 0 0 1) default-font)


(let ((first-time T)
      (*first-allocatable-colormap-index* 1))

  (defun first-allocatable-colormap-index (root-window)
    "Noop.  Get rid of all calls to this."
    (when first-time
      (setf first-time NIL))
    1)

  (defun reset-first-allocatable-colormap-index (root-window)
    (setf first-time T)
    (first-allocatable-colormap-index root-window))

  (defun set-first-allocatable-colormap-index (root-window value)
    (declare (ignore root-window))
    (setf *first-allocatable-colormap-index* value)))

;;; :xcolor and :colormap-index map to CLX' color and pixel concepts.
;;; color is a device independant RGB triplet whereas pixel is a
;;; device dependant RGB triplet encoded as a 32bit integer.  To
;;; simplify the only case now found in the real world, truecolor
;;; displays, we assume that there is only on pixmap and that that
;;; pixmap is truecolor and hardware defined. (Refer to the CLX
;;; documentation for more information on this) So, no need to
;;; allocate, deallocate colors from a color map, just translate them
;;; from color to pixel as the hardware has defined.
(create-instance 'COLOR GRAPHIC-QUALITY
  ;; note, :color-name is now depreciated.
  :declare ((:parameters :red :green :blue :color-name)
	    (:type ((real 0 1) :red :green :blue)
		   ((or string atom) :color-name))
	    (:constant :color-p))
  (:red 1.0)
  (:green 1.0)
  (:blue 1.0)
  (:color-p t)    ;; depreciated, all screens are considered to be 'color'
  (:xcolor
   (o-formula
    (gem:colormap-property
     (gv gem:device-info :current-root)
     :MAKE-COLOR (gvl :red) (gvl :green) (gvl :blue))))
  (:colormap-index
   (o-formula
    (gem:colormap-property
     (gv gem:device-info :current-root)
     :ALLOC-COLOR (gvl :xcolor)))))

(define-method :destroy-me COLOR (hue)
	       (destroy-schema hue))

(create-instance 'RED color
  (:red 1.0) (:green 0.0) (:blue 0.0))

(create-instance 'GREEN color
  (:red 0.0) (:green 1.0) (:blue 0.0))

(create-instance 'BLUE color
  (:red 0.0) (:green 0.0) (:blue 1.0))

(create-instance 'YELLOW color
  (:red 1.0) (:green 1.0) (:blue 0.0))

(create-instance 'CYAN color
  (:red 0.0) (:green 1.0) (:blue 1.0))

(create-instance 'PURPLE color
  (:red 1.0) (:green 0.0) (:blue 1.0))

(create-instance 'ORANGE color
  (:red 1.0) (:green 0.65) (:blue 0.0))

(create-instance 'WHITE color
  (:red 1.0) (:green 1.0) (:blue 1.0))

(create-instance 'BLACK color
  (:red 0.0) (:green 0.0) (:blue 0.0))

(create-instance 'LINE-STYLE graphic-quality
  :declare ((:type (fixnum :line-thickness) ;(integer :line-thickness)
		   (keyword :line-style :cap-style :join-style)
		   ((is-a-p color) :foreground-color :background-color))
	    (:maybe-constant :line-thickness :line-style :cap-style :join-style
			     :dash-pattern :foreground-color :background-color
			     :stipple))
  (:line-thickness (the fixnum 0))
  (:line-style :solid)			; or :dash or :double-dash
  (:cap-style :butt)			; or :not-last, :round or :projecting
  (:join-style :miter)			; or :round or :bevel
  (:dash-pattern nil)
  (:foreground-color black)
  (:background-color white)
  (:stipple nil))


(create-instance 'DEFAULT-LINE-STYLE line-style
  (:constant T))


(create-instance 'LINE-0 line-style
  (:constant T))
(defvar THIN-LINE LINE-0)
(create-instance 'LINE-1 line-style
  (:constant T)
  (:line-thickness 1))
(create-instance 'LINE-2 line-style
  (:constant T)
  (:line-thickness 2))
(create-instance 'LINE-4 line-style
  (:constant T)
  (:line-thickness 4))
(create-instance 'LINE-8 line-style
  (:constant T)
  (:line-thickness 8))

(create-instance 'RED-LINE line-style
  (:constant T)
  (:foreground-color red))
(create-instance 'GREEN-LINE line-style
  (:constant T)
  (:foreground-color green))
(create-instance 'BLUE-LINE line-style
  (:constant T)
  (:foreground-color blue))
(create-instance 'CYAN-LINE line-style
  (:constant T)
  (:foreground-color cyan))
(create-instance 'YELLOW-LINE line-style
  (:constant T)
  (:foreground-color yellow))
(create-instance 'ORANGE-LINE line-style
  (:constant T)
  (:foreground-color orange))
(create-instance 'PURPLE-LINE line-style
  (:constant T)
  (:foreground-color purple))
(create-instance 'WHITE-LINE line-style
  (:constant T)
  (:foreground-color white))

(create-instance 'DOTTED-LINE line-style
  (:constant T)
  (:line-style :dash)
  (:line-thickness 1)
  (:dash-pattern '(1 1)))


(create-instance 'DASHED-LINE line-style
  (:constant T)
  (:line-style :dash)
  (:dash-pattern '(4 4)))


(create-instance 'FILLING-STYLE graphic-quality
  :declare ((:parameters :foreground-color :background-color :fill-style
			 :fill-rule :stipple)
	    (:type (fill-style :fill-style)
		   ((member :even-odd :winding) :fill-rule)
		   ((is-a-p color) :foreground-color :background-color)))
  (:fill-style :solid)    ;; or :opaque-stippled or :stippled
  (:fill-rule :even-odd)  ;; or :winding
  (:foreground-color black)
  (:background-color white)
  (:stipple nil))


(create-instance 'DEFAULT-FILLING-STYLE filling-style)

;; For the *-FILL schemas, please see the end of this file (to avoid
;; forward references, they had to be put there)....


;;; View-Object Hierarchy
;;

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


;; Aggregates allow for a group of graphical-objects to be associated
;; together to form a new, more complex object.
;;
;; An implementation detail:
;; The children of a gob are stored in a list from bottom most to top
;; most, since we want to redraw fastest and redraws occur from bottom to
;; top.
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

  ;;; TOA OMITTED
  ;;; ;; The TOA is the Topmost-Overlapping-Aggregate.  This slot will hopefully
  ;;; ;; improve the performance of the update algorithm.  The formula given here
  ;;; ;; is only for AGGREGATEs.  A different one appears within Graphical-Object.
  ;;; (:toa (o-formula
  ;;; 	  (let ((parent (gvl :parent)))
  ;;; 	    (or (and parent (gv parent :toa))
  ;;; 		(if (gvl :overlapping) kr::*schema-self*)))))

)


;; Class Graphical-object
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
  (:select-outline-only nil)

 ;;; OMITTING TOA
 ;;;  ;; The TOA is the Topmost-Overlapping-Aggregate.  This slot will hopefully
 ;;;  ;; improve the performance of the update algorithm.  The formula given here
 ;;;  ;; is for NON-AGGREGATE objects.  A different one appears within Aggregates.
 ;;;  (:toa (o-formula
 ;;; 	  (let ((parent (gvl :parent)))
 ;;; 	    (and parent (gv parent :toa)))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apparently in the days of the Pharaohs, scribes processing long
;;  Lisp files had problems because the stones containing the object
;;  code they generated were very large and heavy. For this reason,
;;  the original "create-instances.lisp" file was broken into two
;;  pieces, thereby preventing the scribes from getting hernias. Or
;;  something like that.
;;
;;  At any rate, modern Lisp implementations no longer have this
;;  problem.
;;
;; The code from the former create-instances2.lisp follows.

(create-instance 'LINE graphical-object
  :declare ((:parameters :x1 :y1 :x2 :y2 :line-style :draw-function :visible)
	    (:type (fixnum :x1 :y1 :x2 :y2))
	    (:maybe-constant :x1 :y1 :x2 :y2 :line-style :line-p :visible)
	    (:update-slots :visible :fast-redraw-p :x1 :x2 :y1 :y2 :line-style
			   :filling-style :draw-function))

  (:x1 0)
  (:y1 0)
  (:x2 0)
  (:y2 0)
  (:left
   (o-formula (- (q-min (gvl-fixnum :x1) (gvl-fixnum :x2))
		 (if (eq (gvl :line-style :cap-style) :projecting)
		     (q-max 1 (the fixnum (or (gvl :line-style :line-thickness) 0)))
		     (the fixnum (floor (or (gvl :line-style :line-thickness) 0) 2))))
	      0))
  (:top
   (o-formula  (- (q-min (gvl-fixnum :y1) (gvl-fixnum :y2))
		  (if (eq (gvl :line-style :cap-style) :projecting)
		      (q-max 1 (gvl-fixnum :line-style :line-thickness))
		      (the fixnum (floor (or (gvl :line-style :line-thickness) 0) 2))))
	       0))
  (:width
   (o-formula (+ (q-abs (- (or (gvl-fixnum :x1) 0) (or (gvl-fixnum :x2) 0)))
		 (* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
		    (q-max 1 (the fixnum (or (gvl :line-style :line-thickness) 0)))))))
  (:height
   (o-formula (+ (q-abs (- (or (gvl-fixnum :y1) 0) (or (gvl-fixnum :y2) 0)))
		 (* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
		    (q-max 1 (the fixnum (or (gvl :line-style :line-thickness) 0)))))))
  (:line-p T))

(create-instance 'RECTANGLE graphical-object
  :declare ((:parameters :left :top :width :height :line-style :filling-style
			 :draw-function :visible)
	    (:type (fixnum :left :top :width :height))
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :visible)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :line-style :filling-style :draw-function))
  )


(create-instance 'FAST-REDRAW-RECTANGLE rectangle
   (:line-style NIL))

(let ((update-vals (s-value FAST-REDRAW-RECTANGLE :update-slots-values
			(make-array 9 :initial-element nil))))
  (setf (aref update-vals +rect-draw-function+) :copy)
  (setf (aref update-vals 0) T)
  (setf (aref update-vals 1) :rectangle))

(create-instance 'ROUNDTANGLE rectangle
  :declare ((:parameters :left :top :width :height :radius :line-style
			 :filling-style :draw-function :visible)
	    (:type ((or keyword (integer 0)) :radius))
	    (:maybe-constant :left :top :width :height :radius :line-style
			     :filling-style :visible)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :radius :draw-radius :line-style :filling-style
			   :draw-function))
  (:radius :small)
  (:draw-radius (o-formula (let ((r (gvl :radius))
				 (smaller-side (q-min (gvl :width)
						    (gvl :height))))
			     (if (numberp r)
				 (q-min (q-max r 0) (floor smaller-side 2))
				 (case r
				   (:small (floor smaller-side 5))
				   (:medium (floor smaller-side 4))
				   (:large (floor smaller-side 3))
				   (t 0)))))))


(create-instance 'ARC graphical-object
  :declare ((:parameters :left :top :width :height :angle1 :angle2
			 :line-style :filling-style :draw-function :visible)
	    (:type (number angle1 angle2)
		   (fixnum :left :top :width :height))
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :angle1 :angle2
			     :visible)
	    (:update-slots :visible :fast-redraw-p :left :top :width :height
                           :angle1 :angle2
			   :line-style :filling-style :draw-function))
  (:angle1 0)
  (:angle2 (/ pi 4)))


(create-instance 'OVAL arc
  :declare ((:parameters :left :top :width :height
			 :line-style :filling-style :draw-function :visible))
  )


(create-instance 'CIRCLE arc
  :declare ((:parameters :left :top :width :height
			 :line-style :filling-style :draw-function :visible)))


(create-instance 'MULTIPOINT graphical-object
  :declare ((:parameters :point-list :line-style :filling-style :draw-function
			 :visible)
	    (:type (list :point-list))
	    (:maybe-constant :point-list :line-style :filling-style
			     :draw-function :visible)
	    (:update-slots :visible :fast-redraw-p :point-list
			   :line-style :filling-style :draw-function))
  (:point-list '(18 10 10 24 26 24 18 10))  ;; a roughly equilateral triangle.
  (:left (o-formula
	  (let ((point-list (gvl :point-list)))
	    (if point-list
		(let* ((min-x 9999)
		       (line-style (gvl :line-style))
		       (lsthickness (if line-style
					(* 2 (q-max 1 (gv line-style
							:line-thickness)))
					0)))
		  (do ((point point-list (cddr point)))
		      ((null point) (- min-x lsthickness))
		    (setf min-x (q-min min-x (car point)))))
		0))))
  (:top (o-formula
	 (let ((point-list (gvl :point-list)))
	   (if point-list
	       (let* ((min-y 9999)
		      (line-style (gvl :line-style))
		      (lsthickness (if line-style
				       (* 2 (q-max 1 (gv line-style
						       :line-thickness)))
				       0)))
		 (do ((point point-list (cddr point)))
		     ((null point) (- min-y lsthickness))
		   (setf min-y (q-min min-y (cadr point)))))
	       0))))
  (:width (o-formula
	   (let ((point-list (gvl :point-list)))
	     (if point-list
		 (let* ((min-x 9999)
			(max-x 0)
			(line-style (gvl :line-style))
			(lsthickness (if line-style
					 (* 4 (q-max 1 (gv line-style
							 :line-thickness)))
					 0)))
		   (do ((point point-list (cddr point)))
		       ((null point) (+ (- max-x min-x) lsthickness))
		     (setf min-x (q-min min-x (car point)))
		     (setf max-x (q-max max-x (car point)))))
		 0))))
  (:height (o-formula
	    (let ((point-list (gvl :point-list)))
	      (if point-list
		  (let* ((min-y 9999)
			 (max-y 0)
			 (line-style (gvl :line-style))
			 (lsthickness (if line-style
					  (* 4 (q-max 1 (gv line-style
							  :line-thickness)))
					  0)))
		    (do ((point point-list (cddr point)))
			((null point) (+ (- max-y min-y) lsthickness))
		      (setf min-y (q-min min-y (cadr point)))
		      (setf max-y (q-max max-y (cadr point)))))
		  0)))))


(create-instance 'POLYLINE multipoint
  :declare ((:parameters :point-list :hit-full-interior-p :line-style
			 :filling-style :draw-function :visible))
  (:hit-full-interior-p nil))


#|
;;; 11/4/1993	dzg & amickish -- commented out.
;;; (defun font-to-xfont (opal-font display)
;;;   (gem:font-to-internal gem::*root-window*
;;; 			(g-value opal-font :font-from-file)))
|#


(create-instance 'BITMAP graphical-object
  :declare ((:parameters :left :top :image :filling-style
			 :draw-function :visible)
	    (:maybe-constant :left :top :image :filling-style :visible)
	    (:ignored-slots :depended-slots :update-slots :update-slots-values
			    :root-pixmap-plist :image)
	    (:update-slots :visible :fast-redraw-p :image :top :left
			   :line-style :filling-style :draw-function))
  (:image nil)
  (:width (o-formula (let ((image (gvl :image)))
		       (if image
			   (gem:image-size
			    (or (gvl :window)
				(gv gem:DEVICE-INFO :current-root))
			    image)
			   0))))
  (:height (o-formula
	    (let ((image (gvl :image)))
	      (if image
		  (multiple-value-bind (width height)
		      (gem:image-size
		       (or (gvl :window)
			   (gv gem:DEVICE-INFO :current-root))
		       image)
		    (declare (ignore width))
		    height)
		  0))))
  (:filling-style default-filling-style))


;;; All the *-FILL-BITMAPs will have their :image slot set once the function
;;; "halftone-image" is defined...

(create-instance 'WHITE-FILL-BITMAP bitmap
  (:percent 0)
  (:image))   ;;; will be (halftone-image 0)

(create-instance 'LIGHT-GRAY-FILL-BITMAP bitmap
  (:percent 25)
  (:image))   ;;; will be (halftone-image 25)

(create-instance 'GRAY-FILL-BITMAP bitmap
  (:percent 50)
  (:image))   ;;; will be (halftone-image 50)

(create-instance 'DARK-GRAY-FILL-BITMAP bitmap
  (:percent 75)
  (:image))   ;;; will be (halftone-image 75)

;; Have to define GRAY-LINE here instead of in create-instances.lisp with
;; the other line styles because gray-fill-bitmap is defined in this file.
(create-instance 'GRAY-LINE line-style
  (:constant T)
  (:stipple gray-fill-bitmap))

;;; Colors and filling-styles for Motif

;;;  Orange, Green, Blue lists for defining colors
(defvar MOTIF-GRAY-VALUE (float (/ #xd3d3 #xffff)))

(defvar MOTIF-BLUE-VALUES
  (list (float (/ #x7272 #xffff)) (float (/ #x9f9f #xffff)) 1))

(defvar MOTIF-GREEN-VALUES
  (list (float (/ #x5f5f #xffff)) (float (/ #x9e9e #xffff))
        (float (/ #xa0a0 #xffff))))

(defvar MOTIF-ORANGE-VALUES (list 1 .6 .4))

;;;  Lite values:  Most of the values are just the outline colors
;;;  of a MOTIF-xxx colored object, with some slight modifications.

(defvar MOTIF-LIGHT-GRAY-VALUE .9)
(defvar MOTIF-LIGHT-BLUE-VALUES (list 0.7217459 0.8998047 1))
(defvar MOTIF-LIGHT-GREEN-VALUES (list 0.61834437 0.8660691 0.7))
(defvar MOTIF-LIGHT-ORANGE-VALUES (list 1 0.9129001 0.71510005))


(create-instance 'MOTIF-GRAY-FILL default-filling-style
   (:foreground-color (create-instance 'MOTIF-GRAY color
                         (:red MOTIF-GRAY-VALUE)
                         (:green MOTIF-GRAY-VALUE)
                         (:blue MOTIF-GRAY-VALUE))))

(create-instance 'MOTIF-BLUE-FILL default-filling-style
   (:foreground-color (create-instance 'MOTIF-BLUE color
                         (:red (first MOTIF-BLUE-VALUES))
                         (:green (second MOTIF-BLUE-VALUES))
                         (:blue (third MOTIF-BLUE-VALUES)))))

(create-instance 'MOTIF-ORANGE-FILL default-filling-style
   (:foreground-color (create-instance 'MOTIF-ORANGE color
                         (:red (first MOTIF-ORANGE-VALUES))
                         (:green (second MOTIF-ORANGE-VALUES))
                         (:blue (third MOTIF-ORANGE-VALUES)))))

(create-instance 'MOTIF-GREEN-FILL default-filling-style
   (:foreground-color (create-instance 'MOTIF-GREEN color
                         (:red (first MOTIF-GREEN-VALUES))
                         (:green (second MOTIF-GREEN-VALUES))
                         (:blue (third MOTIF-GREEN-VALUES)))))

(create-instance 'MOTIF-LIGHT-GRAY-FILL filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-GRAY color
		       (:red MOTIF-LIGHT-GRAY-VALUE)
		       (:green MOTIF-LIGHT-GRAY-VALUE)
		       (:blue MOTIF-LIGHT-GRAY-VALUE))))

(create-instance 'MOTIF-LIGHT-BLUE-FILL filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-BLUE color
		       (:red (first MOTIF-LIGHT-BLUE-VALUES))
		       (:green (second MOTIF-LIGHT-BLUE-VALUES))
		       (:blue (third MOTIF-LIGHT-BLUE-VALUES)))))

(create-instance 'MOTIF-LIGHT-ORANGE-FILL filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-ORANGE color
		       (:red (first MOTIF-LIGHT-ORANGE-VALUES))
		       (:green (second MOTIF-LIGHT-ORANGE-VALUES))
		       (:blue (third MOTIF-LIGHT-ORANGE-VALUES)))))

(create-instance 'MOTIF-LIGHT-GREEN-FILL filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-GREEN color
		       (:red (first MOTIF-LIGHT-GREEN-VALUES))
		       (:green (second MOTIF-LIGHT-GREEN-VALUES))
		       (:blue (third MOTIF-LIGHT-GREEN-VALUES)))))



(create-instance 'ARROW-CURSOR bitmap
  (:constant :image)
  ;; Have to delay Get-Garnet-Bitmap from being executed before device
  ;; is initialized
  (:image (o-formula (Get-Garnet-Bitmap "garnet.cursor"))))

(create-instance 'ARROW-CURSOR-MASK bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "garnet.mask"))))

(defparameter Arrow-Pair (cons ARROW-CURSOR ARROW-CURSOR-MASK))


(create-instance 'HOURGLASS-CURSOR bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "hourglass.cursor"))))

(create-instance 'HOURGLASS-CURSOR-MASK bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "hourglass.mask"))))

(defparameter HourGlass-Pair (cons HOURGLASS-CURSOR HOURGLASS-CURSOR-MASK))

(create-instance 'GARBAGE-CURSOR bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "garbage.cursor"))))

(create-instance 'GARBAGE-CURSOR-MASK bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "garbage.mask"))))

;;; See windows.lisp for an application of these cursors.
(defparameter garbage-Pair (cons garbage-CURSOR garbage-CURSOR-MASK))

(create-instance 'ARROWHEAD polyline
  :declare ((:parameters :head-x :head-y :from-x :from-y :length :diameter
			 :open-p :line-style :filling-style :draw-function
			 :visible)
	    (:type (fixnum :head-x :head-y :from-x :from-y :dx :dy
			    :connect-x :connect-y :ax :ay :cx :cy :length
			    :diameter)
		   (number :radius :ftlength :ux :uy))
	    (:maybe-constant :line-style :filling-style :length :diameter
			     :open-p :head-x :head-y :from-x :from-y :radius
			     :visible)
	    (:update-slots :visible :fast-redraw-p :point-list
			   :line-style :filling-style :draw-function
			   :head-x :head-y :from-x :from-y :length :diameter
			   :open-p))
  (:head-x 0)
  (:head-y 0)
  (:from-x 0)
  (:from-y 0)
  (:radius (o-formula (/ (gvl-fixnum :diameter) 2)))
  (:dx (o-formula (- (gvl-fixnum :from-x) (gvl-fixnum :head-x))))
  (:dy (o-formula (- (gvl-fixnum :from-y) (gvl-fixnum :head-y))))
  (:ftlength (o-formula (let ((dx (gvl-fixnum :dx))
			      (dy (gvl-fixnum :dy)))
			  (max 1.0 (sqrt (+ (the fixnum (* dx dx)) (the fixnum (* dy dy))))))))
  (:ux (o-formula (/ (gvl-fixnum :dx) (gvl-fixnum :ftlength))))
  (:uy (o-formula (/ (gvl-fixnum :dy) (gvl-fixnum :ftlength))))
  (:connect-x (o-formula (round (+ (gvl-fixnum :head-x) (* (gvl-fixnum :length) (gvl :ux))))))
  (:connect-y (o-formula (round (+ (gvl-fixnum :head-y) (* (gvl-fixnum :length) (gvl :uy))))))
  (:ax (o-formula (round (- (gvl :connect-x) (* (gvl :radius) (gvl :uy))))))
  (:ay (o-formula (round (+ (gvl :connect-y) (* (gvl :radius) (gvl :ux))))))
  (:cx (o-formula (round (+ (gvl :connect-x) (* (gvl :radius) (gvl :uy))))))
  (:cy (o-formula (round (- (gvl :connect-y) (* (gvl :radius) (gvl :ux))))))
  (:point-list (o-formula (let ((ax (gvl-fixnum :ax)) (ay (gvl-fixnum :ay))
				(head-x (gvl-fixnum :head-x))
				(head-y (gvl-fixnum :head-y))
				(cx (gvl-fixnum :cx)) (cy (gvl-fixnum :cy)))
			    (if (gvl :open-p)
				(list ax ay head-x head-y cx cy)
				(list ax ay head-x head-y cx cy ax ay)))))
  (:length 10)
  (:diameter 10)
  (:open-p t)
)

;;; To create a window for displaying gobs, create a schema which is
;;  an instance of the window class described below specifying slots
;;  as needed. For example:
;;
;;  (create-instance my-window window
;;    (:width 100)
;;    (:height 100))
;;
(create-instance 'WINDOW view-object
  :declare ((:type ((or (is-a-p aggregate) null) :aggregate)
		   ((integer 1) :width :height)
		   ((integer 0) :border-width))
	    (:maybe-constant :left :top :width :height :visible)
	    (:ignored-slots :buffer-gc)
	    (:update-slots :visible :fast-redraw-p :aggregate :parent
			   :top :left :width :height :cursor :title :icon-title
			   :display :background-color :icon-bitmap
			   :draw-on-children :modal-p :save-under)
	    (:local-only-slots (:drawable nil) (:window nil)
			       (:parent nil) (:destroy-hooks nil)))
  (:left 0)
  (:top 0)
  (:width 355)
  (:height 277)
  (:omit-title-bar-p NIL)
  (:aggregate)
  (:title)       ; set in :initialize method
  (:icon-title)  ; set in :initialize method
  (:border-width 2)
  (:cursor (o-formula
	    (let ((parent (gvl :parent)))
	      (if parent
		  (gv parent :cursor)
		  Arrow-Pair))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;  *-FILL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'LIGHT-GRAY-FILL filling-style
  (:fill-style :opaque-stippled)
  (:stipple light-gray-fill-bitmap))

(create-instance 'GRAY-FILL filling-style
  (:fill-style :opaque-stippled)
  (:stipple gray-fill-bitmap))

(create-instance 'DARK-GRAY-FILL filling-style
  (:fill-style :opaque-stippled)
  (:stipple dark-gray-fill-bitmap))

(create-instance 'BLACK-FILL filling-style
  (:fill-style :solid))

(create-instance 'WHITE-FILL filling-style
  (:foreground-color white))
(create-instance 'RED-FILL filling-style
  (:foreground-color red))
(create-instance 'GREEN-FILL filling-style
  (:foreground-color green))
(create-instance 'BLUE-FILL filling-style
  (:foreground-color blue))
(create-instance 'YELLOW-FILL filling-style
  (:foreground-color yellow))
(create-instance 'ORANGE-FILL filling-style
  (:foreground-color orange))
(create-instance 'CYAN-FILL filling-style
  (:foreground-color cyan))
(create-instance 'PURPLE-FILL filling-style
  (:foreground-color purple))
