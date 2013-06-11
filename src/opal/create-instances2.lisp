;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opal:Create-Instances2.Lisp
;;;
;;; Changes:
;;; 25-Jan-94 amickish  Changed opal:white-fill to be a solid fill, instead of
;;;                     an opaque stippled fill
;;; 04-Dec-93 amickish  Do not maintain :display of opal::window
;;; 20-Aug-93 rajan     Added MOTIF-LIGHT-xxx and MOTIF-LIGHT-xxx-FILL
;;; 06-Aug-93 amickish  Added opal:Arrow-Pair
;;; 28-Jul-93 amickish  Moved multi-text objects to multi-text.lisp
;;; 30-Jun-93 amickish  Superceded opal:text and opal:cursor-text definitions
;;;                with new cursor-text.lisp
;;; 11-Jun-93 amickish  Added initial values to opal:line's :left and :top
;;;  6-Jun-93 amickish  Added type restrictions on :width and :height of window
;;; 22-Apr-93 amickish  Added HourGlass cursor; used Get-Garnet-Bitmap
;;; 13-Jan-93 amickish  Utilized new :font-height slot in text formulas;
;;;                added parameter declarations.
;;; 10-Dec-92 amickish  Fixed :height formulas of text objects to not depend
;;;                on :text-extents or :string if :actual-heightp is NIL.
;;; 09-Dec-92 dzg  Added type declarations
;;; 25-Nov-92 amickish  Added opal:gray-line
;;; 20-Nov-92 amickish  Fixed dimension formulas of MULTIPOINT to return 0
;;;                when :point-list is NIL
;;; 11-Jun-92 ecp  Added slot to polyline to allow genuine :point-in-gob
;;; 27-May-92 ecp  Added :save-under to the update-slots list of opal:window.
;;; 26-May-92 ecp  Added :modal-p to the update-slots list of opal:window.
;;; 16-Apr-92 amickish :ignored-slots value: :buffer-gc ===> (list :buffer-gc)
;;; 13-Apr-92 ecp  Must say :initial-element nil in make-array, since
;;;		   the default for CMUCL is 0.
;;; 10-Apr-92 ecp  Made :point-list of multipoint default to be
;;;		   a roughly equilateral triangle.
;;; 31-Mar-92 ecp  Added :draw-on-children and :icon-bitmap to
;;;		   :update-slots of window.
;;; 30-Mar-92 Duchier  Gv the :font-from-file of :font in the formula for
;;;		 :xfont of text, so that :xfont is recomputed whenever
;;;		 :family or :face of :font changes.
;;; 21-Jan-91 amickish  Added constant formula lists
;;;  9-Dec-91 ecp  Rewrote :text-extents formula of text to use
;;;		   multiple-value-bind instead of multiple-value-list.
;;; 25-Nov-91 koz  added init of update-slots-vals for fast-redraw-rectangle
;;; 18-Nov-91 Pervin  Added :background-color to windows.
;;;  4-Oct-91 amickish  Added fast-redraw-rectangle
;;; 10-Sep-91 ecp  Simplified :xfont formula of opal:text.
;;;  6-Aug-91 dzg  Safe versions of min, max in formulas of opal:line.
;;;  1-Aug-91 ecp  Made bitmap use filling-style instead of line-style.
;;; 20-May-91 ecp  Gray bitmaps now have :percent slot telling what percent
;;;		   of bits are filled.
;;;  4-Apr-91 ecp  New slot :omit-title-bar-p for windows.
;;; 22-Feb-91 amickish  New exported motif colors and filling styles.
;;; 22-Oct-90 ecp  New improved formulae for top, left, width, height of line.
;;; 17-Oct-90 ecp  In formula for :width of text, allowed string to be nil.
;;;  5-Oct-90 ecp  More work on formula for :width of text.
;;; 11-Sep-90 ecp  Changed formula for :width of text.
;;; 30-Aug-90 ecp  Multipoints whose line-thicknesses are 0 must be
;;;                treated as if their line-thicknesses were 1 (since
;;;                they're drawn that way).
;;; 14-Aug-90 bam  Added :local-only-slots slot to opal:window.
;;;  1-Aug-90 dzg  New :local-only-slots slot in opal:cursor-text.
;;; 27-Jun-90 bam  Made :buffer-gc be :ignored-slot of opal:window.
;;;  4-Jun-90 dzg  Removed (:parent NIL) from opal:window.
;;;  1-May-90 ecp  If :image of bitmap is nil, width and height are 0.
;;; 16-Apr-90 ecp  Moved defun of font-to-xfont from new-defs.lisp to
;;;		   create-instances2.lisp
;;; 23-Mar-90 ecp  New slot :fill-background-p for text objects.
;;; 19-Mar-90 ecp  Changed :tile to :stipple
;;; 12-Mar-90 ecp  Setting :title and :icon-title of windows
;;;		   in :initialize method.
;;; 13-Feb-90 ecp Implemented color.

(in-package "OPAL")


;;; dzg
;;; Safe versions of min, max.  Return rather arbitrary value (i.e., 0) when
;;; all arguments are NIL
;;;
(defun safe-min (&rest numbers)
  (let ((result (first numbers)))
    (dolist (n (cdr numbers))
      (when n
	(if result
	    (if (< n result)
		(setf result n))
	    (setf result n))))
    result))



(defun safe-max (&rest numbers)
  (let ((result (first numbers)))
    (dolist (n (cdr numbers))
      (when n
	(if result
	    (if (> n result)
		(setf result n))
	    (setf result n))))
    result))



;;; This is only necessary because Lisp has problems with really large
;;; fasl files.  Great.

(create-instance 'opal:LINE opal:graphical-object
  :declare ((:parameters :x1 :y1 :x2 :y2 :line-style :draw-function :visible)
	    (:type (integer :x1 :y1 :x2 :y2))
	    (:maybe-constant :x1 :y1 :x2 :y2 :line-style :line-p :visible)
	    (:update-slots :visible :fast-redraw-p :x1 :x2 :y1 :y2 :line-style
			   :filling-style :draw-function))

  (:x1 0)
  (:y1 0)
  (:x2 0)
  (:y2 0)
  (:left
   (o-formula (- (safe-min (gvl :x1) (gvl :x2))
		 (if (eq (gvl :line-style :cap-style) :projecting)
		     (safe-max 1 (gvl :line-style :line-thickness))
		     (floor (gvl :line-style :line-thickness) 2))) 0))
  (:top
   (o-formula  (- (safe-min (gvl :y1) (gvl :y2))
		  (if (eq (gvl :line-style :cap-style) :projecting)
		      (safe-max 1 (gvl :line-style :line-thickness))
		      (floor (gvl :line-style :line-thickness) 2))) 0))
  (:width
   (o-formula (+ (abs (- (or (gvl :x1) 0) (or (gvl :x2) 0)))
		 (* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
		    (safe-max 1 (gvl :line-style :line-thickness))))))
  (:height
   (o-formula (+ (abs (- (or (gvl :y1) 0) (or (gvl :y2) 0)))
		 (* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
		    (safe-max 1 (gvl :line-style :line-thickness))))))
  (:line-p T))

(create-instance 'opal:RECTANGLE opal:graphical-object
  :declare ((:parameters :left :top :width :height :line-style :filling-style
			 :draw-function :visible)
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :visible)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :line-style :filling-style :draw-function))
  )


(create-instance 'opal::FAST-REDRAW-RECTANGLE opal:rectangle
   (:line-style NIL))

(let ((update-vals (s-value opal::FAST-REDRAW-RECTANGLE :update-slots-values
			(make-array 9 :initial-element nil))))
   (setf (aref update-vals +rect-draw-function+) :copy)
   (setf (aref update-vals 0) T)
   (setf (aref update-vals 1) :rectangle))

(create-instance 'opal:ROUNDTANGLE opal:rectangle
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
                                (smaller-side (min (gvl :width)
                                                   (gvl :height))))
                            (if (numberp r)
                                (min (max r 0) (floor smaller-side 2))
                                (case r
                                  (:small (floor smaller-side 5))
                                  (:medium (floor smaller-side 4))
                                  (:large (floor smaller-side 3))
                                  (t 0)))))))


(create-instance 'opal:ARC opal:graphical-object
  :declare ((:parameters :left :top :width :height :angle1 :angle2
			 :line-style :filling-style :draw-function :visible)
	    (:type (number angle1 angle2))
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :angle1 :angle2
			     :visible)
	    (:update-slots :visible :fast-redraw-p :left :top :width :height
                           :angle1 :angle2
			   :line-style :filling-style :draw-function))
  (:angle1 0)
  (:angle2 (/ pi 4)))


(create-instance 'opal:OVAL opal:arc
  :declare ((:parameters :left :top :width :height
			 :line-style :filling-style :draw-function :visible))
  )


(create-instance 'opal:CIRCLE opal:arc
  :declare ((:parameters :left :top :width :height
			 :line-style :filling-style :draw-function :visible)))


(create-instance 'opal:MULTIPOINT opal:graphical-object
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
					(* 2 (max 1 (gv line-style
							:line-thickness)))
					0)))
		  (do ((point point-list (cddr point)))
		      ((null point) (- min-x lsthickness))
		    (setf min-x (min min-x (car point)))))
		0))))
  (:top (o-formula
	 (let ((point-list (gvl :point-list)))
	   (if point-list
	       (let* ((min-y 9999)
		      (line-style (gvl :line-style))
		      (lsthickness (if line-style
				       (* 2 (max 1 (gv line-style
						       :line-thickness)))
				       0)))
		 (do ((point point-list (cddr point)))
		     ((null point) (- min-y lsthickness))
		   (setf min-y (min min-y (cadr point)))))
	       0))))
  (:width (o-formula
	   (let ((point-list (gvl :point-list)))
	     (if point-list
		 (let* ((min-x 9999)
			(max-x 0)
			(line-style (gvl :line-style))
			(lsthickness (if line-style
					 (* 4 (max 1 (gv line-style
							 :line-thickness)))
					 0)))
		   (do ((point point-list (cddr point)))
		       ((null point) (+ (- max-x min-x) lsthickness))
		     (setf min-x (min min-x (car point)))
		     (setf max-x (max max-x (car point)))))
		 0))))
  (:height (o-formula
	    (let ((point-list (gvl :point-list)))
	      (if point-list
		  (let* ((min-y 9999)
			 (max-y 0)
			 (line-style (gvl :line-style))
			 (lsthickness (if line-style
					  (* 4 (max 1 (gv line-style
							  :line-thickness)))
					  0)))
		    (do ((point point-list (cddr point)))
			((null point) (+ (- max-y min-y) lsthickness))
		      (setf min-y (min min-y (cadr point)))
		      (setf max-y (max max-y (cadr point)))))
		  0)))))


(create-instance 'opal:POLYLINE opal:multipoint
  :declare ((:parameters :point-list :hit-full-interior-p :line-style
			 :filling-style :draw-function :visible))
  (:hit-full-interior-p nil))


#|
;;; 11/4/1993	dzg & amickish -- commented out.
;;; (defun font-to-xfont (opal-font display)
;;;   (gem:font-to-internal gem::*root-window*
;;; 			(g-value opal-font :font-from-file)))
|#


(create-instance 'opal:BITMAP opal:graphical-object
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
			   (gem:image-size (or (gvl :window)
					       (gv DEVICE-INFO :current-root))
					   image)
			   0))))
  (:height (o-formula
	    (let ((image (gvl :image)))
	      (if image
		  (multiple-value-bind (width height)
		      (gem:image-size (or (gvl :window)
					  (gv DEVICE-INFO :current-root))
				      image)
		    (declare (ignore width))
		    height)
		  0))))
  (:filling-style opal:default-filling-style))


;;; All the *-FILL-BITMAPs will have their :image slot set once the function
;;; "halftone-image" is defined...

(create-instance 'opal::WHITE-FILL-BITMAP opal:bitmap
  (:percent 0)
  (:image))   ;;; will be (halftone-image 0)

(create-instance 'opal::LIGHT-GRAY-FILL-BITMAP opal:bitmap
  (:percent 25)
  (:image))   ;;; will be (halftone-image 25)

(create-instance 'opal::GRAY-FILL-BITMAP opal:bitmap
  (:percent 50)
  (:image))   ;;; will be (halftone-image 50)

(create-instance 'opal::DARK-GRAY-FILL-BITMAP opal:bitmap
  (:percent 75)
  (:image))   ;;; will be (halftone-image 75)

;; Have to define GRAY-LINE here instead of in create-instances.lisp with
;; the other line styles because gray-fill-bitmap is defined in this file.
(create-instance 'opal::GRAY-LINE opal:line-style
  (:constant T)
  (:stipple opal::gray-fill-bitmap))

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


(create-instance 'MOTIF-GRAY-FILL opal:default-filling-style
   (:foreground-color (create-instance 'MOTIF-GRAY opal:color
                         (:red MOTIF-GRAY-VALUE)
                         (:green MOTIF-GRAY-VALUE)
                         (:blue MOTIF-GRAY-VALUE))))

(create-instance 'MOTIF-BLUE-FILL opal:default-filling-style
   (:foreground-color (create-instance 'MOTIF-BLUE opal:color
                         (:red (first MOTIF-BLUE-VALUES))
                         (:green (second MOTIF-BLUE-VALUES))
                         (:blue (third MOTIF-BLUE-VALUES)))))

(create-instance 'MOTIF-ORANGE-FILL opal:default-filling-style
   (:foreground-color (create-instance 'MOTIF-ORANGE opal:color
                         (:red (first MOTIF-ORANGE-VALUES))
                         (:green (second MOTIF-ORANGE-VALUES))
                         (:blue (third MOTIF-ORANGE-VALUES)))))

(create-instance 'MOTIF-GREEN-FILL opal:default-filling-style
   (:foreground-color (create-instance 'MOTIF-GREEN opal:color
                         (:red (first MOTIF-GREEN-VALUES))
                         (:green (second MOTIF-GREEN-VALUES))
                         (:blue (third MOTIF-GREEN-VALUES)))))

(create-instance 'MOTIF-LIGHT-GRAY-FILL opal:filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-GRAY opal:color
		       (:red MOTIF-LIGHT-GRAY-VALUE)
		       (:green MOTIF-LIGHT-GRAY-VALUE)
		       (:blue MOTIF-LIGHT-GRAY-VALUE))))

(create-instance 'MOTIF-LIGHT-BLUE-FILL opal:filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-BLUE opal:color
		       (:red (first MOTIF-LIGHT-BLUE-VALUES))
		       (:green (second MOTIF-LIGHT-BLUE-VALUES))
		       (:blue (third MOTIF-LIGHT-BLUE-VALUES)))))

(create-instance 'MOTIF-LIGHT-ORANGE-FILL opal:filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-ORANGE opal:color
		       (:red (first MOTIF-LIGHT-ORANGE-VALUES))
		       (:green (second MOTIF-LIGHT-ORANGE-VALUES))
		       (:blue (third MOTIF-LIGHT-ORANGE-VALUES)))))

(create-instance 'MOTIF-LIGHT-GREEN-FILL opal:filling-style
  (:foreground-color (create-instance 'MOTIF-LIGHT-GREEN opal:color
		       (:red (first MOTIF-LIGHT-GREEN-VALUES))
		       (:green (second MOTIF-LIGHT-GREEN-VALUES))
		       (:blue (third MOTIF-LIGHT-GREEN-VALUES)))))



(create-instance 'opal::ARROW-CURSOR opal:bitmap
  (:constant :image)
  ;; Have to delay Get-Garnet-Bitmap from being executed before device
  ;; is initialized
  (:image (o-formula (Get-Garnet-Bitmap "garnet.cursor"))))

(create-instance 'opal::ARROW-CURSOR-MASK opal:bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "garnet.mask"))))

(defparameter Arrow-Pair (cons ARROW-CURSOR ARROW-CURSOR-MASK))


(create-instance 'opal::HOURGLASS-CURSOR opal:bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "hourglass.cursor"))))

(create-instance 'opal::HOURGLASS-CURSOR-MASK opal:bitmap
  (:constant :image)
  (:image (o-formula (Get-Garnet-Bitmap "hourglass.mask"))))

(defparameter HourGlass-Pair (cons HOURGLASS-CURSOR HOURGLASS-CURSOR-MASK))

#+cmu
(progn

  (create-instance 'opal::garbage-CURSOR opal:bitmap
		   (:constant :image)
		   (:image (o-formula (Get-Garnet-Bitmap "garbage.cursor"))))

  (create-instance 'opal::garbage-CURSOR-MASK opal:bitmap
		   (:constant :image)
		   (:image (o-formula (Get-Garnet-Bitmap "garbage.mask"))))

  (defparameter garbage-Pair (cons garbage-CURSOR garbage-CURSOR-MASK))

  (defun set-gc-cursor ()
    (opal:change-cursors garbage-pair))

  (defun unset-gc-cursor ()
    (opal:restore-cursors))

  (pushnew #'set-gc-cursor ext:*before-gc-hooks*)
  (pushnew #'unset-gc-cursor ext:*after-gc-hooks*)
  )


(create-instance 'opal:ARROWHEAD opal:polyline
  :declare ((:parameters :head-x :head-y :from-x :from-y :length :diameter
			 :open-p :line-style :filling-style :draw-function
			 :visible)
	    (:type (integer :head-x :head-y :from-x :from-y :dx :dy
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
  (:radius (o-formula (/ (gvl :diameter) 2)))
  (:dx (o-formula (- (gvl :from-x) (gvl :head-x))))
  (:dy (o-formula (- (gvl :from-y) (gvl :head-y))))
  (:ftlength (o-formula (let ((dx (gvl :dx))
			      (dy (gvl :dy)))
			  (max 1.0 (sqrt (+ (* dx dx) (* dy dy)))))))
  (:ux (o-formula (/ (gvl :dx) (gvl :ftlength))))
  (:uy (o-formula (/ (gvl :dy) (gvl :ftlength))))
  (:connect-x (o-formula (round (+ (gvl :head-x) (* (gvl :length) (gvl :ux))))))
  (:connect-y (o-formula (round (+ (gvl :head-y) (* (gvl :length) (gvl :uy))))))
  (:ax (o-formula (round (- (gvl :connect-x) (* (gvl :radius) (gvl :uy))))))
  (:ay (o-formula (round (+ (gvl :connect-y) (* (gvl :radius) (gvl :ux))))))
  (:cx (o-formula (round (+ (gvl :connect-x) (* (gvl :radius) (gvl :uy))))))
  (:cy (o-formula (round (- (gvl :connect-y) (* (gvl :radius) (gvl :ux))))))
  (:point-list (o-formula (let ((ax (gvl :ax)) (ay (gvl :ay))
				(head-x (gvl :head-x))
				(head-y (gvl :head-y))
				(cx (gvl :cx)) (cy (gvl :cy)))
			    (if (gvl :open-p)
				(list ax ay head-x head-y cx cy)
				(list ax ay head-x head-y cx cy ax ay)))))
  (:length 10)
  (:diameter 10)
  (:open-p t)
)

;;; To create a window for displaying gobs, create a schema which is an
;;; instance of the window class described below specifying slots as
;;; needed. For example:
;;;
;;; (create-instance my-window opal:window
;;;   (:width 100)
;;;   (:height 100))
;;;
(create-instance 'opal::WINDOW opal:view-object
  :declare ((:type ((or (is-a-p opal:aggregate) null) :aggregate)
		   ((integer 1) :width :height)
		   ((integer 0) :border-width))
	    (:maybe-constant :left :top :width :height :visible)
	    (:ignored-slots :buffer-gc)
	    (:update-slots :visible :fast-redraw-p :aggregate :parent
			   :top :left :width :height :cursor :title :icon-title
			   :display :background-color :icon-bitmap
			   :draw-on-children :modal-p :save-under)
	    (:local-only-slots (:drawable nil) (:parent nil) (:window nil)))
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

(create-instance 'opal::LIGHT-GRAY-FILL opal:filling-style
  (:fill-style :opaque-stippled)
  (:stipple opal::light-gray-fill-bitmap))

(create-instance 'opal::GRAY-FILL opal:filling-style
  (:fill-style :opaque-stippled)
  (:stipple opal::gray-fill-bitmap))

(create-instance 'opal::DARK-GRAY-FILL opal:filling-style
  (:fill-style :opaque-stippled)
  (:stipple opal::dark-gray-fill-bitmap))

(create-instance 'opal::BLACK-FILL opal:filling-style
  (:fill-style :solid))

(create-instance 'opal::WHITE-FILL opal:filling-style
  (:foreground-color opal:white))
(create-instance 'opal::RED-FILL opal:filling-style
  (:foreground-color opal:red))
(create-instance 'opal::GREEN-FILL opal:filling-style
  (:foreground-color opal:green))
(create-instance 'opal::BLUE-FILL opal:filling-style
  (:foreground-color opal:blue))
(create-instance 'opal::YELLOW-FILL opal:filling-style
  (:foreground-color opal:yellow))
(create-instance 'opal::ORANGE-FILL opal:filling-style
  (:foreground-color opal:orange))
(create-instance 'opal::CYAN-FILL opal:filling-style
  (:foreground-color opal:cyan))
(create-instance 'opal::PURPLE-FILL opal:filling-style
  (:foreground-color opal:purple))

