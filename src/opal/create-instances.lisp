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


;;; Change Log:
;;      date     who    what
;;      ----     ---    ----
;;    12-Sep-95  goldman   Changed the way colormap entries are tracked in
;;                         from using a fixed-size array to using a hash-table.
;;    25-May-94  amickish  New :max-char-ascent/descent formulas for fonts
;;    19-Apr-94  amickish  Reset first-time in first-allocatable-colormap-index
;;     5-Mar-94  amickish  Used names for font slot type declarations
;;    17-Dec-93  amickish  Gem'ified :font-from-file formula
;;    15-Dec-93  amickish  :active-devices slot now contains ?-DEVICE objects
;;    23-Aug-93  amickish  Changed default hit-threshold to 0
;;    24-May-93  amickish  Changed :width and :height formulas of aggregate to
;;                         depend on aggregate's own :left and :top
;;     3-May-93  amickish  Added s-value's in :xcolor formula of opal:COLOR;
;;                         added :ps-font-name/size to opal::FONT-FROM-FILE
;;    19-Apr-93  amickish  Added formulas to opal::FONT-FROM-FILE; added
;;                         opal:CURSOR-FONT
;;     3-Mar-93  amickish  Added :visible type declaration to VIEW-OBJECT
;;    10-Feb-93  amickish  Added :known-as type declaration to VIEW-OBJECT
;;    13-Jan-93  amickish  Added :xfont, :max-char-ascent, :max-char-descent,
;;                         :font-height, and :char-width slots to opal:font;
;;                         added parameter declarations
;;    30-Dec-92  amickish  Set :standard-p in get-standard-font for save-agg
;;     3-Jun-92  amickish  Added opal:white-line
;;     7-Apr-92  amickish  Made Get-Standard-Font use default values if NIL
;;                         parameters were supplied and added error checking.
;;     2-Apr-92  rgm    new multifont
;;    25-Mar-92  amickish  Get-Values ---> G-Value
;;    26-Feb-92  ecp    An opal:color may have a :color-name slot with a
;; 			string like "pink".
;;    21-Jan-92  amickish  Made opal:default-font an instance of opal:font,
;;                      added constant formula lists.
;;     6-Aug-91  dzg    Added extra error checking in formulas for :width
;; 			and height of aggregate.
;;     6-Aug-91  amickish  Added :ps-font-name and :ps-font-size to opal:font
;;     5-Aug-91  ecp    Made opal:default-font be same as opal:font.
;;    26-Mar-91  ecp    Added :components to :local-only-slots slot of
;; 			opal:aggregate.
;;     7-Mar-91  ecp    The question of whether the screen is color or
;; 			black-and-white is now determined in defs.lisp.
;;    22-Feb-91  amickish  New exported motif colors and filling styles.
;;    14-Feb-91  ecp    Yet more changes to color so that colors are
;;                      deallocated when they are not used anymore.
;;     8-Feb-91  ecp    Added :color-p slot to opal:color to tell if
;;                      screen is black-and-white or color.
;;    10-Aug-90  loyall Made :width, :height of aggregate not depend
;;                      directly on :top, :left.
;;     1-Aug-90  dzg    New :local-only-slots slot in opal:view-object
;;    19-Jul-90  ecp    Made thickness of line-1 be 1.
;;    20-Jun-90  ecp    Temporarily made thickness of dotted-line be 1,
;; 			due to new CLX bug.
;;     4-Jun-90  ecp    Removed inverse relation between :parent and :child
;;    16-Apr-90  ecp    Moved creation of default-font earlier.
;;    27-Mar-90  ecp    In build-pixmap, changed 0 and 1 to *black*
;; 			and *white*.
;;    19-Mar-90  ecp    Got rid of Garnet-Font-Pathname.
;; 			Changed :tile to :stipple
;;     1-Mar-90  ecp    In build-pixmap, changed the :bitmap-p argument
;; 			to xlib:put-image from t to nil.
;;    13-Feb-90  ecp    Implemented color.
;;    25-Jan-90  ecp    Changes to fonts.
;;     5-Dec-89  ecp    Moved create-instance of FONT-FROM-FILE earlier.
;;      ******* SEE OPAL CHANGE.LOG ********
;;    15-Jun-89  koz	Placed Graphic-Quality hierarchy before View-Object
;; 			to resolve forward references (instead of s-value).
;; 			This should fix bug that made Cursor-Text not inherit
;; 			the right slots at creation time.
;;    15-Jun-89  koz	Converted from kr:formula to kr:o-formula.
;;    15-Jun-89  koz	Extracted all forward references and placed them all
;; 			in S-VALUEs at the end of this file, or in other files
;; 			if they needed functions not yet defined...
;;    14-Jun-89  koz    Created.  Simply extracted all the calls to kr:create-
;; 			instance from all the Opal files.  No modifications
;; 			were made to them.


(in-package "OPAL")

;; I *hate* to do this, but this function needs to go here so that the
;; the reference to it below doesn't generate a warning at compile time.  Of
;; course, we *should* be able to just declare it, but no...  Bug in compiler!
;;
(defun build-pixmap (a-window image width height bitmap-p)
  (gem:create-pixmap a-window width height 1 image bitmap-p))


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



;;; Graphic-Quality Hierarchy
;;

(create-instance 'opal:GRAPHIC-QUALITY NIL)

(define-method :destroy-me opal:graphic-quality (quality)
  (destroy-schema quality))

(define-method :destroy opal:graphic-quality (quality)
  (dolist (instance (copy-list (g-local-value quality :is-a-inv)))
    (destroy instance))
  (destroy-me quality))
			   

(create-instance 'opal::FONT-FROM-FILE opal:graphic-quality
  :declare ((:parameters :font-path :font-name)
	    (:type ((or string cons) :font-name)
		   ((or null string) :font-path))
	    (:ignored-slots :display-xfont-plist))
  (:xfont (o-formula (fff-to-xfont (gvl :font-from-file)
				   (gv DEVICE-INFO :current-root))))
  (:max-char-ascent
   (o-formula (let ((root (gv DEVICE-INFO :current-root)))
                (if root (gem:max-character-ascent root (gv :self)) 0))))
  (:max-char-descent
   (o-formula (let ((root (gv DEVICE-INFO :current-root)))
                (if root (gem:max-character-descent root (gv :self)) 0))))
  (:font-height (o-formula (+ (gvl :max-char-ascent) (gvl :max-char-descent))))
  (:display-xfont-plist NIL)
  (:font-path NIL)
  (:font-name "")
  ;; Can't transport machine-dependent font info, so just use Courier
  (:ps-font-name "/Courier")
  (:ps-font-size (o-formula (gvl :font-height))))


(define-method :initialize opal:font-from-file (fff)
  (s-value fff :font-from-file fff))

(setf (gethash '(:fixed :roman :medium) *font-hash-table*)
      (create-instance 'opal::default-font-from-file opal:font-from-file
	(:font-name (o-formula (gem:make-font-name
				(gv DEVICE-INFO :current-device)
				'(:fixed :roman :medium))))))

(defun fff-to-xfont (fff root-window)
  (gem:font-to-internal root-window fff))


(create-instance 'opal:FONT opal:graphic-quality
  :declare ((:type (font-family :family)
		   (font-face :face)
		   (font-size :size))
	    (:maybe-constant :family :face :size))
  (:ps-font-name (o-formula (ps-font-name (gvl :family) (gvl :face))))
  (:ps-font-size (o-formula (ps-font-size (gvl :size))))
  (:family :fixed)
  (:face :roman)
  (:size :medium)
  (:xfont (o-formula (fff-to-xfont (gvl :font-from-file)
				   (gv device-info :current-root))))
  (:char-width (o-formula (when (eq (gvl :family) :fixed)
			    (gem:text-width (gv device-info :current-root)
					    (gv :self) "X"))))
  (:max-char-ascent
   (o-formula (let ((root (gv DEVICE-INFO :current-root)))
                (if root (gem:max-character-ascent root (gv :self)) 0))))
  (:max-char-descent
   (o-formula (let ((root (gv DEVICE-INFO :current-root)))
                (if root (gem:max-character-descent root (gv :self)) 0))))
  (:font-height (o-formula (+ (gvl :max-char-ascent) (gvl :max-char-descent))))
  (:font-from-file
   (o-formula
    (let ((key (list (gvl :family) (gvl :face) (gvl :size))))
      (or (gethash key *font-hash-table*)
	  (let* ((root-window (gv DEVICE-INFO :current-device))
                 (font-name (gem:make-font-name root-window key)))
	    (if (gem:font-name-p root-window font-name)
                (setf (gethash key *font-hash-table*)
		      (create-instance NIL opal:font-from-file
		        (:font-name font-name)))
	        (progn
		  (warn "~A not allowed for :~A slot of font; substituting default-font." 
			(car font-name)
			(cdr font-name))
		  opal::default-font-from-file))))))))

(create-instance 'opal:DEFAULT-FONT opal:FONT
   (:constant T))

(create-instance 'opal::CURSOR-FONT opal:FONT-FROM-FILE
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
	      (create-instance nil opal:font
		(:constant T)
		(:standard-p T)
		(:family family)
		(:face face)
		(:size size))))))

(setf (aref *Font-Table* 0 0 1) opal:default-font)


(let ((first-time T)
      (*first-allocatable-colormap-index* 1))

  (defun first-allocatable-colormap-index (root-window)
    (when first-time
      ;; Find out the first colormap index that you are actually allowed to
      ;; allocate and deallocate.
      ;;THIS WON'T WORK ON A TRUE-COLOR SCREEN! [1995/12/08:goldman]
      (when *read-write-colormap-cells-p*
	(setq *first-allocatable-colormap-index*
	      (gem:colormap-property root-window :FIRST-ALLOCATABLE-INDEX)))
      (setf first-time NIL))
    *first-allocatable-colormap-index*)

  (defun reset-first-allocatable-colormap-index (root-window)
    (setf first-time T)
    (first-allocatable-colormap-index root-window))
  
  (defun set-first-allocatable-colormap-index (root-window value)
    (declare (ignore root-window))
    (setf *first-allocatable-colormap-index* value)))



(create-instance 'opal:COLOR opal:graphic-quality
  :declare ((:parameters :red :green :blue :color-name)
	    (:type ((real 0 1) :red :green :blue)
		   ((or string atom) :color-name))
	    (:constant :color-p))
  (:red 1.0)
  (:green 1.0)
  (:blue 1.0)
  (:color-p *is-this-a-color-screen?*)  ; Set by initialize-x11-values
  (:xcolor (o-formula
	    (let ((name (gvl :color-name)))
	      (if name
		  (multiple-value-bind (red green blue)
		      (gem:colormap-property (gv device-info :current-root)
					     :LOOKUP-RGB name)
		    ;; The PS module needs the RGB values
		    (s-value (gv :self) :red red)
		    (s-value (gv :self) :green green)
		    (s-value (gv :self) :blue blue)
		    name)
		  (gem:colormap-property
		   (gv device-info :current-root)
		   :MAKE-COLOR (gvl :red) (gvl :green) (gvl :blue))))))
  (:colormap-index
   (o-formula
    (let* ((root-window (gv device-info :current-root))
	   (old-index (g-cached-value (gv :self) :colormap-index))
	   (new-index (gem:colormap-property root-window :ALLOC-COLOR
					     (gvl :xcolor))))
      ;;changed the following [1995/12/08:goldman]
      (when *read-write-colormap-cells-p*
	(when (and old-index
		   (>= old-index
		       (first-allocatable-colormap-index root-window))
		   (zerop (decf (gethash old-index *colormap-index-table*))))
	  (gem:colormap-property root-window :FREE-COLORS (list old-index)))
	(incf (gethash new-index *colormap-index-table* 0)))
      new-index))))
	

(define-method :destroy-me opal:color (hue)
  (when *is-this-a-color-screen?*
    (let ((index (g-cached-value hue :colormap-index)))
      (dolist (device (g-value DEVICE-INFO :active-devices))
	(let ((root-window (g-value device :root-window)))
	  (when (and index
		     ;;replaced the old array with a hash-table
		     (zerop (decf (gethash index *colormap-index-table*)))
		     (>= index (first-allocatable-colormap-index root-window)))
	    (gem:colormap-property root-window
				   :FREE-COLORS (list index)))))))
  (destroy-schema hue))

				    
(create-instance 'opal:RED opal:color
  (:red 1.0) (:green 0.0) (:blue 0.0))

(create-instance 'opal:GREEN opal:color
  (:red 0.0) (:green 1.0) (:blue 0.0))

(create-instance 'opal:BLUE opal:color
  (:red 0.0) (:green 0.0) (:blue 1.0))

(create-instance 'opal:YELLOW opal:color
  (:red 1.0) (:green 1.0) (:blue 0.0))

(create-instance 'opal:CYAN opal:color
  (:red 0.0) (:green 1.0) (:blue 1.0))

(create-instance 'opal:PURPLE opal:color
  (:red 1.0) (:green 0.0) (:blue 1.0))

(create-instance 'opal:ORANGE opal:color
  (:red 1.0) (:green 0.65) (:blue 0.0))

(create-instance 'opal:WHITE opal:color
  (:red 1.0) (:green 1.0) (:blue 1.0))

(create-instance 'opal:BLACK opal:color
  (:red 0.0) (:green 0.0) (:blue 0.0))

(create-instance 'opal:LINE-STYLE opal:graphic-quality
  :declare ((:type (fixnum :line-thickness) ;(integer :line-thickness)
		   (keyword :line-style :cap-style :join-style)
		   ((is-a-p opal:color) :foreground-color :background-color))
	    (:maybe-constant :line-thickness :line-style :cap-style :join-style
			     :dash-pattern :foreground-color :background-color
			     :stipple))
  (:line-thickness 0)
  (:line-style :solid)			; or :dash or :double-dash
  (:cap-style :butt)			; or :not-last, :round or :projecting
  (:join-style :miter)			; or :round or :bevel
  (:dash-pattern nil)
  (:foreground-color opal::black)
  (:background-color opal::white)
  (:stipple nil))


(create-instance 'opal:DEFAULT-LINE-STYLE opal:line-style
  (:constant T))


(create-instance 'opal::LINE-0 opal:line-style
  (:constant T))
(defvar opal::THIN-LINE opal::LINE-0)
(create-instance 'opal::LINE-1 opal:line-style
  (:constant T)
  (:line-thickness 1))
(create-instance 'opal::LINE-2 opal:line-style
  (:constant T)
  (:line-thickness 2))
(create-instance 'opal::LINE-4 opal:line-style
  (:constant T)
  (:line-thickness 4))
(create-instance 'opal::LINE-8 opal:line-style
  (:constant T)
  (:line-thickness 8))

(create-instance 'opal:RED-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:red))
(create-instance 'opal:GREEN-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:green))
(create-instance 'opal:BLUE-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:blue))
(create-instance 'opal:CYAN-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:cyan))
(create-instance 'opal:YELLOW-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:yellow))
(create-instance 'opal:ORANGE-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:orange))
(create-instance 'opal:PURPLE-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:purple))
(create-instance 'opal:WHITE-LINE opal:line-style
  (:constant T)
  (:foreground-color opal:white))

(create-instance 'opal::DOTTED-LINE opal:line-style
  (:constant T)
  (:line-style :dash)
  (:line-thickness 1)
  (:dash-pattern '(1 1)))


(create-instance 'opal::DASHED-LINE opal:line-style
  (:constant T)
  (:line-style :dash)
  (:dash-pattern '(4 4)))


(create-instance 'opal:FILLING-STYLE opal:graphic-quality
  :declare ((:parameters :foreground-color :background-color :fill-style
			 :fill-rule :stipple)
	    (:type (fill-style :fill-style)
		   ((member :even-odd :winding) :fill-rule)
		   ((is-a-p opal:color) :foreground-color :background-color)))
  (:fill-style :solid)    ;; or :opaque-stippled or :stippled
  (:fill-rule :even-odd)  ;; or :winding
  (:foreground-color opal::black)
  (:background-color opal::white)
  (:stipple nil))


(create-instance 'opal:DEFAULT-FILLING-STYLE opal:filling-style)

;; For the *-FILL schemas, please see the end of this file (to avoid
;; forward references, they had to be put there)....


;;; View-Object Hierarchy
;;

(create-instance 'opal:VIEW-OBJECT NIL
  :declare ((:type (fixnum :left :top) ; (integer :left :top)
		   (#-(and) (integer 0) #+(and) fixnum :width :height :hit-threshold)
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
(create-instance 'opal:AGGREGATE opal:view-object
  :declare (:type (list :components))
  (:components)
  (:update-slots NIL) ; New update does not use AGGREGATE'S visible!
  (:left (o-formula
          (let ((min-x 999999))
	    (declare (fixnum min-x))
	    (dolist (child (gv-local (gv :self) :components))
	      (when (gv child :visible)
		(setf min-x (min min-x (gv child :left)))))
	    (if (= min-x 999999) 0 min-x))
	  0))
  (:top (o-formula
	 (let ((min-y 999999))
	   (declare (fixnum min-y))
	   (dolist (child (gv-local (gv :self) :components))
	     (when (gv child :visible)
	       (setf min-y (min min-y (gv child :top)))))
	   (if (= min-y 999999) 0 min-y))
	 0))
  (:width (o-formula
	   (let ((max-x -999999)
		 (min-x (gvl :left)))
	     (declare (fixnum max-x min-x))
	     (dolist (child (gv-local (gv :self) :components))
	       (when (gv child :visible)
		 (setf max-x (max max-x (+ (or (gv child :left) 0)
					   (or (gv child :width) 0))))))
	     (max 0 (- max-x min-x)))))
  (:height (o-formula
	    (let ((max-y -999999)
		  (min-y (gvl :top)))
	      (declare (fixnum max-y min-y))
	      (dolist (child (gv-local (gv :self) :components))
                 (when (gv child :visible)
                   (setf max-y (max max-y (+ (or (gv child :top) 0)
					     (or (gv child :height) 0))))))
	      (max 0 (- max-y min-y)))))

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
(create-instance 'opal:GRAPHICAL-OBJECT opal:view-object
  :declare ((:type ((or (is-a-p opal:line-style) null) :line-style)
		   ((or (is-a-p opal:filling-style) null) :filling-style)
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
  (:line-style opal:default-line-style)
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
