;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CLX-TRUETYPE; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  The code in this file was written by Mikhail Filonenko and       ;;
;;  modified for integration with Garnet. It is used in accordance   ;;
;;  with the terms of the MIT license. See the file                  ;;
;;  MIT-LICENSE.clx-truetype in this directory.                      ;;
;;*******************************************************************;;

;;; $Id$
;;


(in-package "CLX-TRUETYPE")

(defvar *Fixed-Font-Family* "Liberation Mono")
(defvar *Serif-Font-Family* "Times New Roman")
(defvar *Sans-Serif-Font-Family* "Liberation Sans")


(def-kr-type BOOLEAN ()
  '(member t nil))

(create-instance 'opal::truetype-font opal:graphic-quality
  :declare ((:type (string :family)
		   (string :subfamily)
		   (integer :size)
		   (boolean :underline)
		   (boolean :strikethrough)))
  (:family "Courier")           ; Font family (i.e. Times New Roman, Zapf Dingbats etc.)
  (:subfamily "Regular")	; Font subfamily. For e.g. regular, italic, bold, bold italib. 
  (:size 12)                    ; Font size in points.
  (:underline nil)              ; Draw line under text string.
  (:strikethrough nil)          ; Draw strike through text string.
  (:overline nil)               ; Draw line over text string.
  (:background nil)             ; Background color.
  (:foreground nil)             ; Foreground color.
  (:overwrite-gcontext nil)     ; Use font values for background and foreground colors.
  (:antialias t)                ; Antialias text string.
  (:string-bboxes (make-hash-table :test 'equal)) ; Cache for text bboxes
  (:string-line-bboxes (make-hash-table :test 'equal)) ; Cache for text line bboxes
  (:string-alpha-maps (make-hash-table :test 'equal))  ; Cache for text alpha maps
  (:string-line-alpha-maps (make-hash-table :test 'equal)) ; Cache for text line alpha maps
  (:documentation "Class for representing font information."))

(defun check-valid-font-families (family subfamily)
  (let ((fam (gethash family *font-cache*)))
    (if (and fam (gethash subfamily fam))
	t
	(error "Font is not found: ~A ~A" family subfamily))))

(defun lookup-font (family)
  (let ((font (gethash family *font-cache*)))
    (when font
      (format t "~A~%" font)
      (maphash (lambda (a b) (declare (ignore a)) (format t "~t~A~%" b)) font))))


(define-method :initialize opal::truetype-font (font)
;;    ((instance font) &rest initargs &key family subfamily &allow-other-keys)
  (let ((family (s-value font :family))
	(subfamily (s-value font :subfamily)))
    (check-valid-font-families family subfamily)))

#-(and)
(defmethod (setf font-family) :before
  (family (instance font))
  (check-valid-font-families family (font-subfamily instance)))

#-(and)
(defmethod (setf font-subfamily) :before
  (subfamily (instance font))
  (check-valid-font-families (font-family instance) subfamily))

#-(and)
(defmethod (setf font-family) :after
  (family (font font))
  (clrhash (font-string-bboxes font))
  (clrhash (font-string-line-bboxes font)))

#-(and)
(defmethod (setf font-subfamily) :after
  (subfamily (font font))
  (clrhash (font-string-bboxes font))
  (clrhash (font-string-line-bboxes font)))

#-(and)
(defmethod (setf font-size) :after (value (font font))
  (clrhash (font-string-bboxes font))
  (clrhash (font-string-line-bboxes font)))

#-(and)
(defmethod (setf font-underline) :after (value (font font))
  (clrhash (font-string-bboxes font)))

#-(and)
(defmethod (setf font-overline) :after (value (font font))
  (clrhash (font-string-bboxes font)))

(defun font-equal (font1 font2)
  "Returns t if two font objects are equal, else returns nil."
  (equalp font1 font2)
  #-(and)
  (and (string-equal (font-family font1)
		     (font-family font2))
       (string-equal (font-subfamily font1)
		     (font-subfamily font2))
       (= (font-size font1) (font-size font2))
       (eql (font-underline font1) (font-underline font2))
       (eql (font-strikethrough font1) (font-strikethrough font2))
       (eql (font-overline font1) (font-overline font2))
       (equal (font-background font1) (font-background font2))
       (equal (font-foreground font1) (font-foreground font2))
       (eql (font-overwrite-gcontext font1) (font-overwrite-gcontext font2))
       (eql (font-antialias font1) (font-antialias font2))))

#-(and)
(defmethod print-object ((instance font) stream)
  "Pretty printing font object"
  (with-slots (family subfamily underline strikethrough
                   overline background foreground overwrite-gcontext
                   antialias)
      instance
    (if *print-readably*
        (format stream
                "#.(~S '~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)"
                'cl:make-instance 'font
                :family family :subfamily subfamily :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias)
        (format stream
                "#<'~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S>"
                'font
                :family family :subfamily subfamily :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias))))

;;; ZPB-TTF font objects cache
(defun get-font-pathname (font)
  (gethash (gv font :subfamily) (gethash (gv font :family) *font-cache*)))

(defvar *font-loader-cache* (make-hash-table :test 'equal))

(defmacro with-font-loader ((loader font) &body body)
  (let ((exists-p (gensym))
        (font-path (gensym)))
    `(let ((,font-path (get-font-pathname ,font)))
       (multiple-value-bind (,loader ,exists-p)
           (gethash ,font-path *font-loader-cache*)
         (unless ,exists-p
           (setf ,loader (setf (gethash ,font-path *font-loader-cache*)
                               (zpb-ttf:open-font-loader ,font-path))))
         ,@body))))

;;; Screen DPI
(defun screen-default-dpi (screen)
  "Returns default dpi for @var{screen}. pixel width * 25.4/millimeters width"
  (values (floor (* (xlib:screen-width screen) 25.4)
                 (xlib:screen-width-in-millimeters screen))
          (floor (* (xlib:screen-height screen) 25.4)
                 (xlib:screen-height-in-millimeters screen))))

(defun screen-dpi (screen)
  "Returns current dpi for @var{screen}."
  (values (getf (xlib:screen-plist screen) :dpi-x
                (floor (* (xlib:screen-width screen) 25.4)
                       (xlib:screen-width-in-millimeters screen)))
          (getf (xlib:screen-plist screen) :dpi-y
                (floor (* (xlib:screen-height screen) 25.4)
                             (xlib:screen-height-in-millimeters screen)))))

(defun (setf screen-dpi) (value screen)
  "Sets current dpi for @var{screen}."
  (setf (getf (xlib:screen-plist screen) :dpi-x) value
        (getf (xlib:screen-plist screen) :dpi-y) value))


;;; Font metrics

(defun font-units->pixels-x (drawable font)
  "px = funits*coeff. Function returns coeff."
  (with-font-loader (loader font)
    (multiple-value-bind (dpi-x dpi-y)
        (screen-dpi (drawable-screen drawable))
      (declare (ignore dpi-y))
      (let* ((size (gvl font :size))
	     (units/em (zpb-ttf:units/em loader))
	     (pixel-size-x (* size (/ dpi-x 72))))
	(* pixel-size-x (/ units/em))))))

(defun font-units->pixels-y (drawable font)
  "px = funits*coeff. Function returns coeff."
  (with-font-loader (loader font)
    (multiple-value-bind (dpi-x dpi-y)
        (screen-dpi (drawable-screen drawable))
      (declare (ignore dpi-x))
;;      (with-slots (size) font
      (let ((size (gvl font :size)))
        (let* ((units/em (zpb-ttf:units/em loader))
               (pixel-size-y (* size (/ dpi-y 72))))
          (* pixel-size-y (/ units/em)))))))

(defun font-ascent (drawable font)
  "Returns ascent of @var{font}. @var{drawable} must be window, pixmap or screen."
  (with-font-loader (loader font)
    (ceiling (* (font-units->pixels-y drawable font) (zpb-ttf:ascender loader)))))

(defun font-descent (drawable font)
  "Returns descent of @var{font}. @var{drawable} must be window, pixmap or screen."
  (with-font-loader (loader font)
    (floor (* (font-units->pixels-y drawable font) (zpb-ttf:descender loader)))))

(defun font-line-gap (drawable font)
  "Returns line gap of @var{font}. @var{drawable} must be window, pixmap or screen."
  (with-font-loader (loader font)
    (ceiling (* (font-units->pixels-y drawable font) (zpb-ttf:line-gap loader)))))

(defun baseline-to-baseline (drawable font)
  "Returns distance between baselines of @var{font}:
  baseline-to-baseline = ascent - descent + line gap
@var{drawable} must be window, pixmap or screen. ascent - descent + line gap"
  (+ (font-ascent drawable font) (- (font-descent drawable font))
     (font-line-gap drawable font)))

(defun text-bounding-box (drawable font string &key start end)
  "Returns text bounding box. @var{drawable} must be window, pixmap or screen.
 Text bounding box is only for contours. Bounding box for space (#x20) is zero."
  (when (and start end)
    (setf string (subseq string start end)))
  (multiple-value-bind (dpi-x dpi-y)
      (screen-dpi (drawable-screen drawable))
    (multiple-value-bind (string-bboxes exists-p)
        (gethash (cons dpi-x dpi-y) (gv font :string-bboxes) (make-hash-table :test 'equal))
      (unless exists-p
        (setf (gethash (cons dpi-x dpi-y) (gv font :string-bboxes)) string-bboxes))
      (or (gethash string string-bboxes)
          (setf (gethash string string-bboxes)
                (with-font-loader (loader font)
                  (let* ((bbox
                           (zpb-ttf:string-bounding-box string loader))
                         (units->pixels-x (font-units->pixels-x drawable font))
                         (units->pixels-y (font-units->pixels-y drawable font))
                         (xmin (zpb-ttf:xmin bbox))
                         (ymin (zpb-ttf:ymin bbox))
                         (xmax (zpb-ttf:xmax bbox))
                         (ymax (zpb-ttf:ymax bbox)))
                    (when (gv font :underline)
                      (setf ymin (min ymin (- (zpb-ttf:underline-position loader)
                                              (zpb-ttf:underline-thickness loader)))))
                    (when (gv font :overline)
                      (setf ymax (max ymax (+ (zpb-ttf:ascender loader)
                                              (zpb-ttf:underline-position loader)
                                              (+ (zpb-ttf:underline-thickness loader))))))
                    (vector (floor (* xmin
                                      units->pixels-x))
                            (floor (* ymin
                                      units->pixels-y))
                            (ceiling (* xmax
                                        units->pixels-x))
                            (ceiling (* ymax
                                        units->pixels-y))))))))))

(defun text-width (drawable font string &key start end)
  "Returns width of text bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (xmax bbox) (xmin bbox))))

(defun text-height (drawable font string &key start end)
  "Returns height of text bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (ymax bbox) (ymin bbox))))

(defun text-line-bounding-box (drawable font string &key start end)
  "Returns text line bounding box. @var{drawable} must be window, pixmap 
or screen. Text line bounding box is bigger than text bounding box. Its
height is ascent + descent, width is sum of advance widths minus sum of kernings."
  (when (and start end)
    (setf string (subseq string start end)))
  (multiple-value-bind (dpi-x dpi-y)
      (screen-dpi (drawable-screen drawable))
    (multiple-value-bind (string-line-bboxes exists-p)
        (gethash (cons dpi-x dpi-y) 
		 (gv font :string-line-bboxes))
      (unless exists-p
	(setf string-line-bboxes
	      (setf (gethash (cons dpi-x dpi-y) (gv font :string-line-bboxes))
		    (make-hash-table :test 'equal))))
      (or (gethash string string-line-bboxes)
          (setf (gethash string string-line-bboxes)
                (with-font-loader (loader font)
                  (let* ((units->pixels-x (font-units->pixels-x drawable font))
                         (xmin 0)
                         (ymin (font-descent drawable font))
                         (ymax (font-ascent drawable font))
                         (string-length (length string))
                         (xmax 
			  (if (> string-length 0)
			      (zpb-ttf:advance-width (zpb-ttf:find-glyph
						      (elt string 0)
						      loader))
			      0)))
                    (if (zpb-ttf:fixed-pitch-p loader)
                        (setf xmax (* xmax string-length))
                        (do ((i 1 (1+ i)))
                            ((>= i string-length))
                          (incf xmax
                                (+ (zpb-ttf:advance-width (zpb-ttf:find-glyph 
							   (elt string i)
							   loader))
                                   (zpb-ttf:kerning-offset 
				    (elt string (1- i)) 
				    (elt string i)
				    loader)))))
                    (vector (floor (* xmin units->pixels-x))
                            ymin
                            (ceiling (* xmax units->pixels-x))
                            ymax))))))))

(defun text-line-width (drawable font string &key start end)
  "Returns width of text line bounding box. @var{drawable} must be window, pixmap or screen. 
It is sum of advance widths minus sum of kernings."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (xmax bbox) (xmin bbox))))

(defun text-line-height (drawable font string &key start end)
  "Returns height of text line bounding box. @var{drawable} must be window, pixmap or screen."
  (when (and start end)
    (setf string (subseq string start end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (ymax bbox) (ymin bbox))))

(defun xmin (bounding-box)
  "Returns left side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 0))))

(defun ymin (bounding-box)
  "Returns bottom side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 1))))

(defun xmax (bounding-box)
  "Returns right side x of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 2))))

(defun ymax (bounding-box)
  "Returns top side y of @var{bounding-box}"
  (typecase bounding-box
    (vector (elt bounding-box 3))))

;;; Font rendering 
(defun clamp (value min max)
  "Clamps the value 'value' into the range [min,max]."
  (max min (min max value)))

(defun make-state (font)
  "Wrapper around antialising and not antialiasing renderers."
  (if (gv font :antialias)
      (aa:make-state)
      (aa-bin:make-state)))

(defun aa-bin/update-state (state paths)
  "Update state for not antialiasing renderer."
  (if (listp paths)
      (dolist (path paths)
        (aa-bin/update-state state path))
      (let ((iterator (paths:path-iterator-segmented paths)))
        (multiple-value-bind (i1 k1 e1) (paths:path-iterator-next iterator)
          (declare (ignore i1))
          (when (and k1 (not e1))
            ;; at least 2 knots
            (let ((first-knot k1))
              (loop
                 (multiple-value-bind (i2 k2 e2) (paths:path-iterator-next iterator)
                   (declare (ignore i2))
                   (aa-bin:line-f state
                           (paths:point-x k1) (paths:point-y k1)
                           (paths:point-x k2) (paths:point-y k2))
                   (setf k1 k2)
                   (when e2
                     (return))))
              (aa-bin:line-f state
                      (paths:point-x k1) (paths:point-y k1)
                      (paths:point-x first-knot) (paths:point-y first-knot)))))))
  state)

(defun update-state (font state paths)
  "Wrapper around antialising and not antialiasing renderers."
  (if (gv font :antialias)
      (vectors:update-state state paths)
      (aa-bin/update-state state paths)))

(defun cells-sweep (font state function &optional function-span)
  "Wrapper around antialising and not antialiasig renderers."
  (if (gv font :antialias)
      (aa:cells-sweep state function function-span)
      (aa-bin:cells-sweep state function function-span)))

(defun text-pixarray (drawable font string)
  "Render a text string of 'face', returning a 2D (unsigned-byte 8) array suitable as
an alpha mask, and dimensions. This function returns five values: alpha mask byte
array, x-origin, y-origin (subtracted from position before rendering), horizontal and
vertical advances.  @var{drawable} must be window or pixmap."
  (multiple-value-bind (dpi-x dpi-y)
      (screen-dpi (drawable-screen drawable))
    (multiple-value-bind (string-alpha-maps exists-p)
        (gethash (cons dpi-x dpi-y) (gv font :string-alpha-maps) (make-hash-table :test 'equal))
      (unless exists-p
        (setf (gethash (cons dpi-x dpi-y) (gv font :string-alpha-maps)) string-alpha-maps))
      (apply 
       'values 
       (or (gethash string string-alpha-maps)
           (setf (gethash string string-alpha-maps)
                 (with-font-loader (loader font)
                   (let* ((bbox (text-bounding-box drawable font string))
                          (min-x (xmin bbox))
                          (min-y (ymin bbox))
                          (max-x (xmax bbox))
                          (max-y (ymax bbox))
                          (width  (- max-x min-x))
                          (height (- max-y min-y)))
                     (if (or (zerop width) (zerop height))
                         (list nil 0 0 0 0)
                         (let* ((units->pixels-x (font-units->pixels-x drawable font))
                                (units->pixels-y (font-units->pixels-y drawable font))
                                (array (make-array (list height width)
                                                   :initial-element 0
                                                   :element-type '(unsigned-byte 8)))
                                (state (make-state font))
                                (paths (paths-ttf:paths-from-string
					loader string
					:offset (paths:make-point (- min-x) max-y)
					:scale-x units->pixels-x
					:scale-y (- units->pixels-y))))
                           
                           (when (gv font :underline)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* units->pixels-y 
							 (zpb-ttf:underline-position loader)))
                                    (underline-path
				     (paths:make-rectangle-path
				      0 (+ max-y (- underline-offset))
				      max-x (+ max-y (- underline-offset) thickness))))
                               (push underline-path paths)))
                           (when (gv font :strikethrough)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* 2 units->pixels-y
							 (zpb-ttf:underline-position loader)))
                                    (line-path
				     (paths:make-rectangle-path 
				      0 (+ max-y underline-offset) 
				      max-x (+ max-y underline-offset thickness))))
                               (push line-path paths)))
                           (when (gv font :overline)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* units->pixels-y
							 (zpb-ttf:underline-position loader)))
                                    (ascend (* units->pixels-y (zpb-ttf:ascender loader)))
                                    (overline-path
				     (paths:make-rectangle-path
				      0 (- max-y ascend underline-offset)
				      max-x
				      (- max-y ascend underline-offset thickness))))
                               (push overline-path paths)))
                           (update-state font state paths)
                           (cells-sweep
			    font state
			    (lambda (x y alpha)
			      (when (and (<= 0 x (1- width))
					 (<= 0 y (1- height)))
				(setf alpha (min 255 (abs alpha))
				      (aref array y x) 
				      (clamp
				       (floor (+ (* (- 256 alpha) (aref array y x))
						 (* alpha 255))
					      256)
                                                                    0 255)))))
                           (list array 
                                 min-x
                                 max-y
                                 width
                                 height)))))))))))


(defun text-line-pixarray (drawable font string)
  "Render a text line of 'face', returning a 2D (unsigned-byte 8) array suitable as
an alpha mask, and dimensions. This function returns five values: alpha mask byte
array, x-origin, y-origin (subtracted from position before rendering), horizontal and
vertical advances.  @var{drawable} must be window or pixmap."
  (multiple-value-bind (dpi-x dpi-y)
      (screen-dpi (drawable-screen drawable))
    (multiple-value-bind (string-line-alpha-maps exists-p)
        (gethash (cons dpi-x dpi-y)
;;		 (font-string-line-alpha-maps font)
		 (gv font :string-line-alpha-maps))
      (unless exists-p
	(setf string-line-alpha-maps
	      (setf (gethash (cons dpi-x dpi-y)
			     (gv font :string-line-alpha-maps))
		    (make-hash-table :test 'equal))))
      (apply 
       'values
       (or (gethash string string-line-alpha-maps)
           (setf (gethash string string-line-alpha-maps) 
                (with-font-loader (loader font)
                   (let* ((bbox (text-line-bounding-box drawable font string))
                          (min-x (xmin bbox))
                          (min-y (ymin bbox))
                          (max-x (xmax bbox))
                          (max-y (ymax bbox))
                          (width  (- max-x min-x))
                          (height (- max-y min-y)))
                     (if (or (= 0 width) (= 0 height))
                         (list nil 0 0 0 0)
                         (let* ((units->pixels-x (font-units->pixels-x drawable font))
                                (units->pixels-y (font-units->pixels-y drawable font))
                                (array (make-array (list height width)
                                                   :initial-element 0
                                                   :element-type '(unsigned-byte 8)))
                                (state (make-state font))
                                (paths (paths-ttf:paths-from-string
					loader string
					:offset (paths:make-point (- min-x) max-y)
					:scale-x units->pixels-x
					:scale-y (- units->pixels-y))))
                           (when (gv font :underline)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* units->pixels-y
							 (zpb-ttf:underline-position loader)))
                                    (underline-path (paths:make-rectangle-path
						     0 (+ max-y (- underline-offset))
						     max-x (+ max-y (- underline-offset) 
							      thickness))))
                               (push underline-path paths)))
                           (when (gv font :strikethrough)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* 2 units->pixels-y
							 (zpb-ttf:underline-position loader)))
                                    (line-path (paths:make-rectangle-path
						0 (+ max-y underline-offset) max-x
						(+ max-y underline-offset thickness))))
                               (push line-path paths)))
                           (when (gv font :overline)
                             (let* ((thickness (* units->pixels-y
						  (zpb-ttf:underline-thickness loader)))
                                    (underline-offset (* units->pixels-y
							 (zpb-ttf:underline-position loader)))
                                    (ascend (* units->pixels-y (zpb-ttf:ascender loader)))
                                    (overline-path
				     (paths:make-rectangle-path
				      0 (- max-y ascend underline-offset)
				      max-x
				      (- max-y ascend underline-offset thickness))))
                               (push overline-path paths)))
                           (update-state font state paths)
                           (cells-sweep
			    font state
			    (lambda (x y alpha)
			      (when (and (<= 0 x (1- width))
					 (<= 0 y (1- height)))
				(setf alpha (min 255 (abs alpha))
				      (aref array y x) (clamp
							(floor (+ (* (- 256 alpha) (aref array y x))
								  (* alpha 255))
							       256)
							0 255)))))
                           (list array 
                                 min-x
                                 max-y
                                 width
                                 height)))))))))))

(defun update-foreground (drawable gcontext font)
  "Lazy updates foreground for drawable. @var{drawable} must be window or pixmap."
  (let ((pixmap (or (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                    (setf (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                          (xlib:create-pixmap 
                           :drawable drawable
                           :depth (xlib:drawable-depth drawable)
                           :width 1 :height 1)))))
    (let ((color (the xlib:card32 
                      (if (gv font :overwrite-gcontext)
                          (gv font :foreground)
                          (xlib:gcontext-foreground gcontext)))))
      (when (or (null (getf (xlib:drawable-plist drawable) :ttf-foreground))
                (/= (getf (xlib:drawable-plist drawable) :ttf-foreground)
                    color))
        (let ((previous-color (xlib:gcontext-foreground gcontext)))
          (setf (xlib:gcontext-foreground gcontext) color)
          (xlib:draw-point pixmap gcontext 0 0)
          (setf (xlib:gcontext-foreground gcontext) previous-color)
          (setf (getf (xlib:drawable-plist drawable) :ttf-foreground) color))))))

(defun update-background (drawable gcontext font x y width height)
  "Lazy updates background for drawable. @var{drawable} must be window or pixmap."
  (let ((previous-color (xlib:gcontext-foreground gcontext))
        (color (the xlib:card32 
                    (if (gv font :overwrite-gcontext)
                        (gv font :background)
                        (xlib:gcontext-background gcontext)))))
    (setf (xlib:gcontext-foreground gcontext) color)
    (xlib:draw-rectangle drawable gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) previous-color)))

;;; Caching X11 objects
(defun get-drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-surface)
      (setf (getf (xlib:drawable-plist drawable) :ttf-surface)
            (xlib:render-create-picture 
             drawable 
             :format (first (xlib::find-matching-picture-formats
			     (xlib:drawable-display drawable)
			     :depth (xlib:drawable-depth drawable)))))))

(defun get-drawable-pen-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-pen)
      (setf (getf (xlib:drawable-plist drawable) :ttf-pen)
            (xlib:render-create-picture
             (or (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                 (setf (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
                       (xlib:create-pixmap 
                        :drawable drawable
                        :depth (xlib:drawable-depth drawable)
                        :width 1 :height 1)))
             :format (first (xlib::find-matching-picture-formats
			     (xlib:drawable-display drawable)
			     :depth (xlib:drawable-depth drawable)))
             :repeat :on))))

(defun display-alpha-picture-format (display)
  (or (getf (xlib:display-plist display) :ttf-alpha-format)
      (setf (getf (xlib:display-plist display) :ttf-alpha-format)
            (first
             (xlib:find-matching-picture-formats
              display
              :depth 8 :alpha 8 :red 0 :blue 0 :green 0)))))

;;; Drawing text

(defun draw-text (drawable gcontext font string x y
		  &key start end draw-background-p)
  "Draws text string using @var{font} on @var{drawable} with graphic context
@var{gcontext}. @var{x}, @var{y} are the left point of base line. @var{start} and
@var{end} are used for substring rendering. If @var{gcontext} has background color,
text bounding box will be filled with it. Text line bounding box is bigger than text
bounding box. @var{drawable} must be window or pixmap."
  (when (and start end)
    (when (>= start end)
      (return-from draw-text))
    (setf string (subseq string start end)))
  (multiple-value-bind (alpha-data min-x max-y width height)
      (text-pixarray drawable font string)
    (when (or (= 0 width) (= 0 height))
      (return-from draw-text))
    (let* ((display (xlib:drawable-display drawable))
           (image (xlib:create-image :width width :height height
				     :depth 8 :data alpha-data))
           (alpha-pixmap (xlib:create-pixmap :width width :height height
					     :depth 8 :drawable drawable))
           (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
           (alpha-picture
             (progn
               (xlib:put-image alpha-pixmap alpha-gc image :x 0 :y 0)
               (xlib:render-create-picture 
		alpha-pixmap :format (display-alpha-picture-format display))))
           (source-picture (get-drawable-pen-picture drawable))
           (destination-picture (get-drawable-picture drawable)))
      (update-foreground drawable gcontext font)
      (when draw-background-p
        (update-background drawable gcontext font
			   (+ x min-x) (- y max-y) width height))
      ;; Sync the destination picture with the gcontext
      (setf (xlib:picture-clip-x-origin destination-picture) 
	    (xlib:gcontext-clip-x gcontext))
      (setf (xlib:picture-clip-y-origin destination-picture)
	    (xlib:gcontext-clip-y gcontext))
      (setf (xlib:picture-subwindow-mode destination-picture)
	    (xlib:gcontext-subwindow-mode gcontext))
      (setf (xlib::picture-clip-mask destination-picture)
            (xlib::gcontext-clip-mask gcontext))
      (xlib:render-composite :over source-picture
			     alpha-picture destination-picture
			     0 0 0 0
			     (+ x min-x) (- y max-y) width height)
      nil)))

(defun draw-text-line (drawable gcontext font string x y 
		       &key start end draw-background-p)
  "Draws text string using @var{font} on @var{drawable} with graphic context
@var{gcontext}. @var{x}, @var{y} are the left point of base line. @var{start} and
@var{end} are used for substring rendering.  If @var{gcontext} has background color,
text line bounding box will be filled with it. Text line bounding box is bigger than
text bounding box.  @var{drawable} must be window or pixmap."
  (when (and start end)
    (when (>= start end)
      (return-from draw-text-line))
    (setf string (subseq string start end)))
  (multiple-value-bind (alpha-data min-x max-y width height)
      (text-line-pixarray drawable font string)
    (when (or (= 0 width) (= 0 height))
      (return-from draw-text-line))
    (let* ((display (xlib:drawable-display drawable))
           (image (xlib:create-image 
		   :width width :height height 
		   :depth 8 :data alpha-data))
           (alpha-pixmap (xlib:create-pixmap :width width :height height 
					     :depth 8 :drawable drawable))
           (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
           (alpha-picture
	    (progn
	      (xlib:put-image alpha-pixmap alpha-gc image :x 0 :y 0)
	      (xlib:render-create-picture
	       alpha-pixmap :format (display-alpha-picture-format display))))
           (source-picture (get-drawable-pen-picture drawable))
           (destination-picture (get-drawable-picture drawable)))
      (update-foreground drawable gcontext font)
      (when draw-background-p
        (update-background drawable gcontext font
			   (+ x min-x) (- y max-y)
			   width height))
      ;; Sync the destination picture with the gcontext
      (setf (xlib:picture-clip-x-origin destination-picture)
	    (xlib:gcontext-clip-x gcontext))
      (setf (xlib:picture-clip-y-origin destination-picture)
	    (xlib:gcontext-clip-y gcontext))
      (setf (xlib:picture-subwindow-mode destination-picture)
	    (xlib:gcontext-subwindow-mode gcontext))
      (setf (xlib::picture-clip-mask destination-picture)
            (xlib::gcontext-clip-mask gcontext))
      (xlib:render-composite :over 
			     source-picture alpha-picture destination-picture
			     0 0 0 0 (+ x min-x) (- y max-y) width height)
      nil)))

;;; Test utils

(defun trgrey (i)
  "Visualize alpha mask using graphic characters"
  (cond
    ((> i 200) "██")
    ((> i 150) "▓▓")
    ((> i 100) "▒▒")
    ((> i 50)  "░░")
    (t "  ")))

(defun print-pixarray (array)
  "Print 2d array of alpha mask using graphic characters."
  (do ((i 0 (1+ i)))
      ((>= i (array-dimension array 0)) nil)
    (do ((j 0 (1+ j)))
        ((>= j (array-dimension array 1)) nil)
      (format t "~A" (trgrey (aref array i j))))
    (format t "~%")))

(defun font-lines-height (drawable font lines-count)
  "Returns text lines height in pixels. For one line height is ascender+descender. 
For more than one line height is ascender+descender+linegap."
  (if (> lines-count 0)
      (+ (+ (xft:font-ascent drawable font)
            (- (xft:font-descent drawable font)))
         (* (1- lines-count) (+ (xft:font-ascent drawable font)
                                (- (xft:font-descent drawable font))
                                (xft:font-line-gap drawable font))))
      0))

;;; "clx-truetype" goes here. Hacks and glory await!

