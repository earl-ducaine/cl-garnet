;; The main rendering engine

(in-package :clx-freetype2-renderer)
(export '(draw-glyphs
	  text-width))
(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))
;; (defmacro xlib-lazy-update (xlib-expr symbol ) 
;;   `(or (getf ,xlib-expr 'symbol)
;;        (setf (getf ,xlib-expr 'symbol)
;; 	     @body)))
(defparameter *ft2-face-cache* (make-hash-table :test 'equal :size 256)
  "Cache of FreeType2 bitmaps and size info")
(defun cache-char (face char vertical-p cache)
  (or (gethash char *ft2-face-cache*)
      (setf (gethash char *ft2-face-cache*) 
	    (render-char face char vertical-p))))
(defun render-char (face char vertical-p)
  (multiple-value-bind (bitmap advance top left)
      (ft2:default-load-render face char vertical-p)
    (vector (bitmap-to-cffi-array bitmap) advance top left)));(ft2:bitmap-to-array bitmap)

;; TODO should probably import these funs from cl-freetype2
(defun nth-mono-pixel (row n)
  (multiple-value-bind (q offset) (truncate n 8)
    (let ((byte (cffi-sys:%mem-ref row :unsigned-char q)))
    (if (logbitp (- 7 offset) byte) 1 0))))

(defun nth-gray-pixel (row n)
  (cffi-sys:%mem-ref row :unsigned-char n))

(defun bitmap-to-cffi-array (bitmap)
  "=> ARRAY

Convert `BITMAP` from internal `FT_Bitmap`'s internal representation to
a native array.  This is specified for a `FT-BITMAP-PIXEL-FORMAT` of `:MONO`,
`:GRAY`, `:LCD`, and `:LCD-V`.

Note that for :LCD and :LCD-V, the result is a either 3\\*width or
3\\*height, respectively.  This may change in the future."
  (let ((buffer (ft2::ft-bitmap-buffer bitmap))
        (rows (ft2::ft-bitmap-rows bitmap))
        (width (ft2::ft-bitmap-width bitmap))
        (pitch (ft2::ft-bitmap-pitch bitmap))
        (format (ft2::ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
                      (:mono #'nth-mono-pixel)
                      (:gray #'nth-gray-pixel)
                      (:lcd #'nth-gray-pixel)
                      (:lcd-v #'nth-gray-pixel)))
          (array (cffi:foreign-alloc :pointer 
				     :count rows 
				     :initial-element
				     (cffi:foreign-alloc :uint8 :count width))))
      (declare (function pixel-fn))
      #+-(format t "buffer: ~A rows: ~A width: ~A pitch: ~A format: ~A~%"
                 buffer rows width pitch format)
      (loop for i from 0 below rows
	 as ptr = (cffi-sys:inc-pointer buffer (* i pitch))
	 do (loop for j from 0 below width
	       do (cffi-sys:%mem-set (funcall pixel-fn ptr j) (cffi-sys:inc-pointer array (+ i j)) :unsigned-char)
		 #+-(format t "~a " (cffi-sys:%mem-ref (cffi-sys:inc-pointer array (+ i j)) :unsigned-char))
		 ) #+-(format t "~%")
      	 finally (return (values array format))))))

(defmethod initialize-instance :after ((this-font font) &key)
  (let ((this-style (slot-value this-font 'style))
	 (this-family (slot-value this-font 'family)))
    (unless this-style
      (setf (slot-value this-font 'style) (find-default-style this-family)))
    (check-valid-font-families (slot-value this-font 'family)
			       (slot-value this-font 'style)))
  (let* ((display (xlib:open-display ""))
	(screen (first (xlib:display-roots display))))
    (with-slots (family style ft-face size) this-font
    (setf ft-face (ft2:new-face (get-font-pathname family style)))
    (multiple-value-bind (dpi-x dpi-y) (screen-dpi screen)
      (ft2:set-char-size ft-face (* size 64) 0 dpi-x dpi-y))
    (loop for i from 37 to 255
       do (cache-char ft-face (code-char i) nil *ft2-face-cache*)))))

(defun load-render (face char vertical-p)
  (ft2:load-char face char (if vertical-p '(:vertical-layout) '(:default)))
  (let ((char-list (cache-char face char vertical-p *ft2-face-cache*)))
    (when char-list
      (values (aref char-list 0)
	      (aref char-list 1)
	      (aref char-list 2)
	      (aref char-list 3)))))

(defun print-alpha-data (array)
  (loop for i from 0 below (array-dimension array 0)
     do (loop for j from 0 below (array-dimension array 1)
	     do (format t "~4d" (aref array i j)))
     do (princ #\Newline)))

(defun make-alpha-picture (pixmap gc image display)
  (xlib:put-image pixmap gc image :x 0 :y 0)
  (xlib:render-create-picture pixmap
			      :format (display-alpha-picture-format display)))
(defun update-foreground (drawable gcontext)
  "Lazy updates foreground for drawable. @var{drawable} must be window or pixmap."
  (let ((pixmap (or (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
                    (setf (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
                          (xlib:create-pixmap 
                           :drawable drawable
                           :depth (xlib:drawable-depth drawable)
                           :width 1 :height 1)))))
    (let ((color (the xlib:card32 (xlib:gcontext-foreground gcontext))))
      (when (or (null (getf (xlib:drawable-plist drawable) :ft2-foreground))
                (/= (getf (xlib:drawable-plist drawable) :ft2-foreground)
                    color))
        (let ((previous-color (xlib:gcontext-foreground gcontext)))
          (setf (xlib:gcontext-foreground gcontext) color)
          (xlib:draw-point pixmap gcontext 0 0)
          (setf (xlib:gcontext-foreground gcontext) previous-color)
          (setf (getf (xlib:drawable-plist drawable) :ft2-foreground) color))))))
(defun update-background (drawable gcontext x y width height)
  "Lazy updates background for drawable. @var{drawable} must be window or pixmap."
  (let ((previous-color (xlib:gcontext-foreground gcontext))
        (color (the xlib:card32 (xlib:gcontext-background gcontext))))
    (setf (xlib:gcontext-foreground gcontext) color)
    (xlib:draw-rectangle drawable gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) previous-color)))

(defun string-to-array (face string direction width height)
  (let* ((flags (if (or (eq direction :up-down)
                        (eq direction :down-up))
                    '(:vertical-layout)
                    '(:default)))
         (array (make-array (list height width) :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
    (ft2:do-string-render (face string bitmap x y
				:direction direction
				:load-function #'load-render)
      (let ((barray (ft2:bitmap-to-array bitmap)))
        (case direction
          (:left-right (ablit array barray :x x :y y))
          (:right-left (ablit array barray :x (+ width x) :y y))
          (:up-down    (ablit array barray :x x :y y))
          (:down-up    (ablit array barray :x x :y (+ height y))))))
    array))
(defun text-width (face string)
  (round (ft2:string-pixel-width face string)))
(defun text-height (face string)
  (round (ft2:string-pixel-height face string)))
(defun render-glyphs (drawable gcontext x y string font update-bg-p)
  "Actually handle the rendering"
  (let* ((display (xlib:drawable-display drawable))
	 (face (slot-value font 'ft-face))
	 (width (text-width face string))
	 (height (text-height face string))
	 (alpha-data (string-to-array face string :left-right width height))
	 (y-max (round (ft2:face-ascender-pixels face)))
	 (x-min (round (get-bearing-x #\t face)))
	 (x-pos (+ x x-min))
	 (y-pos (- y y-max))
	 (image (xlib:create-image :width width :height height 
	 			   :depth 8 :data alpha-data))
	 (alpha-pixmap (xlib:create-pixmap :width width :height height
					   :depth 8 :drawable drawable))
	 (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap 
					 :foreground (xlib:gcontext-foreground gcontext)
					 :background (xlib:gcontext-background gcontext)))
	 (alpha-picture (make-alpha-picture alpha-pixmap alpha-gc image display))
	 (source-picture (get-drawable-pen-picture drawable))
	 (dest-picture (get-drawable-picture drawable)))
    (update-foreground drawable gcontext)
    (when update-bg-p
      (update-background drawable gcontext x-pos y-pos  width height))
    (setf  
     (xlib:picture-clip-x-origin dest-picture) (xlib:gcontext-clip-x gcontext)
     (xlib:picture-clip-y-origin dest-picture) (xlib:gcontext-clip-y gcontext)
     (xlib:picture-subwindow-mode dest-picture) (xlib:gcontext-subwindow-mode gcontext)
     (xlib::picture-clip-mask dest-picture) (xlib::gcontext-clip-mask gcontext))
    (xlib:render-composite :over source-picture alpha-picture dest-picture 0 0 0 0 
			   x-pos y-pos  width height))
  nil)

(defun draw-glyphs (drawable gcontext x y string &key (start 0) end font update-bg-p)
  "Draw glyphs to gcontext depending on whether or not face was provided"
  (if font
      (render-glyphs drawable gcontext x y (if (and start end (not (>= start end)))
					       (subseq string start end)
					       string) font update-bg-p)
      (xlib:draw-glyphs drawable gcontext x y string :start start :end end)))
