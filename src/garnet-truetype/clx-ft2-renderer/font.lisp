(in-package #:clx-freetype2-renderer)
(export '(font))
(defclass font ()
  ((family :type string :initarg :family :accessor font-family :documentation "Font family.")
   (style :type string 
	  :initarg :style 
	  :initform nil
	  :accessor font-style 
	  :documentation "Font style. For e.g. Regular, Italic, Bold, Bold Italic.")
   (size :type numeric 
	 :initarg :size 
	 :accessor font-size 
	 :initform 12 
	 :documentation "Font size in points.")
   (underline 
    :type boolean 
    :initarg :underline 
    :initform nil 
    :accessor font-underline 
    :documentation "Draw line under text string.")
   (strikethrough 
    :type boolean 
    :initarg :strikethrough 
    :initform nil 
    :accessor font-strikethrough 
    :documentation "Draw strike through text string.")
   (overline 
    :type boolean 
    :initarg :overline 
    :initform nil 
    :accessor font-overline 
    :documentation "Draw line over text string.")
   (ft-face :type freetype2-types:ft-face
	    :accessor font-face
	    :initform nil
	    :documentation "Internal reference to the freetype2 face")
   (char->glyph-info 
    :type hash-table
    :initform (make-hash-table :size 256)
    :documentation "Cache for glyph info")
   ;; (string-alpha-maps 
   ;;  :type hash-table 
   ;;  :initform (make-hash-table :test 'equal) 
   ;;  :accessor font-string-alpha-maps
   ;;  :documentation "Cache for text alpha maps")
   )
  (:documentation "Class for representing font information."))

(defun find-default-style (family)
  "Tries to pick a reasonable style"
  (let ((styles (get-font-styles family)))
    (if (= (length styles) 1) 
	(car styles)
	(or (find "Roman" styles :test #'string-equal)
	    (find "Regular" styles :test #'string-equal)
	    (find "Medium" styles :test #'string-equal)
	    (find "Book" styles :test #'string-equal)
	    ;; Give up and take the first of the list
	    (car styles)))))
(defun get-font-pathname (family style)
  (let ((font-hash (gethash family *font-cache*)))
    (when font-hash 
      (gethash style font-hash))))
(defun check-valid-font-families (family style)
  (when (null (get-font-pathname family style))
    (error "Font is not found: ~A-~A" family style)))

;; (defmethod initialize-instance :before 
;;     ((instance font) &rest initargs &key family style &allow-other-keys)
;;   (declare (ignorable initargs))
;;   (check-valid-font-families family style))

(defmethod (setf font-family) :before
  (family (instance font))
  (check-valid-font-families family (font-style instance)))

(defmethod (setf font-style) :before
  (style (instance font))
  (check-valid-font-families (font-family instance) style))

(defmethod (setf font-family) :after
  (family (font font)))

(defmethod (setf font-style) :after
  (style (font font)))

(defmethod (setf font-size) :after (value (font font))
  (set-face-size font (first (xlib:display-roots (xlib:open-display ""))) value))

(defmethod (setf font-underline) :after (value (font font)))

(defmethod (setf font-overline) :after (value (font font)))

(defmethod (setf font-face) :after (value (font font)))

(defgeneric font-equal (font1 font2)
  (:documentation "Returns t if two font objects are equal, else returns nil.")
  (:method ((font1 font) (font2 font))
    (and (string-equal (font-family font1)
                       (font-family font2))
         (string-equal (font-style font1)
                       (font-style font2))
         (= (font-size font1) (font-size font2))
         (eql (font-underline font1) (font-underline font2))
         (eql (font-strikethrough font1) (font-strikethrough font2))
         (eql (font-overline font1) (font-overline font2)))))
(defmethod print-object ((instance font) stream)
  "Pretty printing font object"
  (with-slots (family style underline strikethrough
		      overline)
      instance
    (if *print-readably*
        (format stream
                "#.(~S '~S ~S ~S ~S ~S ~S)"
                'cl:make-instance 'font
                :family family :style style :underline underline 
                :strikethrough strikethrough
                :overline overline)
        (format stream
                "#<'~S ~S ~S ~S ~S ~S >"
                'font
                :family family :style style :underline underline 
                :strikethrough strikethrough :overline overline ))))
