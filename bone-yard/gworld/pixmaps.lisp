(in-package :ccl)

;© Copyright Apple Computer 1991-1992
;Alan Ruttenberg
;alanr@media.mit.edu
;Written to support work in the Apple Human Interface Group.

;; Tested only on color monitor.
;; remember the pixmap for a window is the screen pixmap (big!)

;;; 10/03/03  RGA --- Commented out a call which breaks under carbon.
;;;  The CICON stuff does not appear to get called.  Hope this is harmless.


(declaim (ignore ignore))

(deftype mcl-color () 'fixnum)

;; ****************************************************************
;; class definitions
;; Define mixins for things which have colortables, and which have pixmaps. The has-foo are mixins.
;; gworld class creates one at initialization time.
;; pixmap class is not used here, since it is rare that you want to create a pixmap, more
;; often creating a gworld is the right thing.
;; cicons are initialized from a resource id. Really should also have pointer to resource file.
;; redefine window here to mix in the has-pixmap.

(defclass has-colortable ()
  ((colortable-pointer :initarg :colortable-pointer :initform nil :accessor
                       colortable-pointer)
   (colortable-array :initarg :colortable-array :initform nil :accessor
                     colortable-array)))

(defclass has-pixmap (has-colortable)
  ((pixmap-pointer :initarg :pixmap-pointer :initform nil :accessor pixmap-pointer)))

(defclass pixmap (has-pixmap)
  ((pixmap-handle :initarg :pixmap-handle :initform nil :accessor pixmap-handle)))


(defclass cicon (has-pixmap reload-pointers-mixin)
  ((cicon-pointer :initarg :cicon-pointer :initform nil :accessor cicon-pointer)
   (cicon-handle :initarg :cicon-handle :initform nil :accessor cicon-handle)
   (id :initarg :id :initform nil :accessor id)))

;; ****************************************************************
;; accessors for the handles need to be defined by the parents
;; Everybody needs to be able to respond to a pixmap-handle and colortable-handle message

(defmethod pixmap-handle ((c cicon))
  (rref (cicon-pointer c) cicon.iconpmap :storage :pointer))

(defmethod colortable-handle ((p has-pixmap))
  (rref (pixmap-pointer p) :pixmap.pmtable :storage :pointer))

;; ****************************************************************
;; accessors for windows. Since we can't modify window to have an extra instance variable, store
;; pixmap-pointer on the plist

(defmethod pixmap-pointer ((w window))
  (view-get w 'pixmap-pointer))

(defmethod (setf pixmap-pointer) (new (w window))
  (setf (view-get w 'pixmap-pointer) new))

(defmethod colortable-pointer ((w window))
  (view-get w 'colortable-pointer))

(defmethod (setf colortable-pointer) (new (w window))
  (setf (view-get w 'colortable-pointer) new))

(defmethod pixmap-handle ((w window))
  (assert (window-color-p w) (w) "Only color windows have pixmaps")
  (rref (wptr w) :cwindowrecord.port.portpixmap))

;; ****************************************************************
;; handle and pointer locking. 
;; The protocol is to use with-pixmap, or with colortable. Inside that, you need to lock or
;; otherwise make available the pointers, which usually involves some sort of handle locking.
;; The with-locked-parent method does what needs to be done. Inside the the with-locked-parent
;; dynamic scope the foo-pointer method is valid.

(defmethod with-locked-parent ((anything t) continuation)
  (declare (ignore continuation))
  nil)

(defmethod with-locked-parent :around ((anything t) continuation)
  (funcall continuation))

(defmethod with-locked-parent :around ((c has-colortable) continuation)
   (with-dereferenced-handle-perhaps-disposed-in-body (ptr (colortable-handle c))
    (setf (colortable-pointer c) ptr)
    (multiple-value-prog1
      (call-next-method c continuation)
      (setf (colortable-pointer c) nil))))

(defmethod with-locked-parent :around ((p has-pixmap) continuation)
  (with-dereferenced-handle-perhaps-disposed-in-body (ptr (pixmap-handle p))
    (setf (pixmap-pointer p) ptr)
    (multiple-value-prog1
      (call-next-method p continuation)
      (setf (pixmap-pointer p) nil))))

;; duplicate the above two methods for windows, since we can't add the mixin
(defmethod with-locked-parent :around ((c window) continuation)
  (with-dereferenced-handle-perhaps-disposed-in-body (ptr (colortable-handle c))
    (setf (colortable-pointer c) ptr)
    (multiple-value-prog1
      (call-next-method c continuation)
      (setf (colortable-pointer c) nil))))

(defmethod with-locked-parent :around ((p window) continuation)
  (with-dereferenced-handle-perhaps-disposed-in-body (ptr (pixmap-handle p))
    (setf (pixmap-pointer p) ptr)
    (multiple-value-prog1
      (call-next-method p continuation)
      (setf (pixmap-pointer p) nil))))

(defmethod with-locked-parent :around ((c cicon) continuation)
  (with-dereferenced-handle-perhaps-disposed-in-body (ptr (cicon-handle c))
    (setf (cicon-pointer c) ptr)
    (multiple-value-prog1
      (call-next-method c continuation)
      (setf (cicon-pointer c) nil))))

;; ****************************************************************
;; some syntax to make this look easy

(defmacro with-colortable ((variable instance) &rest body)
  `(with-locked-parent ,instance
     #'(lambda()
        (let ((,variable (colortable-pointer ,instance)))
          ,@body))))

(defmacro with-pixmap ((variable instance) &rest body)
  `(with-locked-parent ,instance
     #'(lambda()
        (let ((,variable (pixmap-pointer ,instance)))
          ,@body))))


;; ****************************************************************
;; cicon methods 

;; cicons don't have their fields properly intialized until they are plotted the first time.
;; so plot this just outside the window
(defun kludge-initialize-icon (icon)
  (rlet ((it :pointer))
    ;; **TODO** fix obsolete trap
    ;;(#_getcwmgrport it)
    (with-port (%get-ptr it)
      (rlet ((r :rect :topleft #@(-32 -32) :bottomright #@(0 0)))
        (#_plotcicon r icon)))))

(defmethod initialize-instance ((icon cicon) &key)
  (call-next-method)
  (initialize-pointers icon)
  (colortable-to-array icon)
  )

(defmethod initialize-pointers ((icon cicon))
  (setf (cicon-handle icon) 
        (#_getcicon (id icon)))
  (when (equal (cicon-handle icon) (%null-ptr))
    (error "The icon was not found"))
  (kludge-initialize-icon (cicon-handle icon)))

;; this isn't quite right yet, since if dither is chosen, the masking isn't done correctly.

(defmethod plot-icon ((c cicon) top left bottom right &optional dither?)
  (if dither?
    (with-pixmap (ipm c)
      (rlet ((r :rect :top top :left left :bottom bottom :right right)
             (rs :rect :topleft #@(0 0) :bottomright #@(32 32)))
        (#_copybits ipm (rref (wptr *current-view*) :windowrecord.portbits)
         rs r 64 (%null-ptr))))
    (rlet ((r :rect :top top :left left :bottom bottom :right right))
      (#_plotcicon r (cicon-handle c)))))

;; ****************************************************************
;; colortable-methods

(defmethod suitable-array ((c has-colortable) array size)
  (and (arrayp array) 
       (subtypep 'mcl-color (array-element-type array))
       (null (cdr (array-dimensions array)))
       (>= (array-dimension array 0) size)))

;; copy the colortable to a lisp array
(defmethod colortable-to-array ((c has-colortable) &optional array)
  (with-colortable (table c)
    (let ((size (rref table :colortable.ctsize :storage :pointer)))
      (let ((array (if (and array (suitable-array c array size))
                     array
                     (make-array (1+ size) :element-type 'mcl-color))))
        (let ((ctarray (rref table :colortable.cttable :storage :pointer)))
          (loop for i from 0 to size
                do
                (setf (aref array i)
                      (rgb-to-color
                       (rref (rref ctarray (:cspecarray.array i)) :colorspec.rgb))))
          (setf (colortable-array c) array))))))

(defmethod array-to-colortable ((c has-colortable) &optional (array (colortable-array c)))
  (with-colortable (table c)
    (let ((size (rref table :colortable.ctsize :storage :pointer)))
      (when (not (suitable-array c array size))
        (error "This array doesn't make sense as a color table"))
      (let ((ctarray (rref table :colortable.cttable :storage :pointer)))
        (loop for i from 0 to size
              do
              (color-to-rgb
               (aref array i)
               (rref (rref ctarray (:cspecarray.array i)) :colorspec.rgb)))))))

;; value is between 0 and 1, where 0 is unchanged, and 1 is total
;; scale can be saturation (makes it become grayer),
;; lightness (make it become lighter), or value (makes is darker)

(defmethod array-to-colortable-scale-value ((c has-colortable) scale value &optional (array (colortable-array c)))
  (with-colortable (table c)
    (let ((size (rref table :colortable.ctsize :storage :pointer)))
      (when (not (suitable-array c array size))
        (error "This array doesn't make sense as a color table"))
      (let ((ctarray (rref table :colortable.cttable :storage :pointer)))
        (loop for i from 0 to size
              do
              (rlet ((hsv :rgbcolor)
                     (rgb :rgbcolor))
                (color-to-rgb (aref array i) rgb)
                (#_rgb2hsv rgb hsv)
                (ecase value
                  (:saturation
                   (setf (rref hsv rgbcolor.green) (floor (* scale (rref hsv rgbcolor.green)))))
                  (:value 
                   (setf (rref hsv rgbcolor.blue) (floor (* scale (rref hsv rgbcolor.blue)))))
                  (:lightness 
                   (setf (rref hsv rgbcolor.green) (floor (* scale (rref hsv rgbcolor.green))))
                   (setf (rref hsv rgbcolor.blue) (+ (rref hsv rgbcolor.blue)
                                                     (floor (* (- 1 scale) (- 65535 (rref hsv rgbcolor.blue))))))))
                (#_hsv2rgb hsv (rref (rref ctarray (:cspecarray.array i)) :colorspec.rgb))))))))

(defmethod colortable-seed ((c has-colortable))
  (with-colortable (table c)
    (rref table :colortable.ctseed :storage :pointer)))

(defmethod (setf colortable-seed) (new-seed (c has-colortable))
  (with-colortable (table c)
    (setf (rref table :colortable.ctseed :storage :pointer) new-seed)))
