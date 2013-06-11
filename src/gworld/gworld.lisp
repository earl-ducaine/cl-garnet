;-*- Mode: Lisp; Package: CCL -*-

(in-package :ccl)

;© Copyright Apple Computer 1991-1992
;Alan Ruttenberg
;alanr@media.mit.edu
;Written to support work in the Apple Human Interface Group.

(defclass gworld (has-pixmap)
  ((pixmap-handle :initarg :pixmap-handle :initform nil :accessor pixmap-handle)
   (gworld :initarg :gworld :initform nil :accessor gworld)
   (depth :initarg :depth :initform nil :accessor depth)
   (gdevice :initarg :gdevice :initform (%null-ptr) :accessor gdevice)
   (colortable :initarg :colortable :initform nil :accessor colortable)
   (gray? :initarg :gray? :initform nil :accessor gray?)
   (size :initarg :size :initform #@(100 100) :accessor size)
   (temporary? :initarg :temporary? :initform nil :accessor temporary?)))

(defmethod with-locked-parent :around ((g gworld) continuation)
  (unwind-protect 
    (progn
      (let ((pm (#_getgworldpixmap (gworld g))))
        (#_lockpixels pm)
        (setf (pixmap-handle g) pm))
      (call-next-method g continuation))
    ;; have to be careful here. don't use pm, since it may have been deallocated in body, 
    ;; due to updategworld
    (#_unlockpixels (#_getgworldpixmap (gworld g)))
    (setf (pixmap-handle g) nil)))

;; ****************************************************************
;; gworld methods. If the colortable is not specified, then it defaults to the system colortable
;; for the appropriate depth, unless gray? is t, in which it is a grayscale colortable. A colortable
;; doesn't make sense for depths greater than 8

(defun default-gdevice ()
  (#_getmaxdevice (rect #@(-32000 -32000) #@(32000 32000))))

(defmethod initialize-instance ((g gworld) &rest args)
  (apply 'shared-initialize g t args)
  (if (depth g)
    (assert (member (depth g) '(1 2 4 8 16 32) :test 'eql) ((depth g))
            "depth of a gworld needs to be a power of 2")  
    (progn
      (when (%null-ptr-p (gdevice g))
        (setf (gdevice g) (default-gdevice)))
      (setf (depth g) (rref (rref (gdevice g) gdevice.gdpmap) pixmap.pixelsize))))
  (unless (colortable g)
    (default-colortable g))
  (unless (gworld g)
    (allocate-gworld g))
  (call-next-method))
;;; RGA --- This version seems to do some funny things.
#+comment
(defmethod default-colortable ((g gworld))
  (setf (colortable g)
        (if (gray? g)
          (#_getctable (case (depth g)
                         (2 34)
                         (4 36)
                         (8 40)
                         ((1 16 32) (%null-ptr))))
          (#_getctable (case (depth g)
                         (2 2)
                         (4 4)
                         (8 8)
                         ((1 16 32) (%null-ptr)))))))
;;; RGA This should work according to inside Mac.
(defmethod default-colortable ((g gworld))
  (setf (colortable g)
        (if (gray? g)
          (#_getctable (+ 32 (depth g)))
          (#_getctable (depth g)))))

(defmethod allocate-gworld ((g gworld))
  (rlet ((gw :pointer))
    (let ((result (#_newgworld gw
                   (depth g) 
                   (rect (size g))
                   (colortable g)
                   (gdevice g)
                   (if (temporary? g) 4 0))))
      (assert (zerop result) () "Error creating gworld")
      (setf (gworld g) (gworld-macptr (%get-ptr gw) t)))))

(defmacro with-focused-gworld ((gworld) &body body)
  (let ((p (make-symbol "PORT"))
        (g (make-symbol "GDEVICE")))
    `(rlet ((,p :pointer) (,g :pointer))
       (unwind-protect 
         (progn
           (#_getgworld ,p ,g)
           (#_setgworld (gworld ,gworld) (%null-ptr))
           ,@body
           )
         (#_setgworld (%get-ptr ,p) (%get-ptr ,g))))))

(defmethod update-gworld ((g gworld) &key colortable depth size gdevice)
  (assert (gworld g) () "Gworld should be non nil here!")
  (rlet ((gw :pointer))
    (%put-ptr gw (gworld g))
    (if (minusp 
         (#_updategworld gw
          (setf (depth g) (or depth (depth g)))
          (rect (setf (size g) (or size (size g))))
          (setf (colortable g) (or colortable (colortable g)))
          (setf (gdevice g) (or gdevice (gdevice g)))
          (gworld-update-method g)))
      (error "updategworld failed!")
      ;; do a setf of the macptr, so that we preserve whatever flags we had
      ;; primarily the disposegworld flag, if it exists.
      (%setf-macptr (gworld g) (%get-ptr gw))
      ))
  (with-focused-gworld (g)
    (let ((r (rect (or size (size g) ))))
      (#_cliprect r)
      (#_eraserect r))))

(defmethod dispose ((g gworld))
  (when (gworld g)
    ;; turn off the dispose flag, since we are disposing by hand.
    (dispose-gworld (gworld g)))
  (setf (gworld g) nil))

(defmethod gworld-update-method ((v gworld))
  (ash 1 #$clippix))



(defmacro with-temp-gworld ((width height depth gworld &optional ctable gray?) &body body)
  `(with-temp-gworld-1 ,width ,height ,depth ,ctable ,gray?
     #'(lambda(,gworld) 
        ,@Body)))

(defun with-temp-gworld-1 (width height depth ctable gray? continuation)
  (let ((world (make-instance 'gworld :size (make-point width height) :depth depth :colortable ctable :gray? gray?
                           :temporary? t)))
    (rlet ((r :rect :topleft #@(0 0) :bottomright (make-point width height)))
      (unwind-protect
        (progn
          (with-focused-gworld (world)
            (#_cliprect r)
            (#_eraserect r))
          (funcall continuation world))
        (dispose world)))))
