;-*- Mode: Lisp; Package: CCL -*-

;;;  10/3/03 RGA --- Commented out some carbon incompatable code.
(in-package :ccl)

;; ****************************************************************

(defvar *spare-str255*)
(defvar *spare-rect-1*)
(defvar *spare-rect-2*)
(def-load-pointers spare-rectangle ()
  (setq *spare-rect-1* (#_newptr 8))
  (setq *spare-rect-2* (#_newptr 8))
  (setq *spare-str255* (#_newptr 256)))

(defmacro stuffrect (rect size-or-topleft-or-top &optional bottomright-or-left bottom right)
  `(progn
     (rect-internal ,rect ,size-or-topleft-or-top  ,bottomright-or-left ,bottom ,right)
     ,rect))

(defun some-ptinrect (rect &rest points)
  (declare (dynamic-extent points))
  (loop for p in points thereis (#_ptinrect p rect)))

(defmacro rect-internal (rect size-or-topleft-or-top &optional bottomright-or-left bottom right)
  (if (not bottomright-or-left)
    `(progn
       (setf (rref ,rect :rect.topleft) #@(0 0))
       (setf (rref ,rect :rect.bottomright) ,size-or-topleft-or-top)
       ,rect)
    (if bottom
      `(progn
         (setf (rref ,rect :rect.top) ,size-or-topleft-or-top)
         (setf (rref ,rect :rect.bottom) ,bottom)
         (setf (rref ,rect :rect.right) ,right)
         (setf (rref ,rect :rect.left) ,bottomright-or-left)
         ,rect)
      `(progn
         (setf (rref ,rect :rect.topleft) ,size-or-topleft-or-top)
         (setf (rref ,rect :rect.bottomright) ,bottomright-or-left)
         ,rect))))

(defmacro rect (size-or-topleft-or-top &optional bottomright-or-left bottom right)
  `(rect-internal *spare-rect-1* ,size-or-topleft-or-top ,bottomright-or-left ,bottom ,right))
(defmacro rect2 (size-or-topleft-or-top &optional bottomright-or-left bottom right)
  `(rect-internal *spare-rect-2* ,size-or-topleft-or-top ,bottomright-or-left ,bottom ,right))

(defmacro pstr (string)
  `(progn 
     (%pstr-pointer ,string *spare-str255*)
     *spare-str255*))

(defmacro rect-region (size-or-topleft-or-top &optional bottomright-or-left bottom right)
  (let ((it (gensym)))
    `(let ((,it (#_newrgn)))
       (#_rectrgn ,it (rect ,size-or-topleft-or-top ,bottomright-or-left ,bottom ,right))
       ,it)))

(defmacro region-rect (region)
  `(progn
     (setf (rref *spare-rect-1* :rect.topleft) (rref ,region region.rgnbbox.topleft)
           (rref *spare-rect-1* :rect.bottomright) (rref ,region :region.rgnbbox.bottomright))
     *spare-rect-1*))

;; ****************************************************************
;; A couple of debug print routines.

(defun rect-string (r &optional (message ""))
  (format nil "~A#<rect ~A ~A>" 
          message
          (point-string (rref r rect.topleft))
          (point-string (rref r rect.bottomright))))

(defun region-string (r &optional (message ""))
  (format nil "~A#<region ~A ~A>" 
          message
          (point-string (rref r region.rgnbbox.topleft))
          (point-string (rref r region.rgnbbox.bottomright))))


;; ****************************************************************
;; Some help for drawing the regions. Surround some drawing code with
;; drawing region, and the region is returned.

#|
;;; RGA these do not seem to be used, and also seem to
;;; have some carbon compatability issues.
(defmacro drawing-region (&rest body)
  "Returns a gcable region - i.e. no need to dispose"
  `(drawing-region-internal #'(lambda(),@body)))

(defun drawing-region-internal (drawer)
  (let ((manager-port (window-manager-port))
        (returned-region nil))
    (with-port manager-port
      (let ((region (make-region t)))
        (unwind-protect 
          (progn
            (#_openrgn)
            (funcall drawer)
            (#_closergn region)
            (setq returned-region region)
            (setq region nil))
          (when region 
            (dispose-region region)))))
    returned-region))
|#

(defvar *empty-region*)
(def-load-pointers utility-regions ()
  (setq *empty-region* (#_newrgn))
  (#_setrectrgn *empty-region* 0 0 0 0))

;; ****************************************************************
;; Some screen dimension accessors.

;;; RGA these don't seem to be used and conflict with internal ccl macros
#-apple
(defun menubar-height ()
  (%get-word (%int-to-ptr #xbaa)))

(defun screen-resolution ()
  (%get-word (%int-to-ptr 258)))

;; ****************************************************************

(defun window-manager-port ()
  (rlet ((it :pointer))
    ;; **TODO** find callers and substitute
    ;;(#_getcwmgrport it)
    (%get-ptr it)))

;; ****************************************************************
;; makes a (gcable) polygon which has 'points' as  vertices. 
#|
;;; RGA seems to be replaced with a method.
(defun make-polygon (&rest points)
  (with-port (window-manager-port)
    (let* ((poly (make-gcable-macptr $flags_disposhandle)))
      (%setf-macptr poly (#_openpoly))
      (unwind-protect 
        (progn
          (#_moveto :long (car points))
          (loop for p in (rest points)
                do (#_lineto :long p)))
        (#_closepoly))
      poly)))
|#

    
;; ****************************************************************
;; This came up because of a situation where handles from a gworld are locked
;; by with-pointers, and unlocked when done, except that the gworld has been disposed
;; by updategworld. And the version of hunlock that is used causes an error because the
;; the block is already free. This version does not cause the error.


(defmacro with-dereferenced-handle-perhaps-disposed-in-body ((var handle) &body body)
  `(progn
     (#_hlock ,handle)
     (let ((,var (%get-ptr ,handle)))
       (unwind-protect (progn ,@body)
         (#_hunlock ,handle)))))

;; ****************************************************************
;; view-send-behind, view-layer

(defmethod view-send-behind ((v simple-view) sibling)
  (let* ((container (view-container v))
         (siblings (and container (view-subviews container))))
    (when (and container (find sibling siblings) (neq v sibling))
      (let ((sib-layer (view-level sibling))
            (my-layer (view-level v)))
        (without-interrupts
         (unless (= my-layer (1+ sib-layer))
           (if (= sib-layer (1- (length siblings)))
             (set-view-level v sib-layer)
             (if (< my-layer sib-layer)
               (set-view-level v sib-layer)
               (set-view-level v (1+ sib-layer))))))))))
                        
(defun setf-method (symbol)
  (let ((f (list 'setf symbol)))
    (declare (dynamic-extent f))
    (fdefinition f)))

(defmacro @- (&rest points)
  (if (null (cdr points))
    (car points)
    `(@- (subtract-points ,(car points) ,(cadr points)) ,@(cddr points))))

(defmacro @+ (&rest points)
  (if (null (cdr points))
    (car points)
    `(@+ (add-points ,(car points) ,(cadr points)) ,@(cddr points))))

