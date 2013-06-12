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
;;; Changes:
;;; 17-Feb-93 Dzg/Mickish  Fixed bug in Free-List
;;; 20-Sep-93 Fernando  Renamed "position" variables to "bit-position"
;;; 15-Jun-93 Changed Set-Line/Filling-Style to always call set-gc with the
;;;                :function parameter, even when the draw function is :no-op
;;; 21-May-93 Andrew Mickish -- Renamed black-xor-hack to HP-XOR-Hack
;;; 17-May-93 koz  Added "exposed-bbox" to the ever-growing win-update-info
;;; 16-May-93 amickish Added fix-update-slots-objects cell to win-update-info
;;; 12-Apr-93 koz  Moved set-*-style functions from macros.lisp to here
;;;                because they use macros defined in this file.
;;;  5-Mar-93 amickish moved shell-exec to utils.lisp
;;; 24-Feb-93 amickish moved *auxilliary-reconnect-routines* here from
;;;                open-and-close
;;; 22-Feb-93 koz/amickish Added Aggregate-Invalidate to Make-Object-Invalid
;;;                to properly handle :fix-update-slots in add-component
;;;  3-Feb-93 koz  Removed "last-invalid-obj", added "invalid-*-fastdraws"
;;;                from win-update-info struct.  Also, added 3 new macros,
;;;                "get-cons", "free-cons", and "free-list"
;;; 10-Dec-92 amickish  Removed *windows-that-have-never-been-updated*
;;;  6-Oct-92 koz  Added invalid-view-objects to win-update-info struct
;;; 26-May-92 ecp  Added new constant number-of-slots-of-update-info-struct.
;;; 01-Apr-92 amickish  Added copy-bbox-fn
;;; 11-Mar-92 ecp  Added two new fields width and height to win-update-info.
;;; 19-Feb-92 koz  Eliminated extra non-printable object at end of 
;;;		   invalid-objects list.
;;;  9-Dec-91 ecp  opal-gc has new stored-clip-mask field
;;; 26-Nov-91 ecp  changed erase-bbox to correctly handle double-buffered
;;;		   windows with background color.
;;; 26-Mar-91 sylvain  #+kcl patches
;;;  1-Oct-90 ecp  Made the print function for opal-gc not print font.
;;; 18-Jun-90 ecp  Added *clear* for erasing buffers.
;;;  5-Jun-90 dzg  Changed update-info structure to reduce storage allocation.
;;;  4-Jun-90 ecp  Altered erase-bbox to handle double buffering.
;;; 16-Apr-90 ecp  Moved defun of font-to-xfont from new-defs.lisp to
;;;		   create-instances2.lisp
;;; 19-Mar-90  Changed tile to stipple
;;;  9-Mar-90  Moved a bunch of defvars to defs.lisp.
;;;  5-Dec-89  Moved definition of new-garnet-window-name here from windows.lisp,
;;;            Added "#-cmu nil" to fix-font-path.
;;;             
(in-package "OPAL")

(defstruct bbox
  (x1 0 :type fixnum)
  (y1 0 :type fixnum)
  (x2 0 :type fixnum)
  (y2 0 :type fixnum)
  (valid-p nil :type (or t nil)))


;; Force-Computation-P is necessary since if an object R is in an aggregate A,
;; and you add-component that aggregate into another (visible) aggregate, then
;; R will be marked dirty, but it will not be added to the invalid-objects list
;; of the window, so at update time all its values in :update-slots-values will
;; be incorrect and will need to be recomputed.  Obtuse, but this works!

(defstruct (update-info (:print-function update-info-print-function))
	window
	old-bbox
	bits)

;;; This constant is used in debug/objsize.lisp to determine the
;;; size in bytes of an update-info structure.
;;; NOTE: IF YOU CHANGE THE DEFINITION OF UPDATE-INFO, BE SURE
;;; TO CHANGE THE VALUE OF THIS CONSTANT.
(defconstant number-of-slots-of-update-info-struct 3)

;;; The update-info-bits field is used to encode the following:
;;;   dirty-p
;;;   aggregate-p
;;;   invalid-p
;;;   force-computation-p
;;;   on-fastdraw-list-p

(defmacro bit-setter (object bit-position value)
  (cond ((eq value T)
	 ;; Value is T at compile time.
	 `(setf (update-info-bits ,object)
		(logior (update-info-bits ,object) ,(ash 1 bit-position))))
	((null value)
	 ;; Value is NIL at compile time.	 
	 `(setf (update-info-bits ,object)
		(logand (update-info-bits ,object)
			,(lognot (ash 1 bit-position)))))
	(t
	 ;; Value is not known at compile time
	 `(if ,value
	      (setf (update-info-bits ,object)
		    (logior (update-info-bits ,object) ,(ash 1 bit-position)))
	      (setf (update-info-bits ,object)
		    (logand (update-info-bits ,object)
			    ,(lognot (ash 1 bit-position))))))))

(defmacro update-info-dirty-p (object)
  `(logbitp 0 (update-info-bits ,object)))

(defsetf update-info-dirty-p (object) (value)
  `(bit-setter ,object 0 ,value))


(defmacro update-info-aggregate-p (object)
  `(logbitp 1 (update-info-bits ,object)))

(defsetf update-info-aggregate-p (object) (value)
  `(bit-setter ,object 1 ,value))


(defmacro update-info-invalid-p (object)
  `(logbitp 2 (update-info-bits ,object)))

(defsetf update-info-invalid-p (object) (value)
  `(bit-setter ,object 2 ,value))


(defmacro update-info-force-computation-p (object)
  `(logbitp 3 (update-info-bits ,object)))

(defsetf update-info-force-computation-p (object) (value)
  `(bit-setter ,object 3 ,value))


(defmacro update-info-on-fastdraw-list-p (object)
  `(logbitp 4 (update-info-bits ,object)))

(defsetf update-info-on-fastdraw-list-p (object) (value)
  `(bit-setter ,object 4 ,value))
	 

(defun update-info-print-function (struct stream depth)
  (declare (ignore depth))
  (format stream "#<Update-Info dirty-p ~A invalid-p ~A>"
	(update-info-dirty-p struct)
	(update-info-invalid-p struct)))

(defstruct (win-update-info (:print-function win-update-info-print-function))
        fix-update-slots-objects
	invalid-view-objects
	invalid-objects
	invalid-xor-fastdraws
	invalid-copy-fastdraws
	invalid-slots
	new-bbox
	clip-mask-1
	clip-mask-2
	old-aggregate
        width
        height
	exposed-bbox
)

;;; The invalid objects slot used to be unprintable because it had
;;; an extra item at the end, but that has been eliminated.
(defun win-update-info-print-function (struct stream depth)
  (declare (ignore depth))
  (format stream "#<Win-UI v ~A o ~A x ~A c ~A s ~A f ~A>"
     (win-update-info-invalid-view-objects struct)
     (win-update-info-invalid-objects struct)
     (win-update-info-invalid-xor-fastdraws struct)
     (win-update-info-invalid-copy-fastdraws struct)
     (win-update-info-invalid-slots struct)
     (win-update-info-fix-update-slots-objects struct)
     ))

(defvar *free-cons* NIL)

(defvar *font-hash-table* (make-hash-table 
			   :test #'equal
			   #+sb-thread :synchronized #+sb-thread t))



(defmacro merge-bbox (dest-bbox source-bbox)
  `(when (bbox-valid-p ,source-bbox)
     (if (bbox-valid-p ,dest-bbox)
      (progn
	(setf (bbox-x1 ,dest-bbox)
		(MIN (bbox-x1 ,dest-bbox) (bbox-x1 ,source-bbox)))
	(setf (bbox-y1 ,dest-bbox)
		(MIN (bbox-y1 ,dest-bbox) (bbox-y1 ,source-bbox)))
	(setf (bbox-x2 ,dest-bbox)
		(MAX (bbox-x2 ,dest-bbox) (bbox-x2 ,source-bbox)))
	(setf (bbox-y2 ,dest-bbox)
		(MAX (bbox-y2 ,dest-bbox) (bbox-y2 ,source-bbox))))
      (progn
	(setf (bbox-x1 ,dest-bbox) (bbox-x1 ,source-bbox))
	(setf (bbox-y1 ,dest-bbox) (bbox-y1 ,source-bbox))
	(setf (bbox-x2 ,dest-bbox) (bbox-x2 ,source-bbox))
	(setf (bbox-y2 ,dest-bbox) (bbox-y2 ,source-bbox))
	(setf (bbox-valid-p ,dest-bbox) T)))))

;;; Leaves the bboxes valid-p bits alone.  Only copies the dimensions.
(defmacro copy-bbox-dims (dest-bbox source-bbox)
  `(progn
	(setf (bbox-x1 ,dest-bbox) (bbox-x1 ,source-bbox))
	(setf (bbox-y1 ,dest-bbox) (bbox-y1 ,source-bbox))
	(setf (bbox-x2 ,dest-bbox) (bbox-x2 ,source-bbox))
	(setf (bbox-y2 ,dest-bbox) (bbox-y2 ,source-bbox))))

;;; Performs the same function as copy-bbox-dims *AND* copies the valid-p bit
(defun copy-bbox-fn (dest-bbox source-bbox)
  (copy-bbox-dims dest-bbox source-bbox)
  (setf (bbox-valid-p dest-bbox) (bbox-valid-p source-bbox)))

;; Returns T iff the dimensions of two bboxes are different. Ignores valid-p.
(defmacro bbox-dims-differ (bb1 bb2)
  `(not (and
	  (= (bbox-x1 ,bb1) (bbox-x1 ,bb2))
	  (= (bbox-y1 ,bb1) (bbox-y1 ,bb2))
	  (= (bbox-x2 ,bb1) (bbox-x2 ,bb2))
	  (= (bbox-y2 ,bb1) (bbox-y2 ,bb2)))))

;;; Updates the bbox given (probably the object's :old-bbox slot value) with
;;; the values from the object.  This *presumes* that the object is visible!
(defmacro update-bbox (object bbox)
    `(let ((left (g-value ,object :left))
	   (top  (g-value ,object :top )))
	(setf (bbox-x1 ,bbox) left)
	(setf (bbox-y1 ,bbox) top)
	(setf (bbox-x2 ,bbox) (+ left (g-value ,object :width )))
	(setf (bbox-y2 ,bbox) (+ top  (g-value ,object :height)))
	(setf (bbox-valid-p ,bbox) T)))

;;; Returns true if they intersect (ignores the valid bit!)
(defmacro bbox-intersect-p (bb1 bb2)
 `(and (<= (bbox-x1 ,bb1) (bbox-x2 ,bb2))   ;; 1 not right of 2
       (<= (bbox-x1 ,bb2) (bbox-x2 ,bb1))   ;; 2 not right of 1
       (<= (bbox-y1 ,bb1) (bbox-y2 ,bb2))   ;; 1 not below 2
       (<= (bbox-y1 ,bb2) (bbox-y2 ,bb1)))) ;; 2 not below 1

;;; Returns true iff bbox intersects either bb1 or bb2.  This will check if
;;; bb2 is NIL, but if bb1 is NIL this will crash.
(defmacro bbox-intersects-either-p (bbox bb1 bb2)
  `(or (bbox-intersect-p ,bbox ,bb1)
       (and ,bb2 (bbox-intersect-p ,bbox ,bb2))))

;;; Erases this bbox from this window (or its buffer). Ignores valid bit.
;;;
(defun erase-bbox (bb a-window buffer)
  (gem:clear-area a-window (bbox-x1 bb) (bbox-y1 bb)
		  (- (bbox-x2 bb) (bbox-x1 bb))
		  (- (bbox-y2 bb) (bbox-y1 bb))
		  buffer))



;; Takes a bbox and a clip mask, and goes through and sets the fields properly
;; within the clip mask.  Ignores valid bit.
(defmacro bbox-to-clip-mask (bb clip-mask)
  `(let ((cm ,clip-mask))
     (setf (car cm) (bbox-x1 ,bb))
     (setf (car (setq cm (cdr cm))) (bbox-y1 ,bb))
     (setf (car (setq cm (cdr cm))) (- (bbox-x2 ,bb) (bbox-x1 ,bb)))
     (setf (cadr cm) (- (bbox-y2 ,bb) (bbox-y1 ,bb)))))



;; propagate dirty bit of T from this object up towards root
;; this will do ugly things if called with object == NULL.
(defmacro propagate-dirty-bit (object update-info)
   `(unless (update-info-dirty-p ,update-info)
      (let ((temp ,object) (temp-update-info ,update-info))
        (loop
	  (setf (update-info-dirty-p temp-update-info) T)
	  (if (or (null (setq temp (g-local-value temp :parent)))
		  (update-info-dirty-p
		     (setq temp-update-info
			   (g-local-value temp :update-info))))
		(return))))))


(defmacro get-cons (the-car the-cdr)
 `(let ((cons-cell *free-cons*))
   (if cons-cell
     (progn
	(setf *free-cons* (cdr *free-cons*))
	(setf (car cons-cell) ,the-car)
	(setf (cdr cons-cell) ,the-cdr)
	cons-cell)
     (cons ,the-car ,the-cdr))))

(defmacro free-cons (cons-cell)
  `(progn
	(setf (cdr ,cons-cell) *free-cons*)
	(setf *free-cons* ,cons-cell)))

(defmacro free-list (the-list)
  `(when ,the-list
     (let ((last-cdr (last ,the-list)))
	(setf (cdr last-cdr) *free-cons*)
	(setf *free-cons* ,the-list))))

(defmacro normal-invalidate (gob win-info)
 `(setf (win-update-info-invalid-objects ,win-info)
	(get-cons ,gob (win-update-info-invalid-objects ,win-info))))

(defmacro xor-invalidate (gob win-info)
 `(setf (win-update-info-invalid-xor-fastdraws ,win-info)
	(get-cons ,gob (win-update-info-invalid-xor-fastdraws ,win-info))))

(defmacro copy-invalidate (gob win-info)
 `(setf (win-update-info-invalid-copy-fastdraws ,win-info)
	(get-cons ,gob (win-update-info-invalid-copy-fastdraws ,win-info))))

(defmacro aggregate-invalidate (gob win-info)
 `(setf (win-update-info-invalid-view-objects ,win-info)
	(get-cons ,gob (win-update-info-invalid-view-objects ,win-info))))

;; this adds the object to the window's invalid-objects entry in its
;; :win-update-info slot and then sets the object's invalid-p to T.
(defmacro make-object-invalid (gob gob-update-info the-window)
 `(let ((w-info (g-local-value ,the-window :win-update-info)))
    (if (update-info-aggregate-p ,gob-update-info)
	(aggregate-invalidate ,gob w-info)
	(case (g-value ,gob :fast-redraw-p)
		((NIL) (normal-invalidate ,gob w-info))
		((T)   (xor-invalidate    ,gob w-info))
		(T     (copy-invalidate   ,gob w-info))))
    (setf (update-info-invalid-p ,gob-update-info) T)))

(defun new-garnet-window-name ()
  (let ((*print-base* 10))
    (format nil "Opal ~S" (incf *opal-window-count*))))


;;; This is a list of init routines that are to be called whenever
;;; reconnect-garnet is called.  This will be used by the gestures handler
;;; and multifont.
(defparameter *auxilliary-reconnect-routines* ())

