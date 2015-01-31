;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CHANGES:
;;;
;;; 03-Sep-98 Gilham  Fixed problem caused by changes moving between 2.2 and
;;;                   3.0.  1) Signature of update method changed for
;;;                   aggregadgets causing run-time error when aggregadgets
;;;                   were used as item-prototypes.  2) Window wasn't
;;;                   propagated to `parts' of aggregadgets once 1) was
;;;                   fixed causing KR error.  See :initialize method and
;;;                   :update methods in update.lisp.
;;; 17-Jun-93 Mickish Repaired to work with Opal changes of 6-Apr-93; removed
;;;                   clip-mask parameters
;;;  6-Apr-93 koz     Commented out entire body of file, because changing
;;;                   with-*-styles to set-*-styles and removing "clip-mask"
;;;                   as an argument to :draw made this code obsolete.
;;;                   Maybe someone will fix it?
;;;  3-Dec-92 Mickish Bound THE-SCHEMA in macros so SCHEMA is not evaluated
;;;                   twice.
;;;  3-Jul-92 Pervin  If the prototype of a v.a. does not have a :draw
;;;		      method, create one out of its :update method.
;;; 28-May-92 Pervin  Fixed three bugs:
;;;     1) update a v.a. created with :item-array = nil would crash
;;;	2) add-item to a v.a. which has never been visible would crash
;;;	3) add-item to an invisible v.a. would incorrectly compute bbox.
;;; 13-Apr-92 Pervin  Added :initial-element nil to make-array.
;;; 09-Apr-92 Pervin  Extensive rewrite so that I could remove the special
;;;		      code from the inner loop of window :update method.
;;;		      No more dirt-array, :added-bbox, :erased-bbox.
;;;		      New virtual-invalid-object type.
;;; 07-Apr-92 Mickish Added documentation string to change-item
;;; 31-Mar-92 Myers   Renamed initialize-virtual-aggregate-bboxes to be
;;;			      recalculate-virtual-aggregate-bboxes
;;; 27-Mar-92 Pervin  Removed all s-expressions out of merge-bbox macro.
;;; 02-Mar-92 Mickish Moved defmacro's to top of file
;;; 11-Dec-91 Pervin  Fixed do-in-clip-rect to only perform @body
;;;		      inside bounds of aggregate.
;;; 10-Dec-91 Pervin  Released into Opal
;;;

(in-package "OPAL")


(defvar work-clip-mask (make-list 4))
(defvar work-bbox (make-bbox :valid-p t))

(defmacro point-to-rank (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :point-to-rank the-schema ,@args)))

(create-instance 'opal:VIRTUAL-AGGREGATE opal:graphical-object
  :declare ((:type (fixnum :array-length-increment
			   :next-available-rank :gaps-in-arrays)))
  (:update-slots '(:visible :fast-redraw-p))
  (:ignored-slots '(:item-array :bbox-array))
  (:dummy-item)      ;; object that is actually drawn.
  (:invalid-object)  ;; invisible object to put in window's invalid object list
  (:item-prototype)
  (:item-array)
  (:array-length 0) ; This is actually the array dimensions and could be a list.
  (:array-length-increment 100)
  (:next-available-rank 0)
  (:gaps-in-arrays 0)
  (:bbox-array))

;;; "thing" is a virtual aggregate.
(defun set-things-size (thing thing-bbox)
  (s-value thing :left (bbox-x1 thing-bbox))
  (s-value thing :top (bbox-y1 thing-bbox))
  (s-value thing :width (- (bbox-x2 thing-bbox) (bbox-x1 thing-bbox)))
  (s-value thing :height (- (bbox-y2 thing-bbox) (bbox-y1 thing-bbox))))

;;; Merges an item
(defun initialize-item-bbox (thing thing-bbox bbox-array dummy item-array rank
								&optional rank2)
  (let (src-bbox)
    (if rank2
	(progn
	  (s-value dummy :rank1 rank)
	  (s-value dummy :rank2 rank2))
        (s-value dummy :rank rank))
    (s-value dummy :item-values 
	(if rank2 (aref item-array rank rank2) (aref item-array rank)))
    (if rank2
	(setf (aref bbox-array rank rank2) (make-bbox))
	(setf (aref bbox-array rank) (make-bbox)))
    (setq src-bbox
	  (if rank2 (aref bbox-array rank rank2) (aref bbox-array rank)))
    (update-bbox dummy src-bbox)
    (merge-bbox thing-bbox src-bbox)
    (set-things-size thing thing-bbox)))


(defun recalculate-virtual-aggregate-bboxes (thing)
  (let ((dummy (g-value thing :dummy-item))
	(item-array (g-value thing :item-array))
	(bbox-array (g-value thing :bbox-array))
	(array-length (g-value thing :array-length))
        (thing-bbox (update-info-old-bbox
			(the UPDATE-INFO (g-value thing :update-info)))))
    (if (numberp array-length)  ;; one-dimensional
      (dotimes (n array-length)
        (when (aref item-array n)
          (initialize-item-bbox thing thing-bbox bbox-array dummy item-array n)))
      (dotimes (m (first array-length))
	(dotimes (n (second array-length))
	  (when (aref item-array m n)
            (initialize-item-bbox thing thing-bbox bbox-array dummy item-array m n)))))
    (if (and thing-bbox (bbox-valid-p thing-bbox))
      (set-things-size thing thing-bbox)
      (progn
	(s-value thing :left 0)
	(s-value thing :top 0)
	(s-value thing :width 0)
	(s-value thing :height 0)))))


;;; The virtual-invalid-object is an invisible placeholder to be put
;;; on the window's invalid-objects list, to let the update method
;;; know what the bbox of the changed part of a virtual-aggregate is.
(create-instance 'virtual-invalid-object opal:view-object
  (:update-slots '(:visible :fast-redraw-p :make-update-think-i-have-changed)))


(define-method :initialize opal:virtual-aggregate (gob)
  (let ((dummy (create-instance nil (g-value gob :item-prototype)))
	array-length)
    ;; If dummy does not have :draw method, create one.
    (when (and (not (g-value dummy :draw))
	       (g-value dummy :update))
      ;; Fix for changes from 2.2 to 3.0. ---fmg
      (if (is-a-p dummy opal:aggregate)
	  (s-value dummy :draw
		   #'(lambda (dummy a-window)
		       (s-value dummy :window a-window)
		       (update dummy (g-value dummy :update-info)
			       nil nil
			       NIL NIL T)))
	(s-value dummy :draw
		 #'(lambda (dummy a-window)
		     (declare (ignore a-window))
		     (update dummy (g-value dummy :update-info)
			     NIL NIL T)))))
    (s-value gob :invalid-object
	     (create-instance nil opal::virtual-invalid-object
	       (:parent gob)
	       (:make-update-think-i-have-changed 0)))
    (s-value dummy :parent gob)
    (s-value dummy :update-slots-values
	     (make-array (length (g-value dummy :update-slots))
			 :initial-element nil))
    (s-value gob :dummy-item dummy)
    (unless (g-value gob :item-array)
      (s-value gob :item-array
	       (make-array 0 :adjustable t :initial-element nil)))
    (setq array-length (array-dimensions (g-value gob :item-array)))
    (if (cdr array-length)		; TWO-DIMENSIONAL
      (progn
	(s-value gob :add-item NIL)
	(s-value gob :remove-item NIL))
      (setq array-length (car array-length)))
    (s-value gob :array-length array-length)
    (s-value gob :bbox-array
	     (make-array array-length :element-type 'opal::bbox 
			 :adjustable t))
    (when (numberp array-length);; one dimensional
      (s-value gob :next-available-rank array-length))
    (call-prototype-method gob)
    (recalculate-virtual-aggregate-bboxes gob)
    (update-slots-values-changed gob 0 (g-local-value gob :update-info))))


;;; Gives rank of item at point <x,y>.
(define-method :point-to-rank opal:virtual-aggregate (gob x y)
  (let ((item-array (g-value gob :item-array))
        (point-in-item (g-value gob :point-in-item))
        item)
    (do ((rank (1- (g-value gob :next-available-rank)) (1- rank)))
        ((< rank 0) (return nil))
      (setq item (aref item-array rank))
      (when (and item (funcall point-in-item gob item x y))
        (return rank)))))



(define-method :point-to-component opal:virtual-aggregate (a-thing x y &key (type t))
  (when (or (eq type t)
            (opal::my-is-a-p (g-value a-thing :item-prototype) type))
    (let ((dummy (g-value a-thing :dummy-item))
          (rank (point-to-rank a-thing x y)))
      (when rank
        (s-value dummy :rank rank)
        (s-value dummy :item-values (aref (g-value a-thing :item-array) rank))
        dummy))))



(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length)) ; list
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* (first r*))
	  (second* (second r*)))
     (declare (fixnum max-x2* max-y2* first* second*))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (declare (fixnum x1* y1*))
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (+ first* (third r*) -1)
						 (+ second* (fourth r*) -1))
	 (declare (fixnum x2* y2*))
	 (setq x1* (if x1* (max 0 x1*) 0))
	 (setq y1* (if y1* (max 0 y1*) 0))
	 (setq x2* (if x2* (min x2* max-x2*) max-x2*))
	 (setq y2* (if y2* (min y2* max-y2*) max-y2*))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ ,m)))
	       ((> ,m x2*))
	     (declare (fixnum ,m))
	     (do ((,n y1* (1+ ,n)))
	         ((> ,n y2*))
	       (declare (fixnum ,n))
	       ,@body)))))))



(defun make-virtual-aggregate-invalid (agg)
  (let* ((v (g-value agg :invalid-object))
	 (win (g-value agg :window))
	 (ui (g-value v :update-info)))
    (when (and win (g-value agg :visible))
      (unless (g-value v :already-on-invalid-objects-list)
        (set-display-slots v win t)
        (make-object-invalid v ui win)
        (incf (g-value v :make-update-think-i-have-changed))
        (s-value v :already-on-invalid-objects-list t)))))


(define-method :update opal:virtual-aggregate (gob update-info bbox-1 bbox-2
						   &optional (total-p NIL))
  (let* ((dummy (g-value gob :dummy-item))
	 (dummy-slots-list (g-value dummy :update-slots))
	 (dummy-update-slots-values (g-local-value dummy :update-slots-values))
	 dummy-vals-indx
	 item-bbox
	 (invalid-object (g-value gob :invalid-object))
	 (dirty-p (update-info-dirty-p update-info))
	 (agg-bbox (update-info-old-bbox update-info))
	 (array-size (g-value gob :array-length)) ; May be a list.
	 (bbox-array (g-value gob :bbox-array))
	 (item-array (g-value gob :item-array))
	 (a-window (g-value gob :window))
	 ;;; *** Temporary:
	 (clip-mask (list (g-value gob :left)
			  (g-value gob :top)
			  (g-value gob :width)
			  (g-value gob :height))))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when
      (or dirty-p
	  total-p
	  (and (bbox-valid-p agg-bbox)
	       (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) (listp clip-mask)
		 (bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(bbox-to-clip-mask agg-bbox clip-mask))
      (if (numberp array-size)   ;; one dimensional
        (dotimes (n (g-value gob :next-available-rank))
	  (setq item-bbox (aref bbox-array n))
          (when (and (bbox-valid-p item-bbox)
		     (or (and bbox-1 (bbox-intersect-p bbox-1 item-bbox))
		         (and bbox-2 (bbox-intersect-p bbox-2 item-bbox))))
            (s-value dummy :rank n)
            (s-value dummy :item-values (aref item-array n))
            ;;; faster than (opal::update-slots-values-changed dummy 0 update-info)
	    (setq dummy-vals-indx -1)
	    (dolist (slot dummy-slots-list)
	      (incf dummy-vals-indx)
  	      (setf (aref dummy-update-slots-values dummy-vals-indx)
	        (g-value dummy slot)))
            (draw dummy a-window)))
        (progn		       ;; two dimensional
	  (setq dummy-slots-list (cddr dummy-slots-list))
	  (if (fifth clip-mask) (setq clip-mask (cddddr clip-mask)))
          (do-in-clip-rect (m n gob clip-mask)
	    (s-value dummy :rank1 m)
	    (s-value dummy :rank2 n)
	    (s-value dummy :item-values (aref item-array m n))
            ;;; faster than (opal::update-slots-values-changed dummy 2 update-info)
	    (setq dummy-vals-indx 1)
	    (dolist (slot dummy-slots-list)
	      (incf dummy-vals-indx)
	      (setf (aref dummy-update-slots-values dummy-vals-indx)
	        (g-value dummy slot)))
	    (draw dummy a-window)))))
    (setf (bbox-valid-p
	    (update-info-old-bbox
	      (the UPDATE-INFO
                (g-value invalid-object :update-info))))
          nil)
    (if dirty-p (setf (update-info-dirty-p update-info) NIL))))



;;; Only valid for one-dimensional virtual-aggregates.
(define-method :add-item opal:virtual-aggregate (gob new-item)
  (let ((item-array (g-value gob :item-array))
	(bbox-array (g-value gob :bbox-array))
	(array-size (g-value gob :array-length))
        (changed-bbox (update-info-old-bbox
			(the UPDATE-INFO
			  (g-value gob :invalid-object :update-info))))
	(gob-bbox     (update-info-old-bbox
			(the UPDATE-INFO
			  (g-value gob :update-info))))
	item-bbox
	(rank (g-value gob :next-available-rank)))
    (when (>= rank array-size)     ;; running out of room
      (if (zerop (g-value gob :gaps-in-arrays))
	(progn                     ;; expand arrays
          (setq array-size (+ array-size (g-value gob :array-length-increment)))
          (setf item-array (adjust-array item-array array-size :initial-element nil))
          (setf bbox-array (adjust-array bbox-array array-size))
          (s-value gob :array-length array-size))
	(progn                     ;; compress arrays
	  (setq rank 0)
	  (dotimes (n array-size)
	    (when (aref item-array n)
	      (setf (aref item-array rank) (aref item-array n))
	      (setf (aref bbox-array rank) (aref bbox-array n))
	      (incf rank)))
	  (s-value gob :gaps-in-arrays 0))))
    (setf (aref item-array rank) new-item)
    (when (and (plusp rank) (not (g-value gob :visible)))
      (setf (bbox-valid-p gob-bbox) T))
    (initialize-item-bbox gob gob-bbox
			      bbox-array
			      (g-value gob :dummy-item) item-array rank)
    (setq item-bbox (aref bbox-array rank))
    (s-value gob :next-available-rank (1+ rank))
    (merge-bbox changed-bbox item-bbox)
    (make-virtual-aggregate-invalid gob)
))

(define-method :change-item opal:virtual-aggregate (gob new-item rank &optional rank2)
  (let ((item-array (g-value gob :item-array))
        (bbox-array (g-value gob :bbox-array))
	(dummy-item (g-value gob :dummy-item))
        (changed-bbox (update-info-old-bbox
			(the UPDATE-INFO
			     (g-value gob :invalid-object :update-info))))
	item-bbox)
    (if rank2
	(progn
	  (setq item-bbox (aref bbox-array rank rank2))
          (merge-bbox changed-bbox item-bbox)
	  (setf (aref item-array rank rank2) new-item))
	(progn
	  (setq item-bbox (aref bbox-array rank))
          (merge-bbox changed-bbox item-bbox)
	  (setf (aref item-array rank) new-item)
	  (initialize-item-bbox gob (update-info-old-bbox
				      (the UPDATE-INFO (g-value gob :update-info)))
			      bbox-array dummy-item item-array rank)
	  (setq item-bbox (aref bbox-array rank))
          (merge-bbox changed-bbox item-bbox)))
    (make-virtual-aggregate-invalid gob)
))
	  


;;; Only valid for one-dimensional virtual-aggregates.
(define-method :remove-item opal:virtual-aggregate (gob rank)
  (let* ((item-array (g-value gob :item-array))
	 (bbox-array (g-value gob :bbox-array))
         (changed-bbox (update-info-old-bbox
			 (the UPDATE-INFO
			      (g-value gob :invalid-object :update-info))))
	 (item-bbox (aref bbox-array rank))
         (gob-bbox (update-info-old-bbox
		     (the UPDATE-INFO (g-value gob :update-info)))))
    (merge-bbox changed-bbox item-bbox)
    (setf (bbox-valid-p item-bbox) nil)
    (setf (aref item-array rank) nil)
    (when (eq (1+ rank) (g-value gob :next-available-rank))
      (do ((x rank (1- x)))
	  ((or (< x 0) (aref item-array x))
	   (s-value gob :next-available-rank (1+ x)))))
    (when (or (eq (bbox-x1 item-bbox) (bbox-x1 gob-bbox))
	      (eq (bbox-x2 item-bbox) (bbox-x2 gob-bbox))
	      (eq (bbox-y1 item-bbox) (bbox-y1 gob-bbox))
	      (eq (bbox-y2 item-bbox) (bbox-y2 gob-bbox)))
      (setf (bbox-valid-p gob-bbox) nil)
      (dotimes (n (g-value gob :next-available-rank))
        (when (setq item-bbox (aref bbox-array n))
          (merge-bbox gob-bbox item-bbox)))
      (set-things-size gob gob-bbox))
    (incf (g-value gob :gaps-in-arrays))
    (make-virtual-aggregate-invalid gob)
))

(define-method :destroy-me opal:virtual-aggregate (gob &optional (top-level-p t))
  (call-prototype-method gob top-level-p))

(define-method :do-items opal:virtual-aggregate (a-aggregate a-function 
							     &key (type t))
   (let ((item-array (g-value a-aggregate :item-array)))
     (loop for child across item-array
	   ;; there may be "holes" in the virtual aggregate's
	   ;; :item-array [2003/09/16:rpg]
	   unless (null child)
	     when (or (eq type t)
		      (is-a-p child type))
	       do (funcall a-function child))))
