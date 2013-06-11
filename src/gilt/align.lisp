;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file originally created by Gilt, then hacked to pieces

#|
============================================================
Change log:
     7/14/93 Andrew Mickish - Excepted :active-p in constant lists
     7/01/93 Andrew Mickish - Moved Align-Func here from gilt.lisp
     2/16/93 Brad Myers - use :parameters instead of :changeable-slots
     8/26/92 Andrew Mickish - Removed CR-ALIGN-FINC and SHOW-ALIGN-DIALOGBOX
               because they are indepentently defined in the align-props files
     2/20/91 Osamu Hashimoto - separated it from particular gadgets
     1/ 3/91 Osamu Hashimoto - created
============================================================
|#

;;; Functions needed from Gilt
;;; (load (common-lisp-user::garnet-pathnames "gilt-functions-loader" common-lisp-user::Garnet-Gilt-PathName))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GILT")

(defparameter *sorted-list* NIL)

(defun obj-sum (sum-key)
  (let ((n (length *sorted-list*))
	(sum 0))
    (when (< n 1) (return-from obj-sum 0))
    (dotimes (i n sum)
      (incf sum (g-value (nth i *sorted-list*) sum-key)))))

(defun space-sum (direction-key space-key)
    (- (+ (g-value (car (last *sorted-list*)) direction-key)
	  (g-value (car (last *sorted-list*)) space-key))
       (g-value (first *sorted-list*) direction-key)))


(defun obj-sort (sort-key)
  (setq *sorted-list* (copy-list (g-value *selection-obj* :value)))
  (let ((n       (length *sorted-list*))
	(ref-obj (car (last *sorted-list*))))
    (when (< n 2) (return-from obj-sort 0))
    (dotimes (i (1- n) *sorted-list*)
      (let ((minpos i)
	    buf)
	(do ((j (1+ i) (1+ j)))
	    ((> j (1- n)))
	  (when 
	      (< 
	       (g-value (nth j *sorted-list*) sort-key)
	       (g-value (nth minpos *sorted-list*) sort-key))
	    (setq minpos j)))
	(setf buf (nth i *sorted-list*))
	(setf (nth i *sorted-list*) (nth minpos *sorted-list*))
	(setf (nth minpos *sorted-list*) buf)))
    (dotimes (i n)
      (when (equal ref-obj (nth i *sorted-list*))
	(return-from obj-sort i)))))


(defun vertical-spacing (default-space)
  (let ((ref-order    (obj-sort :top)))
    (unless (< ref-order 0)
      (let* ((n            (length *sorted-list*))
	     (objs-height  (obj-sum :height))
	     (space-height (space-sum :top :height))
	     (interval     (if (< 0 (1- n)) (1- n) 1))
	     (space        default-space))
	(when (<= (+ objs-height interval) space-height)
	  (setq space (round (/ (- space-height objs-height) interval))))

	(when (> ref-order 0) 
	  ;; backward-spacing
	  (do ((i (1- ref-order) (1- i)))
	      ((< i 0))
	    (let* ((obj        (nth i *sorted-list*))
		   (prev-obj   (nth (1+ i) *sorted-list*))
		   (obj-bottom (- (g-value prev-obj :top) space)))
	      (if (g-value obj :line-p)
		  (progn ;; Lines
		    (let* ((points (g-value obj :points))
			   (y1 (second points))
			   (y2 (fourth points)))
		      (when (> y1 y2)
			(setf (second (g-value obj :points)) obj-bottom)
			(setf (fourth (g-value obj :points))
			      (- obj-bottom (g-value obj :height))))
		      (when (< y1 y2)
			(setf (fourth (g-value obj :points)) obj-bottom)
			(setf (second (g-value obj :points))
			      (- obj-bottom (g-value obj :height))))
		      (when (= y1 y2)
			(setf (second (g-value obj :points)) obj-bottom)
			(setf (fourth (g-value obj :points)) obj-bottom))
		      (mark-as-changed obj :points)))
		  (progn ;; Objects except Lines
		    (setf (second (g-value obj :box))
			  (- obj-bottom (g-value obj :height)))
		    (mark-as-changed obj :box))))))

	(when (< ref-order (1- n))
	  ;; forward-spacing
	  (do ((i (1+ ref-order) (1+ i)))
	      ((> i (1- n)))
	    (let* ((obj       (nth i *sorted-list*))
		   (prev-obj  (nth (1- i) *sorted-list*))
		   (obj-top   (+ (g-value prev-obj :top)
				 (g-value prev-obj :height) space)))
	      (if (g-value obj :line-p)
		  (progn ;; Lines
		    (let* ((points (g-value obj :points))
			   (y1 (second points))
			   (y2 (fourth points)))
		      (when (> y1 y2)
			(setf (second (g-value obj :points))
			      (+ obj-top (g-value obj :height)))
			(setf (fourth (g-value obj :points)) obj-top))
		      (when (> y1 y2)
			(setf (fourth (g-value obj :points))
			      (+ obj-top (g-value obj :height)))
			(setf (second (g-value obj :points)) obj-top))
		      (when (= y1 y2)
			(setf (second (g-value obj :points)) obj-top)
			(setf (fourth (g-value obj :points)) obj-top))
		      (mark-as-changed obj :points)))
		  (progn ;; Objects except Lines
		    (setf (second (g-value obj :box)) obj-top)
		    (mark-as-changed obj :box))))))
))))


(defun horizontal-spacing (default-space)
  (let ((ref-order (obj-sort :left)))
    (unless (< ref-order 0)
      (let* ((n            (length *sorted-list*))
	     (objs-width   (obj-sum :width))
	     (space-width  (space-sum :left :width))
	     (interval     (if (< 0 (1- n)) (1- n) 1))
	     (space        default-space))
	(when (<= (+ objs-width interval) space-width)
	  (setq space (round (/ (- space-width objs-width) interval))))

	(when (> ref-order 0)
	  ;; backward-spacing
	  (do ((i (1- ref-order) (1- i)))
	      ((< i 0))
	    (let* ((obj        (nth i *sorted-list*))
		   (prev-obj   (nth (1+ i) *sorted-list*))
		   (obj-right  (- (g-value prev-obj :left) space)))
	      (if (g-value obj :line-p)
		  (progn ;; Lines
		    (let* ((points (g-value obj :points))
			   (x1 (first points))
			   (x2 (third points)))
		      (when (> x1 x2)
			(setf (first (g-value obj :points)) obj-right)
			(setf (third (g-value obj :points))
			      (- obj-right (g-value obj :width))))
		      (when (< x1 x2)
			(setf (third (g-value obj :points)) obj-right)
			(setf (first (g-value obj :points))
			      (- obj-right (g-value obj :width))))
		      (when (= x1 x2)
			(setf (first (g-value obj :points)) obj-right)
			(setf (third (g-value obj :points)) obj-right))
		      (mark-as-changed obj :points)))
		  (progn ;; Objects except Lines
		    (setf (first (g-value obj :box))
			  (- obj-right (g-value obj :width)))
		    (mark-as-changed obj :box))))))

	(when (< ref-order (1- n))
	  ;; forward-spacing
	  (do ((i (1+ ref-order) (1+ i)))
	      ((> i (1- n)))
	    (let* ((obj       (nth i *sorted-list*))
		   (prev-obj  (nth (1- i) *sorted-list*))
		   (obj-left  (+ (g-value prev-obj :left)
				 (g-value prev-obj :width) space)))
	      (if (g-value obj :line-p)
		  (progn ;; Lines
		    (let* ((points (g-value obj :points))
			   (x1 (first points))
			   (x2 (third points)))
		      (when (> x1 x2)
			(setf (first (g-value obj :points))
			      (+ obj-left (g-value obj :width)))
			(setf (third (g-value obj :points)) obj-left))
		      (when (< x1 x2)
			(setf (third (g-value obj :points))
			      (+ obj-left (g-value obj :width)))
			(setf (first (g-value obj :points)) obj-left))
		      (when (= x1 x2)
			(setf (first (g-value obj :points)) obj-left)
			(setf (third (g-value obj :points)) obj-left))
		      (mark-as-changed obj :points)))
		  (progn ;; Objects except Lines
		    (setf (first (g-value obj :box)) obj-left)
		    (mark-as-changed obj :box))))))
))))



(defun warning-for (direction leng objs)
  (let* ((warning* (concatenate 'string "WARNING: Unable to set the " direction))
	 (warning  (concatenate 'string warning* " of
"))
	 (str NIL))
    (cond ((= leng 1)
	   (setq str (name-for-schema (car objs))))
	  ((= leng 2)
	   (setq str (concatenate 'string
				  (name-for-schema (car objs))
				  " and "
				  (name-for-schema (cadr objs)))))
	  ((>= leng 3)
	   (do ((i 1 (1+ i)))
	       ((> i (- leng 1)))
	     (setq str (concatenate 'string str
				    (name-for-schema (nth (1- i) objs))
				    ", "))
	     (when (= 0 (rem i 3))
	       (setq str (concatenate 'string str "
"))))
	   (setq str (concatenate 'string str "and "
				  (name-for-schema (car (last objs)))))))
    (concatenate 'string warning str)))


(declaim (special align-prop))

(defun align-dialog-ok (top-gadget value)
  (declare (ignore top-gadget value))
  (let* ((size-style (g-value align-prop :same-size-style :value))
	 (height-p   (member "Same Height" size-style :test 'string=))
	 (width-p    (member "Same Width"  size-style :test 'string=))
	 (both-p     (and height-p width-p))
	 (c-r        (g-value align-prop :c-r-align :value))
	 (col-p      (string= "Column" (car c-r)))
	 (row-p      (string= "Row" (car c-r)))
	 (ref-obj    (car (last (g-value *selection-obj* :value)))))

;;; Same Size
  (when (or height-p width-p)
   (let ((fixed-height-objs NIL)
	 (fixed-width-objs  NIL))
     (dolist (obj (g-value *selection-obj* :value))
       (if (g-value obj :line-p)
	    (unless both-p
	      (let* ((points (g-value obj :points))
		     (x1 (first points))
		     (x2 (third points))
		     (y1 (second points))
		     (y2 (fourth points)))
		(when width-p
		  (if (>= x1 x2)
		      (setf (first (g-value obj :points))
			    (1- (+ (g-value ref-obj :width)
				   (g-value obj :left))))
		      (setf (third (g-value obj :points))
			    (1- (+ (g-value ref-obj :width)
				   (g-value obj :left)))))
		  (setf (second (g-value obj :points)) (g-value obj :top))
		  (setf (fourth (g-value obj :points)) (g-value obj :top))
		  (mark-as-changed obj :points))
		(when height-p
		  (if (>= y1 y2)
		      (setf (second (g-value obj :points))
			    (1- (+ (g-value ref-obj :height)
				   (g-value obj :top))))
		      (setf (fourth (g-value obj :points))
			    (1- (+ (g-value ref-obj :height)
				   (g-value obj :top)))))
		  (setf (first (g-value obj :points)) (g-value obj :left))
		  (setf (third (g-value obj :points)) (g-value obj :left))
		  (mark-as-changed obj :points))
		    ))
	  ;; Objects except Lines
	    (let ((params (g-value obj :parameters)))
	      (when height-p
		(if (member :height params)
		    (progn
		      (setf (fourth (g-value obj :box)) (g-value ref-obj :height))
		      (mark-as-changed obj :box))
		    (push obj fixed-height-objs)))
	      (when width-p
		(if (member :width params)
		    (progn
		      (setf (third (g-value obj :box)) (g-value ref-obj :width))
		      (mark-as-changed obj :box))
		    (push obj fixed-width-objs))))))

     (let* ((list (g-value *selection-obj* :value))
	    (ref-obj (car (last list)))
	    (f-height-objs (remove ref-obj fixed-height-objs))
	    (h-length      (length f-height-objs))
	    (f-width-objs  (remove ref-obj fixed-width-objs))
	    (w-length      (length f-width-objs))
	    (str NIL))
       (when (and width-p f-width-objs)
	 (setq str (warning-for "width" w-length f-width-objs)))
       (when (and height-p f-height-objs)
	 (when str (setq str (concatenate 'string str "

")))
	 (setq str (concatenate 'string str
				(warning-for "height" h-length f-height-objs))))



       (when both-p
	 (let ((line-objs NIL)
	       (line-names NIL))
	   (dolist (obj (g-value *selection-obj* :value))
	     (when (g-value obj :line-p)
	       (push obj line-objs)))
	   (let ((l (length line-objs)))
	     (unless (<= l 0)
	       (cond ((= l 1)
		      (setq line-names (name-for-schema (car line-objs))))
		     ((= l 2)
		      (setq str (concatenate 'string
					  (name-for-schema (car line-objs))
					  " and "
					  (name-for-schema (cadr line-objs)))))
		     ((>= l 3)
		      (do ((i 1 (1+ i)))
			  ((> i (- l 1)))
			(setq line-names (concatenate 'string line-names
				         (name-for-schema (nth (1- i) line-objs))
					 ", "))
			(when (= 0 (rem i 3))
			  (setq line-names (concatenate 'string line-names "
"))))
		      (setq line-names (concatenate 'string line-names "and "
				      (name-for-schema (car (last line-objs)))))))
	       (when str (setq str (concatenate 'string str "

")))
	       (setq str (concatenate 'string str 
			 "WARNING: Unable to change both width AND height of Lines:
" line-names))
))))


       (when str
	 (gilt-error str)))
     
))


;;; Same Col/Row
  (when col-p
    (let* ((col-style
	    (g-value (g-value align-prop :column :column-align-style) :value))
	   (ref-left   (if (string= "Left" col-style)
			   (g-value ref-obj :left)
			   NIL))
	   (ref-center (if (string= "Centered" col-style)
			   (+ (g-value ref-obj :left)
			      (round (* 0.5 (g-value ref-obj :width))))
			   NIL))
	   (ref-right  (if (string= "Right" col-style)
			   (+ (g-value ref-obj :left)
			      (g-value ref-obj :width))
			   NIL)))
      (dolist (obj (g-value *selection-obj* :value))
	(let ((obj-width (g-value obj :width)))
	  (if (g-value obj :line-p)
	      (progn ;; Lines
		(let* ((points (g-value obj :points))
		       (x1 (first points))
		       (x2 (third points)))
		  (if (>= x1 x2)
		      (progn
			(when ref-left
			  (setf (third (g-value obj :points)) ref-left))
			(when ref-center
			  (setf (third (g-value obj :points))
				(- ref-center (round (* 0.5 obj-width)))))
			(when ref-right
			  (setf (third (g-value obj :points))
				(- ref-right obj-width)))
			(setf (first (g-value obj :points))
			      (1- (+ (third points) (g-value obj :width)))))
		      (progn
			(when ref-left
			  (setf (first (g-value obj :points)) ref-left))
			(when ref-center
			  (setf (first (g-value obj :points))
				(- ref-center (round (* 0.5 obj-width)))))
			(when ref-right
			  (setf (first (g-value obj :points))
				(- ref-right obj-width)))
			(setf (third (g-value obj :points))
			      (+ (first points) (g-value obj :width)))))
		  (mark-as-changed obj :points)))
	      (progn ;; Objects except Lines
		(when ref-left
		  (setf (first (g-value obj :box)) ref-left))
		(when ref-center
		  (setf (first (g-value obj :box))
			(- ref-center (round (* 0.5 obj-width)))))
		(when ref-right
		  (setf (first (g-value obj :box)) (- ref-right obj-width)))
		(mark-as-changed obj :box)))))

      (vertical-spacing 5)
))


  (when row-p
    (let* ((row-style
	    (g-value (g-value align-prop :row :row-align-style) :value))
	   (ref-top    (if (string= "Top" row-style)
			   (g-value ref-obj :top)
			   NIL))
	   (ref-center (if (string= "Centered" row-style)
			   (+ (g-value ref-obj :top)
			      (round (* 0.5 (g-value ref-obj :height))))
			   NIL))
	   (ref-bottom  (if (string= "Bottom" row-style)
			   (+ (g-value ref-obj :top)
			      (g-value ref-obj :height))
			   NIL)))
      (dolist (obj (g-value *selection-obj* :value))
	(let ((obj-height (g-value obj :height)))
	  (if (g-value obj :line-p)
	      (progn ;; Lines
		(let* ((points (g-value obj :points))
		       (y1 (second points))
		       (y2 (fourth points)))
		  (if (>= y1 y2)
		      (progn
			(when ref-top
			  (setf (fourth (g-value obj :points)) ref-top))
			(when ref-center
			  (setf (fourth (g-value obj :points))
				(- ref-center (round (* 0.5 obj-height)))))
			(when ref-bottom
			  (setf (fourth (g-value obj :points))
				(- ref-bottom obj-height)))
			(setf (second (g-value obj :points))
			      (1- (+ (fourth points) (g-value obj :height)))))
		      (progn
			(when ref-top
			  (setf (second (g-value obj :points)) ref-top))
			(when ref-center
			  (setf (second (g-value obj :points))
				(- ref-center (round (* 0.5 obj-height)))))
			(when ref-bottom
			  (setf (second (g-value obj :points))
				(- ref-bottom obj-height)))
			(setf (fourth (g-value obj :points))
			      (+ (second points) (g-value obj :height)))))
		  (mark-as-changed obj :points)))
	      (progn ;; Objects except Lines
		(when ref-top
		  (setf (second (g-value obj :box)) ref-top))
		(when ref-center
		  (setf (second (g-value obj :box))
			(- ref-center (round (* 0.5 obj-height)))))
		(when ref-bottom
		  (setf (second (g-value obj :box)) (- ref-bottom obj-height)))
		(mark-as-changed obj :box)))))

      (horizontal-spacing 5)
))
))

(defun Align-Func (&rest args)
  (declare (ignore args))
  (Show-Align-DialogBox))


(defun cr-align-func (gadget value)
  (declare (ignore gadget value))
  (let* ((cr         (g-value align-prop :c-r-align))
	 (new-value  (car (g-value cr :value)))
	 (col-style  (g-value (g-value align-prop :column) :column-align-style))
	 (row-style  (g-value (g-value align-prop :row)    :row-align-style)))
    (when (string= "Column" new-value)
      (s-value cr :value '("Column"))
      (s-value col-style :active-p T)
      (s-value row-style :active-p NIL))
    (when (string= "Row" new-value)
      (s-value cr :value '("Row"))
      (s-value col-style :active-p NIL)
      (s-value row-style :active-p T))
    (when (null new-value)
      (s-value col-style :active-p NIL)
      (s-value row-style :active-p NIL))
))



(defun Show-Align-DialogBox ()
  (let ((selected-objs (length (g-value *selection-obj* :value))))
    (when (<= selected-objs 0)
      (Gilt-Error "Nothing Selected")
      (return-from Show-Align-DialogBox NIL))
    (when (<= selected-objs 1)
      (Gilt-Error "Must have more than one item selected for Align")
      (return-from Show-Align-DialogBox NIL)))
    
  (let ((cr         (g-value align-prop :C-R-ALIGN))
	(size       (g-value align-prop :same-size-style))
	(col-style  (g-value (g-value align-prop :column) :column-align-style))
	(row-style  (g-value (g-value align-prop :row)    :row-align-style)))

    (g-value cr :value) (s-value cr :value NIL)
    (g-value col-style :value) (s-value col-style :value "Left")
    (g-value row-style :value) (s-value row-style :value "Top")
    (s-value col-style :active-p NIL)
    (s-value row-style :active-p NIL)

    (g-value size :value) (s-value size :value NIL)
  (let ((width-p NIL)
	(height-p NIL)
	(inavailable NIL))
    (dolist (obj (g-value *selection-obj* :value))
      (let ((params (g-value obj :parameters)))
	(when (member :width params) (setq width-p T))
	(when (member :height params) (setq height-p T))))
    (unless height-p (push "Same Height" inavailable))
    (unless width-p  (push "Same Width"  inavailable))
    (if (and height-p width-p)
	(s-value size :active-p T)
	(s-value size :inactive-items inavailable)))

    (let ((lowest 0)
	  (lowest-obj NIL))
      (dolist (obj (g-value *selection-obj* :value))
	(let ((bottom (+ (g-value obj :top) (g-value obj :height))))
	  (when (<= lowest bottom)
	    (setq lowest bottom) (setq lowest-obj obj))))
      (let ((left (g-value lowest-obj :left))
	    (top  (g-value lowest-obj :top)))
	(multiple-value-setq (left top)
	  (opal:convert-coordinates (g-value lowest-obj :window)
				    (g-value lowest-obj :left)
				    (opal:bottom lowest-obj) NIL))
	(setq top (+ 40 top))
	(show-in-window align-prop left top)))

))







(create-instance 'Column-align OPAL:AGGREGADGET
  (:parts `(
    (:COLUMN-ALIGN-STYLE ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:CONSTANT (T :except :active-p))
      (:SELECTION-FUNCTION NIL)
      (:GILT-REF "TYPE-MOTIF-RADIO-BUTTON-PANEL")
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:H-SPACING 5)
      (:DIRECTION :VERTICAL)
      (:SELECT-FUNCTION NIL)
      (:H-ALIGN :RIGHT)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 15)
      (:FIXED-HEIGHT-P T)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-DIAMETER 23)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:FIXED-WIDTH-P T)
      (:BOX (110 18 74 88 ))
      (:ITEMS ("Left" "Centered" "Right" ))
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 205))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 18)))
;;; column-align's icons
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 23 230 23 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 294))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 53))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 316))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 53)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 28 219 28 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 293))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 59))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 309))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 59)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 33 217 33 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 294))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 64))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 307))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 64)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 38 222 38 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 294))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 68))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 312))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 68)))
;;;
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 55 230 55 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 295))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 23))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 321))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 23)))
    (NIL ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:POINTS (211 60 225 60 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 301))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 27))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 315))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 27)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (214 65 222 65 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 304))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 32))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 312))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 32)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (206 70 229 70 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 294))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 36))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 324))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 36)))
;;;
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 87 230 87 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 293))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 83))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 320))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 83)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (210 92 230 92 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 300))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 89))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 318))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 89)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (213 97 230 97 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 303))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 93))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 315))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 93)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (207 103 230 103 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 294))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 96))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 317))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 96)))
)))

(create-instance 'Row-align OPAL:AGGREGADGET
  (:parts `(
    (:ROW-ALIGN-STYLE ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:CONSTANT (T :except :active-p))
      (:SELECTION-FUNCTION NIL)
      (:GILT-REF "TYPE-MOTIF-RADIO-BUTTON-PANEL")
      (:ITEMS ("Top" "Centered" "Bottom" ))
      (:BOX (110 122 88 88 ))
      (:FIXED-WIDTH-P T)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:BUTTON-DIAMETER 23)
      (:SHADOW-OFFSET 5)
      (:TEXT-OFFSET 5)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:FIXED-HEIGHT-P T)
      (:V-SPACING 15)
      (:TEXT-ON-LEFT-P T)
      (:GRAY-WIDTH 3)
      (:H-ALIGN :RIGHT)
      (:SELECT-FUNCTION NIL)
      (:DIRECTION :VERTICAL)
      (:H-SPACING 5)
      (:V-ALIGN :TOP)
      (:INDENT 0)
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 194))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 118)))
;;; row-align's icons
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 125 205 142 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 295))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 154))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 295))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 175)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (213 125 213 135 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 300))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 153))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 300))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 167)))
    (NIL ,OPAL:LINE
      (:GILT-REF "TYPE-LINE")
      (:CONSTANT (T))
      (:POINTS (221 125 221 137 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 305))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 153))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 305))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 169)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (229 125 229 141 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 312))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 153))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 312))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 173)))
;;;
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 157 205 174 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 296))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 122))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 296))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 142)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (213 162 213 169 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 301))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 126))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 301))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 138)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (221 160 221 171 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 307))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 128))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 307))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 137)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (229 157 229 174 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 311))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 121))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 311))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 142)))
;;;
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (205 189 205 206 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 296))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 186))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 296))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 207)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (213 192 213 206 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 302))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 192))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 302))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 205)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (221 194 221 206 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 306))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 194))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 306))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 205)))
    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (229 187 229 206 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 311))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 187))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 311))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 206)))
)))


(create-instance 'ALIGN-PROP OPAL:AGGREGADGET
  (:WINDOW-TITLE "Align Objects")
  (:WINDOW-LEFT 600)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 250)
  (:WINDOW-HEIGHT 330)
  (:window-background-color opal:motif-gray)
  (:PACKAGE-NAME "GILT")
  (:FUNCTION-FOR-OK `GILT::align-dialog-ok)
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (KR:GVL :WINDOW :WIDTH) 450))
  (:HEIGHT (o-formula (KR:GVL :WINDOW :HEIGHT) 300))
  (:parts `(
    (:C-R-ALIGN ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:CONSTANT (T :except :active))
      (:SELECTION-FUNCTION cr-align-func)
      (:GILT-REF "TYPE-MOTIF-CHECK-BUTTON-PANEL")
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:H-SPACING 5)
      (:DIRECTION :VERTICAL)
      (:SELECT-FUNCTION NIL)
      (:H-ALIGN :RIGHT)
      (:GRAY-WIDTH 3)
      (:TEXT-ON-LEFT-P T)
      (:V-SPACING 80)
      (:BUTTON-HEIGHT 20)
      (:FIXED-HEIGHT-P NIL)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:BUTTON-WIDTH 20)
      (:button-diameter 20)
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:SIZE :medium)
            (:FACE :BOLD)))
      (:FIXED-WIDTH-P T)
      (:BOX (15 20 71 120 ))
      (:ITEMS ("Column" "Row" ))
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 93))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 23)))

    (:column ,column-align)
    (:row ,row-align)

    (NIL ,OPAL:LINE
      (:CONSTANT (T))
      (:GILT-REF "TYPE-LINE")
      (:POINTS (10 220 150 220 ))
      (:LINE-P T)
      (:X1 ,(o-formula (FIRST (KR:GVL :POINTS)) 295))
      (:Y1 ,(o-formula (SECOND (KR:GVL :POINTS)) 23))
      (:X2 ,(o-formula (THIRD (KR:GVL :POINTS)) 321))
      (:Y2 ,(o-formula (FOURTH (KR:GVL :POINTS)) 23)))

    (:same-size-style ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:CONSTANT (T :except :active-p :inactive-items))
      (:SELECTION-FUNCTION NIL)
      (:GILT-REF "TYPE-MOTIF-CHECK-BUTTON-PANEL")
      (:ITEMS ( "Same Width" "Same Height"))
      (:BOX (15 225 95 60 ))
      (:FIXED-WIDTH-P T)
      (:FONT ,(create-instance nil OPAL:FONT
	    (:CONSTANT '(T))
            (:SIZE :medium)
            (:FACE :BOLD)))
      (:BUTTON-WIDTH 20)
      (:SHADOW-OFFSET 5)
      (:TEXT-OFFSET 5)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:FIXED-HEIGHT-P NIL)
      (:BUTTON-HEIGHT 20)
      (:V-SPACING 10)
      (:TEXT-ON-LEFT-P T)
      (:GRAY-WIDTH 3)
      (:H-ALIGN :RIGHT)
      (:SELECT-FUNCTION NIL)
      (:DIRECTION :VERTICAL)
      (:H-SPACING 5)
      (:V-ALIGN :TOP)
      (:INDENT 0)
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 70))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 218)))

    (:OK-CANCEL ,GARNET-GADGETS:MOTIF-Text-button-panel
      (:CONSTANT (T))
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-MOTIF-OKCANCEL")
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:V-SPACING 5)
      (:H-ALIGN :CENTER)
      (:FIXED-HEIGHT-P T)
      (:H-SPACING 5)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:FIXED-WIDTH-P T)
      (:SELECT-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Apply" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 5)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :VERTICAL)
      (:BOX (175 225 117 29 ))
      (:LEFT ,(o-formula (FIRST (KR:GVL :BOX)) 199))
      (:TOP ,(o-formula (SECOND (KR:GVL :BOX)) 244)))

#|
    (:column-grayout ,opal:rectangle
      (:draw-function :and)
      (:filling-style ,opal:dark-gray-fill)
      (:left 105)(:top 18)
      (:width 130)(:height 95)
      (:visible T))
    (:row-grayout ,opal:rectangle
      (:draw-function :and)
      (:filling-style ,opal:dark-gray-fill)
      (:left 105)(:top 122)
      (:width 130)(:height 95)
      (:visible T))
    (:height-grayout ,opal:rectangle
      (:draw-function :and)
      (:filling-style ,opal:dark-gray-fill)
      (:left 10)(:top 225)
      (:width 130)(:height 30)
      (:visible T))
    (:width-grayout ,opal:rectangle
      (:draw-function :and)
      (:filling-style ,opal:dark-gray-fill)
      (:left 10)(:top 255)
      (:width 130)(:height 30)
      (:visible T))
|#

)))

