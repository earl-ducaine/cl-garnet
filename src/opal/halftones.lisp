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
;;; Change Log:
;;;     date     who    what
;;;     ----     ---    ----
;;;    8-Sep-94  Amickish Added initialize-halftones to be called by X and Mac
;;;   25-Jan-94  Amickish Do not associate opal:white-fill with the halftone
;;;                       table, because it is now a solid fill.
;;;    9-Jan-94  Amickish Added Install-Bitmap-Images
;;;   17-Dec-93  Amickish Called gem:Device-Image in Build-Halftone-Table
;;;    6-Oct-92  koz      changed #'update-slot-invalidated to a g-value
;;;                       of opal:GRAPHICAL-OBJECT :invalidate-demon
;;;   10-Mar-92  Pervin   Rewrote find-halftone.
;;;    3-Jan-92  Mickish  Changed to call with-demons-disabled.
;;;    4-Mar-91  D'Souza  Removed nickname "MO" of package Opal.
;;;   21-Feb-91  Pervin   Changed bitmap images in the halftone-table so that
;;;                       each halftone strictly contains the one before it.
;;;    8-May-90  Sannella The R4 CLX version of xlib:bitmap-image needs
;;;                       arguments like #*1011 instead of '#(1 1 0 1)
;;;   19-Mar-90  Pervin   Changed tile to stipple
;;;   13-Feb-90  Pervin   Implemented color.
;;;   07-Jul-89  Kosbie   Placed these within "WITH-DEMONS-DISABLED"
;;;   15-Jun-89  Kosbie   Added s-values to *-FILL-BITMAPs :image slots after the
;;;			  function "halftone-image" was defined.

(in-package "OPAL")

(defun halftone-print (s stream ignore)
  (declare (ignore ignore))
  (format stream "#<Halftone ~D>" (halftone-percent s)))

(defun get-descriptor (index)
  (case index
    (0 '(#*0000 #*0000 #*0000 #*0000))
    (1 '(#*1000 #*0000 #*0000 #*0000))
    (2 '(#*1000 #*0000 #*0010 #*0000))
    (3 '(#*1010 #*0000 #*0010 #*0000))
    (4 '(#*1010 #*0000 #*1010 #*0000))
    (5 '(#*1010 #*0100 #*1010 #*0000))
    (6 '(#*1010 #*0100 #*1010 #*0001))
    (7 '(#*1010 #*0101 #*1010 #*0001))
    (8 '(#*1010 #*0101 #*1010 #*0101))
    (9 '(#*1010 #*0101 #*1010 #*0111))
    (10 '(#*1010 #*1101 #*1010 #*0111))
    (11 '(#*1010 #*1101 #*1010 #*1111))
    (12 '(#*1010 #*1111 #*1010 #*1111))
    (13 '(#*1010 #*1111 #*1011 #*1111))
    (14 '(#*1110 #*1111 #*1011 #*1111))
    (15 '(#*1110 #*1111 #*1111 #*1111))
    (16 '(#*1111 #*1111 #*1111 #*1111))))


;;; a bit inelegant, perhaps, but very clear
(defun build-halftone-table (root-window)
  (let ((halftone-table (make-array *halftone-table-size*)))
    (setf (aref halftone-table 0)
	  (make-halftone :percent 0
			 :device-image (gem:device-image root-window 0)))
    (setf (aref halftone-table 1)
	  (make-halftone :percent 6   
			 :device-image (gem:device-image root-window 1)))
    (setf (aref halftone-table 2)
	  (make-halftone :percent 12 
			 :device-image (gem:device-image root-window 2)))
    (setf (aref halftone-table 3)
	  (make-halftone :percent 18
			 :device-image (gem:device-image root-window 3)))
    (setf (aref halftone-table 4) 
	  (make-halftone :percent 25
			 :filling-style OPAL:LIGHT-GRAY-FILL
			 :device-image (gem:device-image root-window 4)))
    (setf (aref halftone-table 5) 
	  (make-halftone :percent 31
			 :device-image (gem:device-image root-window 5)))
    (setf (aref halftone-table 6) 
	  (make-halftone :percent 37
			 :device-image (gem:device-image root-window 6)))
    (setf (aref halftone-table 7) 
	  (make-halftone :percent  43
			 :device-image (gem:device-image root-window 7)))
    (setf (aref halftone-table 8) 
	  (make-halftone :percent 50
			 :filling-style OPAL:GRAY-FILL
			 :device-image (gem:device-image root-window 8)))
    (setf (aref halftone-table 9) 
	  (make-halftone :percent 56
			 :device-image (gem:device-image root-window 9)))
    (setf (aref halftone-table 10) 
	  (make-halftone :percent 62
			 :device-image (gem:device-image root-window 10)))
    (setf (aref halftone-table 11) 
	  (make-halftone :percent 68
			 :device-image (gem:device-image root-window 11)))
    (setf (aref halftone-table 12) 
	  (make-halftone :percent 75
			 :filling-style OPAL:DARK-GRAY-FILL
			 :device-image (gem:device-image root-window 12)))
    (setf (aref halftone-table 13) 
	  (make-halftone :percent 81
			 :device-image (gem:device-image root-window 13)))
    (setf (aref halftone-table 14) 
	  (make-halftone :percent 87
			 :device-image (gem:device-image root-window 14)))
    (setf (aref halftone-table 15) 
	  (make-halftone :percent 93
			 :device-image (gem:device-image root-window 15)))
    (setf (aref halftone-table 16) 
	  (make-halftone :percent 100
			 :device-image (gem:device-image root-window 16)))

    halftone-table))


;;; This function is called by Initialize-Display (in windows.lisp) when the
;;; first window appears, and every time a Garnet image is restarted on the
;;; Mac (see ccl:def-load-pointers in mac.lisp).
(defun Install-Bitmap-Images ()
  (with-demon-disabled (g-value opal:GRAPHICAL-OBJECT :invalidate-demon)
    (s-value opal::WHITE-FILL-BITMAP      :image (halftone-image  0))
    (s-value opal::LIGHT-GRAY-FILL-BITMAP :image (halftone-image 25))
    (s-value opal::GRAY-FILL-BITMAP       :image (halftone-image 50))
    (s-value opal::DARK-GRAY-FILL-BITMAP  :image (halftone-image 75))))


(defun initialize-halftones ()
  (unless *halftone-table*
    ;;; This used to be done by a DefVar, but now the DefVars all occur at
    ;; the start of loading Opal, before the function is defined, so we must
    ;; Setf it here...
    (setf *halftone-table* (build-halftone-table (g-value DEVICE-INFO
							  :current-root)))
    (install-bitmap-images)))


#|
;; quick and dirty 
(defun find-halftone (percent halftone-table)
  (if (< percent (halftone-percent (aref halftone-table 0)))
      0
      (do ((i 1 (1+ i))
	   (tone nil))
	  ((>= i *halftone-table-size*) (1- *halftone-table-size*))
	
	;; decide which one is closer to the value desired
	(if (> (halftone-percent (setf tone (aref halftone-table i)))
	       percent)
	    (if (<= (- (halftone-percent tone) percent)
		    (- percent
		       (halftone-percent (aref halftone-table (1- i)))))
		(return-from find-halftone i)
		(return-from find-halftone (1- i)))))))
|#

;; even quicker and dirtier
(defun find-halftone (percent)
  (round (* (max 0 (min 100 percent)) (1- *halftone-table-size*)) 100))


(defun halftone-image (percent)
  (let ((halftone (aref *halftone-table*
			(find-halftone percent))))
    (values
     (halftone-device-image halftone)
     (halftone-percent halftone))))

(defun halftone-image-darker (percent)
  (let ((halftone (aref *halftone-table*
			(min (1- *halftone-table-size*)
			     (1+ (find-halftone percent))))))
    (values
     (halftone-device-image halftone)
     (halftone-percent halftone))))

(defun halftone-image-lighter (percent)
  (let ((halftone (aref *halftone-table*
			(max 0 (1- (find-halftone percent))))))
    (values
     (halftone-device-image halftone)
     (halftone-percent halftone))))

;;; This takes a list-of-lists, representing the 1's and 0's of the mask of
;;; this filling-style, and creates a filling-style with a :stipple slot set to
;;; a bitmap which has a :image slot set to the image resulting from this
;;; mask.

(defun make-filling-style (fname-or-image-list &key
					       root-window
					       (from-file-p NIL)
					       (foreground-color opal:black)
					       (background-color opal:white))
  (unless root-window
    (setq root-window (g-value DEVICE-INFO :current-root)))
  (let ((result (create-instance NIL opal:filling-style
		  (:foreground-color foreground-color)
		  (:background-color background-color)
		  (:fill-style :opaque-stippled)))
	(stipple-entry (create-instance NIL opal:bitmap))
	(fixed-list (unless from-file-p
		      (mapcar #'(lambda(x) (coerce x 'simple-bit-vector))
			      fname-or-image-list)))
	image)
    (if from-file-p
      (if (probe-file fname-or-image-list)
	(setq image (read-image fname-or-image-list root-window))
	(format t "*** Warning: could not find bitmap file ~A~%"
		fname-or-image-list))
      (setq image (gem:image-from-bits root-window fixed-list)))
    (unless image
      (format t "*** Warning: making filling-style ~A with a NIL image!~%"
	      result))
    (s-value stipple-entry :image image)
    (s-value result :stipple stipple-entry)
    result))

