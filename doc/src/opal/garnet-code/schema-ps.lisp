;;; -*- Mode: Lisp, Fill, Save; Package: PSGRAPH; Log: garnet-changes.log -*-
;;;
;;; ______________________________________________________________________
;;;
;;; This code was written as part of the Garnet (User Interface) project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or anything developed as part of the Garnet
;;; Project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;; ______________________________________________________________________
;;;
(in-package "PSGRAPH" :use '("LISP" "KR"))

(load "/../sphere/usr/ecp/work/opal/doc/psgraph")

(defvar edgegray 0) ; solid black lines
(defvar boxgray 0) ;black lines on boxes
(defvar fontsize 8) ;8 point for class names
(defvar second-fontsize 6) ;6 point for slotnames

(defparameter *interesting-slots*
  '(:left :top :width :height 				;view-object
    :draw-function :line-style :filling-style    	;graphical-object
    :components :overlapping     		 	;aggregate 
    :x1 :x2 :y1 :y2 					;line
    :point-list          				;multipoint
    :angle1 :angle2 :radius 				;arcs
    :image 						;bitmaps
    :font :string  					;text
    :length :diameter :open-p
    :name :font-name :font-path :family :face :size     ;fonts
    :line-thickness :cap-style :join-style :dash-pattern;line-style
    :fill-rule :fill-style :tile			;filling-style
))

(defvar *relations* :is-a-inv)

(defun graph (what file &optional (shrink t) (scribe-p nil))
    (with-open-file (*standard-output* file
				       :direction :output
				       :if-exists :supersede)
       (psgraph what 'child-fn 'info shrink scribe-p)))

(defun info (schema)
  (let ((strings nil))
    (cond (schema
	   (doslots (slot schema)
			(when (and (member slot *interesting-slots*)
				   (get-value schema slot))
			  (push (format nil "  ~A: ~S"
					slot 
					(let ((value (get-value schema slot)))
					  (if (numberp value)
					      (let ((part (/ value (/ pi 60))))
						(if (and (not (zerop part))
							 (= (ffloor part) part))
						    ;its a piece o pi
						    (list
						     (list '*
							   (/ (truncate part)
							      60) 'pi))
						    value))
					      value)))
				strings)))
	   (push (format nil "~A" schema) strings))
	  (t))
    strings))

(defun child-fn (schema)
 (when (schema-p schema)
  (if (listp *relations*)
      (let ((children nil))
	(dolist (relation *relations*)
	  (setf children (append children (get-local-values schema relation)))))
      (get-local-values schema *relations*))))

