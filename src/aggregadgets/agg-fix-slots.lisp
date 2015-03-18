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
;;;  The :fix-update-slots method for opal:aggrelist and supporting functions
;;; 
;;;  Written by Andrew Mickish and Dave Kosbie
;;;
;;; $Id::                                                             $



(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Set-Rank-Slots)))

(defun same-type-p (item1 item2)
  ;; Hack for item/function pairs in gadgets
  (if (and (consp item1) (schema-p (car item1))) (setf item1 (car item1)))
  (if (and (consp item2) (schema-p (car item2))) (setf item2 (car item2)))
  ;; Never reuse schema items
  (if (or (schema-p item1) (schema-p item2))
      NIL
      ;; Optimize for strings
      (or (and (stringp item1) (stringp item2))
	  (typep item1 (type-of item2)))))

;; Check that first several components are of the right type
(defun Fix-Item-Types (agg new-items old-items end)
  (do* ((n 0 (1+ n))
	(sub-new-items new-items (cdr sub-new-items))
	(sub-old-items old-items (cdr sub-old-items))
	(new-item (car sub-new-items) (car sub-new-items))
	(old-item (car sub-old-items) (car sub-old-items)))
       ((eql n end) NIL)
    (unless (same-type-p new-item old-item)
      (opal::Recursive-Remove-Component agg n old-item)
      (opal::Recursive-Add-Component agg n))))


(defun Fix-Items (agg)
  (let* ((old-items (g-value agg :old-items))
	 (new-items (g-value agg :items))
	 (old-number-p (numberp old-items))
	 (new-number-p (numberp new-items)))
    (unless (equal old-items new-items)
      (if new-number-p
	  (if old-number-p
	      (if (> old-items new-items)
		  (dotimes (n (- old-items new-items))
		    ;; Remove components at the end
		    (opal::Remove-The-Component agg new-items))
		  (dotimes (n (- new-items old-items))
		    ;; Always add to the end
		    (opal::Add-The-Component agg (+ n old-items))))
	      ;; Then the :items value is changing from a list to a number
	      (progn
		(dotimes (n (length old-items))
		  (opal::Remove-The-Component agg 0 (nth n old-items)))
		(dotimes (n new-items)
		  (opal::Add-The-Component agg new-items))))
	  (if old-number-p
	      ;; Then the :items value is changing from a number to a list
	      (let ((new-items-length (length new-items)))
		(dotimes (n old-items)
		  (opal::Remove-The-Component agg 0))
		(dotimes (n new-items-length)
		  (opal::Add-The-Component agg new-items-length)))
	      (let ((old-items-length (length old-items))
		    (new-items-length (length new-items)))
		(cond
		  ((> old-items-length new-items-length)
		   (Fix-Item-Types agg new-items old-items new-items-length)
		   ;; Remove and destroy all superfluous components
		   (dotimes (n (- old-items-length new-items-length))
		     (opal::Remove-The-Component
		      agg new-items-length (nth (+ new-items-length n)
						old-items))))

		  ((< old-items-length new-items-length)
		   (Fix-Item-Types agg new-items old-items old-items-length)
		   ;; Add necessary new components
		   (dotimes (n (- new-items-length old-items-length))
		     (opal::Add-The-Component
		      agg (+ n old-items-length))))
		  (t
		   ;; Check that all components are of the right type
		   (Fix-Item-Types agg new-items old-items old-items-length))))
		  ))
      (s-value agg :old-items new-items)
      )))

(defun Set-Rank-Slots (the-lister)
  (let ((rank -1))
    (dolist (comp (g-local-value the-lister :components))
      (unless (eq (incf rank) (get-local-value comp :rank))
	(s-value comp :rank rank)))))


(define-method :fix-update-slots opal:aggrelist (the-lister)
  (let ((us-vals (g-local-value the-lister :update-slots-values))
	(us-vals-index -1)
	us-vals-changed? temp)

    ;; Deal with changing components first
    (unless (equal (g-value the-lister :items)
		   (aref us-vals +lister-items+))
      (Fix-Items the-lister))
    
    ;; Must set :rank slots before determining the dimensions of the aggrelist.
    ;; The :add-item and :remove-item methods do not maintain the :rank slots
    ;; of the remaining components, so we must do it here.  Problems would
    ;; show up if the wrong items were being used in the components.  Must set
    ;; :rank slots again at end of this method, after components have been
    ;; adjusted.
    (Set-Rank-Slots the-lister)
    
    ;; First determine if they changed, and continue only if they did...
    ;; This also sets the update-slots-values array with current values
    (dolist (slot (g-value the-lister :update-slots))
      (unless (equal (setq temp (g-value the-lister slot))
		     (aref us-vals (incf us-vals-index)))
	(setq us-vals-changed? T)
	(setf (aref us-vals us-vals-index) temp)))

    ;; Add special check for :force-computation?, called from
    ;; add-, move-, and remove-component
    (when (g-local-value the-lister :force-computation?)
      (setq us-vals-changed? T)
      (s-value the-lister :force-computation? NIL))
    
    (when us-vals-changed?
      (if (aref us-vals +lister-direction+)
	  (let* ((components        (g-local-value the-lister :components))
		 (max-width         (aref us-vals +lister-max-width+))
		 (max-height        (aref us-vals +lister-max-height+))
		 (left              (aref us-vals +lister-left+))
		 (top               (aref us-vals +lister-top+))
		 (direction         (aref us-vals +lister-direction+))
		 (h-spacing         (aref us-vals +lister-h-spacing+))
		 (v-spacing         (aref us-vals +lister-v-spacing+))
		 (indent            (aref us-vals +lister-indent+))
		 (h-align           (aref us-vals +lister-h-align+))
		 (v-align           (aref us-vals +lister-v-align+))
		 (fixed-width-p     (aref us-vals +lister-fixed-width-p+))
		 (fixed-height-p    (aref us-vals +lister-fixed-height-p+))
		 (fixed-width-size  (aref us-vals +lister-fixed-width-size+))
		 (fixed-height-size (aref us-vals +lister-fixed-height-size+))
		 (rank-margin       (aref us-vals +lister-rank-margin+))
		 (pixel-margin      (aref us-vals +lister-pixel-margin+))
		 ;; RGA added these for right-justify-last
		 ;; (help-style) mode layout  
		 (right-justify-last (aref us-vals +lister-right-justify-last+))
		 (bottom-justify-last (aref us-vals +lister-bottom-justify-last+))
		 (lister-width (if right-justify-last
				   (g-value the-lister :justify-width)))
		 (lister-height (if bottom-justify-last
				    (g-value the-lister :justify-height)))
		 (fixed-width       (if fixed-width-p
					(or fixed-width-size  max-width)))
		 (fixed-height      (if fixed-height-p
					(or fixed-height-size max-height)))
		 (left-offset       0)
		 (top-offset        0)
		 (first-pass?       T)
		 (line-count        0)    ; # objects already on this line
		 (max-line-width    0)
		 (max-line-height   0)
		 (rank             -1)
		 comp-width comp-height left-adjust top-adjust)
	    
	    (dolist (component components)
	      (when (g-value component :visible)
		(setq line-count (1+ line-count))
		
		;; first determine left-offset and top-offset for FIELD...     
		;; NOTE:  There are 2 PROGNs, one for each direction.  They are
		;;        EXACT analogies of each other, and MUST be maintained
		;;        as such (ie, if you changed one, change the other
		;;        accordingly -- this requires MAPPING "top" to "left",
		;;        "height" to "width", etc...)
		
		(if (eq direction :horizontal)
		    (progn
		      (if first-pass?
			  (setq first-pass? NIL)
			  (setq left-offset (+ h-spacing
					       left-offset
					       (or fixed-width comp-width))))
		      
		      (setq comp-width  (g-value component :width))
		      (setq comp-height (g-value component :height))
		      
		      ;; do we have to linewrap?
		      (when (or (and rank-margin
				     (> line-count rank-margin))
				(and pixel-margin
				     (> (+ left-offset
					   (or fixed-width comp-width))
					pixel-margin)))
			(setq left-offset indent)
			(setq top-offset
			      (+ v-spacing top-offset
				 (or fixed-height max-line-height)))
			(setq max-line-height 0)
			(setq line-count 1))
		      ;; RGA on last component check
		      ;; right-justify-last
		      (when (and right-justify-last
				 (null (g-value component :next)))
			(setq left-offset
			  (max left-offset
			       (- lister-width (or fixed-width comp-width)))))

		      (setq max-line-height (max max-line-height comp-height)))
		    
					; else, direction is :vertical
		    (progn
		      (if first-pass?
			  (setq first-pass? NIL)
			  (setq top-offset (+ v-spacing
					      top-offset
					      (or fixed-height comp-height))))
		      
		      (setq comp-height (g-value component :height))
		      (setq comp-width  (g-value component :width))
		      
		      ;; do we have to linewrap?
		      (when (or (and rank-margin
				     (> line-count rank-margin))
				(and pixel-margin
				     (> (+ top-offset (or fixed-height
							  comp-height))
					pixel-margin)))
			(setq top-offset indent)
			(setq left-offset
			      (+ h-spacing left-offset
				 (or fixed-width max-line-width)))
			(setq max-line-width 0)
			(setq line-count 1))
		      ;; RGA on last component check
		      ;; bottom(right)-justify-last
		      (when (and bottom-justify-last
				 (null (g-value component :next)))
			(setq top-offset
			  (max top-offset
			       (- lister-height (or fixed-height comp-height)))))

		      
		      (setq max-line-width (max max-line-width comp-width))))
		
		;; now we have the left-offset and top-offset for FIELD
		;; but these might have to be adjusted for alignment now.
		;; (hence, left-adjust and top-adjust)
		(setq left-adjust
		      (if fixed-width-p
			  (case h-align
			    (:left   0)
			    (:center (floor (/ (- fixed-width comp-width) 2)))
			    (:right  (- fixed-width comp-width)))
			  0))
		(setq top-adjust
		      (if fixed-height-p
			  (case v-align
			    (:top    0)
			    (:center (floor (/ (- fixed-height comp-height)
					       2)))
			    (:bottom (- fixed-height comp-height)))
			  0))
		
		;; Everything is computed for this entry, so set :left and :top
		(s-value component :left (+ left left-offset left-adjust))
		(s-value component :top  (+ top  top-offset  top-adjust))
		(when (progn (g-value the-lister :layout-fixed?)
			     (kr::slot-constant-p the-lister :layout-fixed?))
		  (declare-constant component :left)
		  (declare-constant component :top))
		(unless (eq (incf rank) (get-local-value component :rank))
		  (s-value component :rank rank))
		))
	    ) ; close let*

	  ;; This aggrelist has a direction of NIL, so just set :rank slot
	  (Set-Rank-Slots the-lister)
	  ) ; close if 
      )))


(define-method :notice-items-changed opal:aggrelist
               (alist &optional no-propagation)
  (declare (ignore no-propagation))
  (fix-update-slots alist))
