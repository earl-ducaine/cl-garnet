
(in-package :opal)

(define-method :destroy-me opal::aggregate (a-aggregate &optional (top-level-p T))
  (if a-aggregate
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
	  ;; Need to disable constants, since components slot is constant on agregadgets.
	  (with-constants-disabled
	    (s-value parent :components
		     (delete a-aggregate (g-local-value parent :components))))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))

(defun install-component (a-aggregate gob args)
  (let (locator where)
    (cond ((eq (first args) :where)
                (setq where (second args))
                (setq locator (third args)))
          ((first args)
                (setq where (first args))
                (setq locator (second args)))
          (t (setq where :front)))
    (case where
      ((:front :tail)
       (s-value a-aggregate :components
		(nconc (g-local-value a-aggregate :components)
		       (list gob))))
      ((:behind :before)
       (let ((components (g-local-value a-aggregate :components)))
	 (do ((smash-slot components (cdr smash-slot))
	      (pre-splice nil smash-slot))
	     ((or (eq (car smash-slot) locator)
		  (null smash-slot))
	      (cond ((null smash-slot)
		     (error "Locator for :where :behind ~S is not in aggregate ~S."
			    locator a-aggregate))
		    (pre-splice
		     (setf (cdr pre-splice) (cons gob smash-slot))
		     (s-value a-aggregate :components components))
		    (t (s-value a-aggregate
				:components (cons gob smash-slot))))))))
      ((:in-front :after)
       (let* ((components (g-local-value a-aggregate :components))
	      (remainder (member locator components))
	      (splice (cons gob (cdr remainder))))
	 (cond (remainder
		(setf (cdr remainder) splice)
		(s-value a-aggregate :components components))
	       (t (error "Locator for :where :in-front ~S is not in aggregate ~S"
			 locator a-aggregate)))))
      (:at
       (let* ((components (g-local-value a-aggregate :components))
	      (remainder (unless (zerop locator)
			   (nthcdr (1- locator) components))))
	 (s-value a-aggregate :components
		  (if (zerop locator)
		      (cons gob components)
		      (progn
			(setf (cdr remainder) (cons gob (cdr remainder)))
			components)))))
      ((:back :head)
       (s-value a-aggregate :components
		(cons gob (g-local-value a-aggregate :components))))
      (otherwise
       (s-value a-aggregate :components
		(nconc (g-local-value a-aggregate :components)
		       (list gob)))
       (warn (format nil "Bad where option in add-component: ~S." where))))))

(define-method :add-component opal::aggregate (a-aggregate gob &rest args)
  (if (eq gob (g-local-value gob :window))		;; Is this a window?
      (error "*** ~A is a WINDOW, and was not added to ~A~%"
	     gob a-aggregate))
  (let ((parent (g-local-value gob :parent))
	(internally-parented (g-local-value gob :internally-parented)))
    (if (and parent (not internally-parented))
	(error "Graphical-object ~S has :parent ~S already." gob parent))
    (install-component a-aggregate gob args)
    ;; Set up the reverse pointer from child to aggregate
    (if internally-parented
	(destroy-slot gob :internally-parented)
	(s-value gob :parent a-aggregate)))
  ;; Propagate window and dirty bit to children
  (let ((a-window (g-local-value a-aggregate :window)))
    (when a-window
      (let ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info))))
	(set-display-slots gob a-window t)
	(setf (bbox-valid-p (update-info-old-bbox gob-update-info)) NIL))))
  ;; Signal we have changed components list
  (mark-as-changed a-aggregate :components)
  ;; Return gob
  gob)

(defun add-components (agg &rest components)
  (dolist (component components)
    (add-component agg component))
  (car (last components)))
