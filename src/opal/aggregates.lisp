
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
	;; Place the object on its window's invalid-objects list
	(make-object-invalid gob gob-update-info a-window)
	;; Invalidate all of the aggregate's children (recursively)
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob
	      #'(lambda (c)
		  (let ((c-update-info (g-value c :update-info)))
		    (make-object-invalid c c-update-info a-window)))))
	(setf (bbox-valid-p (update-info-old-bbox gob-update-info)) NIL))))
  ;; Signal we have changed components list
  (mark-as-changed a-aggregate :components)
  ;; Return gob
  gob)

(defun add-components (agg &rest components)
  (dolist (component components)
    (add-component agg component))
  (car (last components)))

(define-method :remove-component opal::aggregate (a-aggregate gob)
 (if (not (eq (g-local-value gob :parent) a-aggregate))
  (warn (format nil "Cannot remove-component ~A from ~A" gob a-aggregate))
  (let* ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info)))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info
	    (when a-window (g-local-value a-window :update-info))))
    (when a-window-update-info
      (let* ((win-update-info (g-value a-window :win-update-info))
	     (window-bbox (update-info-old-bbox a-window-update-info))
	     (bbox (update-info-old-bbox gob-update-info)))
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob #'(lambda (c)
				       (remove-from-invalid-objects-list
					c win-update-info))
			       :self T)
	    (remove-from-invalid-objects-list gob win-update-info))
	(merge-bbox window-bbox bbox)
        (set-display-slots gob nil nil)))
    (setf (update-info-invalid-p gob-update-info) NIL)
    (s-value a-aggregate :components
	     (delete gob (g-local-value a-aggregate :components)
		     :from-end t :count 1))
    (s-value gob :parent NIL)
    ;; signal we have changed components list
    (mark-as-changed a-aggregate :components))))

(define-method :move-component opal::aggregate (a-aggregate gob &rest args)
  (let* ((gob-update-info (g-local-value gob :update-info))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info (g-local-value a-window :update-info)))
    (and a-window a-window-update-info
      (let ((window-bbox (update-info-old-bbox a-window-update-info))
            (bbox (update-info-old-bbox gob-update-info)))
	(merge-bbox window-bbox bbox))))
  (s-value a-aggregate :components
	   (delete gob (g-local-value a-aggregate :components)))
  (install-component a-aggregate gob args))

(defun remove-components (agg &rest components)
  (dolist (component components)
    (remove-component agg component)))

(defun remove-all-components (agg)
  (dolist (component (copy-list (g-local-value agg :components)))
    (remove-component agg component)))

(defun my-is-a-p (child types)
  (when types
    (if (listp types)
	(or (is-a-p child (car types))
	    (my-is-a-p child (cdr types)))
	(is-a-p child types))))

(define-method :do-components opal::aggregate (a-aggregate a-function
					     &key (type t) (self nil))
  (let ((children (g-local-value a-aggregate :components)))
    (dolist (child children)
      (when (or (eq type t)
		(is-a-p child type))
        (funcall a-function child)))
    (when (and self
	       (or (eq type t)
		   (is-a-p a-aggregate type)))
      (funcall a-function a-aggregate))))


;; (define-method :do-all-components opal::aggregate (a-aggregate a-function
;; 						 &key (type t) (self nil))
;;   (let ((children (g-local-value a-aggregate :components)))
;;     (dolist (child children)
;;       (if (is-a-p child opal::aggregate)
;; 	  (do-all-components child a-function :type type :self t)
;; 	  (when (or (eq type t)
;; 		    (is-a-p child type))
;; 	    (funcall a-function child))))
;;     (when (and self
;; 	       (or (eq type t)
;; 		   (is-a-p a-aggregate type)))
;;       (funcall a-function a-aggregate))))
