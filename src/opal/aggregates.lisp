
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
