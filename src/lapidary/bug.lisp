(in-package 'lapidary)

(defun make-instance-or-copy (instance-p)
  (let* ((objs (g-value *selection-info* :selected))
	 editor-agg result obj-name)
    (cond ((null objs) (lapidary-error "must select an object first")
	  (t
	   ; this prevents create-instance from copying feedback links
	   (primary-deselect-objects :none)
	   (secondary-deselect-objects :none)

	   (dolist (obj objs)
	     ;; remove corrupted slots from the object
	     (remove-corrupted-slots obj)

	     ;; ask the user to name the new instance
#|
	     (setf obj-name 
		   (keyword-from-string
		    (lapidary-prompt-for-input "please enter object name: ")))
|#

             (setf obj-name
	       (keyword-from-string (princ-to-string (kr::schema-name obj))))

	     (if instance-p
		 ; create the instance
		 (setf result (create-instance nil obj))
	         ; create a copy
	         (setf result (opal:copy-gadget obj nil)))
	   
	     ;;; put the instance in the correct window
	     (setf editor-agg (g-value obj :window :editor-agg))
	     (opal:add-component editor-agg result)

	     ;; create the :known-as slot and place a pointer to this aggregate
	     ;; in its new parent
	     (s-value editor-agg obj-name result)
	     (s-value result :known-as obj-name)

	     ;;; move the instance 20 pixels to the right and below the
	     ;;; object it is based on
	     (if (is-a-line-p result)
		 (progn
		   (s-value result :x1 (+ 20 (g-value obj :x1)))
		   (s-value result :y1 (+ 20 (g-value obj :y1)))
		   (s-value result :x2 (+ 20 (g-value obj :x2)))
		   (s-value result :y2 (+ 20 (g-value obj :y2))))
	         (opal:set-position result (+ 20 (g-value obj :left))
				           (+ 20 (g-value obj :top))))
	     
	     ;;; Select the instance
	     (primary-select result)))))) 