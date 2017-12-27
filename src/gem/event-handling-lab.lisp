
(defun x-event-handler-new (root-window ignore-keys)
  (setf *my-root-window* root-window)
  (setf  *my-ignore-keys* ignore-keys)
  (apply 'process-x-event
	 (let* ((building-composit-event nil)
		(valid-event nil)
		(composite-event nil))
	   (declare (ignore building-composit-event))
	   (declare (ignore valid-event))
	   (do* ((event (fetch-next-event root-window ignore-keys)
			(fetch-next-event root-window ignore-keys))
		 (composite-event (if event
				      (push (get-event-type event) composite-event)
				      composite-event)
				  (if event
				      (push (get-event-type event) composite-event)
				      composite-event)))
		((or (timeout-p composit-event)
		     (not (any-more-p composite-event)))
		 (if (valid-event-p composite-event)
		     (create-garnet-event composite-event)
		     nil))
	     (format t "event: ~s!%" event)
	     (push event composite-event)))))

(defun fetch-next-event (root-window ignore-keys)
  (let* ((display (the-display root-window))
	 new-above-sibling
	 new-code
	 new-count
	 new-data
	 new-event-key
	 new-event-window
	 new-format
	 new-height
	 new-state
	 new-time
	 new-type
	 new-width
	 new-x
	 new-y
	 new-event-type)
    (xlib:event-case
	(display :discard-p t :timeout (if ignore-keys 0 NIL))
      ;; this first  one is for when a window is deleted by the wm
      (:CLIENT-MESSAGE
       (event-window type data format)
       (setf new-event-type :CLIENT-MESSAGE
	     new-event-window event-window
	     new-type type
	     new-data data
	     new-format format))
      (:MAP-NOTIFY
       (event-window)
       (setf new-event-type :CLIENT-MESSAGE
	     new-event-window event-window))
      (:UNMAP-NOTIFY
       (event-window)
       (setf new-event-type :UNMAP-NOTIFY
	     new-event-window event-window))
      (:REPARENT-NOTIFY
       (event-window)
       (setf new-event-type :REPARENT-NOTIFY
	     new-event-window event-window))
      (:CIRCULATE-NOTIFY
       ()
       (setf new-event-type :CIRCULATE-NOTIFY))
      (:GRAVITY-NOTIFY
       ()
       (setf new-event-type :GRAVITY-NOTIFY))
      (:DESTROY-NOTIFY
       (event-window)
       (setf new-event-type :DESTROY-NOTIFY
	     new-event-window event-window))
      (:CONFIGURE-NOTIFY
       (event-window x y width height above-sibling)
       (setf new-event-type :CONFIGURE-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-width width
	     new-height height
	     new-above-sibling above-sibling))
      (:EXPOSURE
       (event-window x y width height count)
       (setf new-event-type :EXPOSURE
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-width width
	     new-height height
	     new-count count))
      (:KEY-PRESS
       (event-window x y state code time)
       (setf new-event-type :KEY-PRESS
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time))
      (:BUTTON-PRESS
       (event-window x y state code time event-key)
       (setf new-event-type :BUTTON-PRESS
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time
	     new-event-key event-key))
      (:BUTTON-RELEASE
       (event-window x y state code time event-key)
       (setf new-event-type :BUTTON-RELEASE
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time
	     new-event-key event-key))
      (:MOTION-NOTIFY
       (event-window x y)
       (setf new-event-type :MOTION-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y))
      (:ENTER-NOTIFY
       (event-window x y time)
       (setf new-event-type :ENTER-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-time time))
      (:LEAVE-NOTIFY
       (event-window x y time)
       (setf new-event-type :LEAVE-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-time time))
      (:NO-EXPOSURE
       ()
       (setf new-event-type :NO-EXPOSURE))
      (OTHERWISE
       ()
       (setf new-event-type :OTHERWISE)))
    (list root-window
	  ignore-keys
	  new-above-sibling
	  new-code
	  new-count
	  new-data
	  new-event-key
	  new-event-window
	  new-format
	  new-height
	  new-state
	  new-time
	  new-type
	  new-width
	  new-x
	  new-y
	  new-event-type)))

(defun process-x-event (root-window
			ignore-keys
			above-sibling
			code
			count
			data
			event-key
			event-window
			format
			height
			state
			time
			my-type
			width
			x
			y
			event-type)
  (let ((display (the-display root-window)))
    ;; if the event is a release, wait 100 milliseconds to see if the
    ;; next character
    (case event-type
      (:CLIENT-MESSAGE
       (event-handler-debug :CLIENT-MESSAGE event-window my-type data format)
       (interactors::do-client-message event-window my-type data format display))
      (:MAP-NOTIFY
       (event-handler-debug :MAP-NOTIFY)
       (interactors::do-map-notify (x-window-from-drawable root-window
							   event-window)))
      (:UNMAP-NOTIFY
       (event-handler-debug :UNMAP-NOTIFY)
       (interactors::do-unmap-notify (x-window-from-drawable root-window
							     event-window)))
      (:REPARENT-NOTIFY
       (event-handler-debug :REPARENT-NOTIFY)
       (if (connected-window-p event-window)
	   (let ((window (x-window-from-drawable root-window event-window)))
	     (s-value window :already-initialized-border-widths nil)
	     (s-value window :lineage (lineage-of-drawable event-window)))))
      (:CIRCULATE-NOTIFY
       (event-handler-debug :CIRCULATE-NOTIFY)
       (interactors::do-circulate-notify))
      (:GRAVITY-NOTIFY
       (event-handler-debug :GRAVITY-NOTIFY)
       (interactors::do-gravity-notify))
      (:DESTROY-NOTIFY
       (event-handler-debug :DESTROY-NOTIFY)
       (destroy-notify-window event-window))
      (:CONFIGURE-NOTIFY
       (event-handler-debug :CONFIGURE-NOTIFY)
       (if (connected-window-p event-window)
	   (interactors::do-configure-notify (x-window-from-drawable root-window
								     event-window)
	     x y width height above-sibling)))
      (:EXPOSURE
       (event-handler-debug :EXPOSURE x y width height count)
       (when (connected-window-p event-window)
	 (interactors::do-exposure (x-window-from-drawable root-window event-window)
	   x y width height count display)))
      (:KEY-PRESS
       (event-handler-debug :KEY-PRESS event-window x y state code time)
       (if ignore-keys
	   ;; We don't want keys, but check if this is the abort key
	   (let ((c (x-translate-character *root-window* 0 0 state code 0)))
	     (when (eq c interactors::*garnet-break-key*)
	       (format T "~%**Aborting transcript due to user command**~%")
	       (return-from process-x-event :abort)))
	   ;; Normal case: we do want keys
	   (interactors::do-key-press
	       (x-window-from-drawable root-window event-window)
	     x y state code time)))
      (:BUTTON-PRESS
       (setf *last-button-press* time)
       (event-handler-debug :BUTTON-PRESS event-window x y state code time
			    event-key)
       (unless ignore-keys
	 (interactors::do-button-press (x-window-from-drawable root-window
							       event-window)
	   x y state code time event-key)))
      (:BUTTON-RELEASE
       (event-handler-debug :BUTTON-RELEASE event-window x y state code time
			    event-key)
       (unless ignore-keys
	 (interactors::do-button-release (x-window-from-drawable root-window
								 event-window)
	   x y state code time event-key))
       (setf *last-button-press* nil))
      (:MOTION-NOTIFY
       (event-handler-debug :MOTION-NOTIFY event-window x y)
       (unless ignore-keys
	 (interactors::do-motion-notify (x-window-from-drawable root-window event-window)
	   x y display)))
      (:ENTER-NOTIFY
       (event-handler-debug :ENTER-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-enter-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:LEAVE-NOTIFY
       (event-handler-debug :LEAVE-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-leave-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:NO-EXPOSURE
       (event-handler-debug :NO-EXPOSURE)
       (unless ignore-keys
	 t))
      (:OTHERWISE
       (event-handler-debug :UNKNOWN)
       t))))
