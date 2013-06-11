;;; Ken Meltsner's patches for Garnet/Lucid LISP to allow for safe
;;; multiprocessing.
;;;
;;; Contact: meltsner@chipman.crd.ge.com

;;; A process is created at initialization time.  Call with-pause to do
;;; something with the main-event-loop process paused.

;;; Based, in large part, on Rod William's multiprocessing modifications
;;; to Garnet.

;;; Added a safe event loop pause function, temporary suspension of other
;;; processes to fix errors in the Garnet loop, checks when killing processes
;;; to make sure process isn't already dead, etc.  Broke out code
;;; from Garnet files to make it easy to install.

;;; File replaces functions from OPAL and INTERACTORS package

(in-package "OPAL")
(proclaim '(optimize (compilation-speed 0))) ;bug in Lucid/CLX, fixed in 4.4


;;; William's mods for multiple process for Garnet
#+lcl3.0 (defvar *event-loop-display-list* nil)
#+lcl3.0 
(defun enable-event-loop-process (display)
     (let ((process (lcl:make-process :name "Garnet Event Loop"
				      :priority 200
                                      :stack-size 5000
                                      :function #'default-event-handler
                                      :args (list display))))
        (setq *event-loop-display-list* (push (cons display process)
                                              *event-loop-display-list*))))
(defun Disconnect-Garnet ()
  (setq *all-the-windows*
    (remove-if                               ;       This should remove
      #'(lambda (w)                          ;     just opal:window and
          (kr:get-local-value w :update))    ;  inter:interactor-window
      (all-the-instances opal:window)))      ; from list of all windows
  (setq *all-windows-which-have-been-closed* nil)
  ;;; Make all the windows invisible.
  (dolist (w *all-the-windows*) 
    (when (kr:g-value w :visible)
       (push w *all-windows-which-have-been-closed*)
       (kr:s-value w :visible nil) 
       (funcall (kr:g-value w :update) w)))  ; generalized update
  ;;; Remove all connections to X from the text objects,
  ;;; (even those hidden in the :update-slots-values slot!)
  (dolist (txt (all-the-instances opal:text))
    (when (kr:g-cached-value txt :xfont)
      (xlib:close-font (kr:g-cached-value txt :xfont))
      (kr:s-value txt :xfont nil))
    (when (kr:g-cached-value txt :update-slots-values)
      (setf (aref (kr:g-cached-value txt :update-slots-values)
		  opal::*text-xfont*)
	    nil)))
  (dolist (fnt (all-the-instances opal:font-from-file))
    (kr:s-value fnt :display-xfont-plist nil))
  ;;; Remove all connections to X from the window objects.
  (setf opal::*display-name-to-display-mapping* nil)
  (dolist (w *all-the-windows*)
    (kr:s-value w :drawable nil)
    (kr:s-value w :event-mask nil)
    (let ((d (kr:g-cached-value w :display-info)))
      (when d
        (kr:s-value w :display-info nil)
  #+cmu (ext:disable-clx-event-handling (opal::display-info-display d))
  #+lcl3.0 (disable-event-loop-process (opal::display-info-display d))
)))
)
#+lcl3.0
(defun disable-event-loop-process (display)
   (let ((display-process (assoc display *event-loop-display-list*)))
      (when (lcl:processp (cdr display-process))
	    (lcl:kill-process (cdr display-process)))
      (setq *event-loop-display-list* (remove display-process
*event-loop-display-list*))))

(defun initialize-display (display-name)
  (let ((display 
	 (cdr (assoc display-name *display-name-to-display-mapping*
		     :test '(lambda (s1 s2)
			      (let ((end (min (length s1)
					      (length s2))))
				(string-equal s1 s2
					      :end1 end)))))))
    (when (null display)
      (let* ((x-display (xlib:open-display display-name))
	     (x-screen (nth common-lisp-user::Garnet-Screen-Number
                            (xlib:display-roots x-display)))
	     (x-root (xlib:screen-root x-screen))
	     (x-line-style-gc
	      (xlib:create-gcontext :drawable x-root
				    :function 2
				    :foreground (g-value opal:black
							 :colormap-index)
				    :background (g-value opal:white
							 :colormap-index)
				    :line-width 0
				    :line-style :solid
				    :cap-style :butt
				    :join-style :miter
				    :fill-style :solid
				    :fill-rule :even-odd))
	     (x-filling-style-gc
	      (xlib:create-gcontext :drawable x-root
				    :function 2
				    :foreground (g-value opal:black
							 :colormap-index)
				    :background (g-value opal:white
							 :colormap-index)
				    :line-width 0
				    :line-style :solid
				    :cap-style :butt
				    :join-style :miter
				    :fill-style :solid
				    :fill-rule :even-odd))
	     (opal-line-style-gc
		(make-opal-gc	:gcontext x-line-style-gc
				:opal-style NIL
				:function 2
				:line-width 0
				:line-style :solid
				:cap-style  :butt
				:join-style :miter
				:dashes NIL
				:font   NIL
				:fill-style :solid
				:fill-rule  :even-odd
				:stipple   NIL
				:clip-mask :none))
	     (opal-filling-style-gc
		(make-opal-gc	:gcontext x-filling-style-gc
				:opal-style NIL
				:function 2
				:line-width 0
				:line-style :solid
				:cap-style  :butt
				:join-style :miter
				:dashes NIL
				:font   NIL
				:fill-style :solid
				:fill-rule  :even-odd
				:stipple   NIL
				:clip-mask :none)))
	(setf display
	      (make-display-info :display x-display
				 :screen x-screen
				 :root-window x-root
				 :line-style-gc opal-line-style-gc
				 :filling-style-gc opal-filling-style-gc))
	#+cmu (ext:enable-clx-event-handling x-display
				       'default-event-handler)
      #+lcl3.0 (enable-event-loop-process x-display)
      )
      (push (cons display-name display) *display-name-to-display-mapping*))
    display))

(defun create-x-drawable (a-window)
  (let* ((display-info (initialize-display (g-value a-window :display)))
	 (title-name (g-value a-window :title))
	 (left (g-value a-window :left))
	 (top  (g-value a-window :top))
	 (border-width (g-value a-window :border-width))
	 (width  (g-value a-window :width))
	 (height (g-value a-window :height))
	 (parent (get-parent-win a-window display-info))
	 (screen (display-info-screen display-info))
	 (white-pixel (xlib:screen-white-pixel screen))
	 (black-pixel (xlib:screen-black-pixel screen))
	 (drawable (xlib:create-window
		    :parent parent
		    :x left
		    :y top
		    :width width
		    :height height
		    :background white-pixel
		    :border-width border-width
		    :border black-pixel
	       	    :override-redirect :off
		    :event-mask *exposure-event-mask*
		    :class :input-output)))
    (if (g-value a-window :position-by-hand)
	(xlib:set-standard-properties drawable
				   :name title-name
				   :icon-name (or (g-value a-window :icon-title)
						  title-name)
				   :resource-name "Garnet"
				   :width width
				   :height height
				   :user-specified-position-p nil)
	(xlib:set-standard-properties drawable
				   :name title-name
				   :icon-name (or (g-value a-window :icon-title)
						  title-name)
				   :resource-name "Garnet"
				   :width width
				   :height height
				   :x left :y top
				   :user-specified-position-p t))
       
    (setf (g-value a-window :drawable) drawable)
    (setf (g-value a-window :display-info) display-info)
    (setf (gethash drawable *drawable-to-window-mapping*) a-window)
    (when (g-value a-window :double-buffered-p)
      (let* ((buffer (create-x-buffer a-window))
	     (buffer-gc (xlib:create-gcontext :drawable buffer
				:foreground black-pixel
				:background white-pixel)))
        (s-value a-window :buffer buffer)
        (s-value a-window :buffer-gcontext buffer-gc)
	(clear-buffer buffer buffer-gc)))
	
    (s-value a-window :top-border-width border-width)
    (s-value a-window :left-border-width border-width)
    (s-value a-window :bottom-border-width border-width)
    (s-value a-window :right-border-width border-width)

    (setf *windows-that-have-never-been-updated*
      (delete a-window *windows-that-have-never-been-updated*))

    ;; set the cursor to hemlock's cursor or specified cursor/mask combo
    ;; (cursor-file . mask-file)
    (set-window-cursor display-info
		       drawable
		       (g-value a-window :cursor))

;;;  WE NO LONGER PROPAGATE DOWN INTO OBJECTS!!!
    ;; if an aggregate is specified, propagate the drawable, and
    ;; display-info structures down through it
    ;;;(let ((a-aggregate (g-value a-window :aggregate)))
      ;;;(when a-aggregate
	;;;(propagate-down a-aggregate drawable display-info)))

    ;; bring up the window, and display it
    (when (g-value a-window :visible)
      (xlib:map-window drawable)
#-(or lcl3.0 cmu)
      (xlib:display-force-output (display-info-display display-info))
      ;; Wait until map-notify actually takes place
#-lcl3.0
      (xlib:event-case ((display-info-display display-info) :discard-p nil
			:peek-p t :timeout 15)
	(:map-notify (event-window) (eq event-window drawable)))
      )
    drawable))

(in-package "INTER")

(defun handle-error-in-loop (condition)
  (lcl:using-initial-io
   (format t "~%Suspending other processes to fix Garnet error~%")
   (lcl:invoke-debugger condition)))

(defvar *pause-loop-safely* nil)

(defmacro with-pause (&body stuff)
  `(progn
     (setq *pause-loop-safely* t)
     (unwind-protect
	 (progn ,@stuff)
       (setq *pause-loop-safely* nil))))

(defun pause-loop () (setq *pause-loop-safely* t))
(defun resume-loop () (setq *pause-loop-safely* nil))

(export '(*pause-loop-safely* with-pause pause-loop resume-loop))
(defun opal::default-event-handler (display)
  (loop  
   (cond (*pause-loop-safely* 
	  (sys:process-wait-allowing-scheduling "Loading something big"
						#'(lambda ()
						    (null *pause-loop-safely*))
						))

	 (t (opal::default-event-handler2 display)))))

(defun opal::default-event-handler2 (display)
  "Event handler for the interactor windows"
  (lcl:handler-bind 
   ((lcl::error #'inter::handle-error-in-loop))
   (xlib:event-case 
    (display :discard-p t)
    (:MAP-NOTIFY (event-window)
		 (opal::Map-Notify (debug-p :event) event-window)
		 )
    (:UNMAP-NOTIFY (event-window)
		   (opal::Unmap-Notify (debug-p :event) event-window)
		   )
    (:REPARENT-NOTIFY (event-window x y)
		      (opal::Reparent-Notify (debug-p :event) event-window x y)
		      )
    (:CIRCULATE-NOTIFY () (opal::Circulate-Notify (debug-p :event))
		       )
    (:GRAVITY-NOTIFY () (opal::Gravity-Notify (debug-p :event)) )
    (:DESTROY-NOTIFY (event-window)
		     (opal::Destroy-Notify (debug-p :event) event-window)
		     )
    (:CONFIGURE-NOTIFY (x y width height event-window above-sibling)
		       (opal::Configure-Notify (debug-p :event) x y
					       width height
					       event-window above-sibling)
		       )
    (:EXPOSURE (event-window count x y width height)
	       (opal::Exposure (debug-p :event) event-window count x y
width height display)
	       )
    (:KEY-PRESS (event-window x y state code time)
		(if *trans-from-file* T ; ignore events when read transcript
		  (Key-Press event-window x y state code time))
		)
    (:BUTTON-PRESS (event-window x y state code event-key time)
		   (if *trans-from-file* T ; ignore events when read transcript
		     (Button-Press event-window x y
				   state code event-key time))
		   )
    (:BUTTON-RELEASE (event-window x y state code event-key time)
		     (if *trans-from-file* T ; ignore events when read transcript
		       (Button-Release event-window x y
				       state code event-key time))
		     )
    (:MOTION-NOTIFY (event-window x y)
		    (if *trans-from-file* T ; ignore events when read transcript
		      (Motion-Notify event-window x y display))
		    )
    (:NO-EXPOSURE () t )
    (OTHERWISE () (format t "illegal event") t ))))

(defun main-event-loop (&optional window)
  "Event handler for the interactor windows"
  #+(or lcl3.0 cmu) (declare (ignore window))
  #+(or cmu lcl3.0)
  (format t "Calling main-event-loop in Lucid or CMU Common Lisp is not necessary")
  #-(or lcl3.0 cmu)
  (let ((display (if window
                     (opal::display-info-display
                        (g-value window :display-info))
		     (let ((win1 (caar (opal::get-table-contents))))
		       (if win1
			   (xlib:window-display win1)
                           opal::*default-x-display*)))))
   ; Changed call to xlib:discard-current-event to xlib:event-case
   ; (because the former was having no effect in Lucid). -- Pervin 4/9/90
   (unwind-protect
     (catch 'exit-main-loop-exception
       (opal::default-event-handler display)))
   (xlib:event-case (display :discard-p t :timeout 5) ; discard current event
     (otherwise () t)))				      ; which was the typing
)					   	      ; of the *garnet-break-key*

(defun exit-main-event-loop ()
  #-(or lcl3.0 cmu)
  (throw 'exit-main-loop-exception t))

