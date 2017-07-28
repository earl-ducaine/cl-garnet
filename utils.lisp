

(defun get-root-window ()
  (display-info-display  (gv gem::*root-window* :display-info)))

(defun get-screen ()
  (first (xlib::display-roots (display-info-display  (gv gem::*root-window* :display-info)))))



(defun get-screen-height-mm ()
  (xlib::screen-height-in-millimeters
   (first (xlib::display-roots
	   (display-info-display
	    (gv gem::*root-window* :display-info))))))

(defun get-screen-width-mm ()
  (xlib::screen-width-in-millimeters
   (first (xlib::display-roots
	   (display-info-display
	    (gv gem::*root-window* :display-info))))))

(defun get-screen-height-pixels ()
  (xlib::screen-height (first (xlib::display-roots
			       (display-info-display
				(gv gem::*root-window* :display-info))))))

(defun get-screen-width-pixels ()
  (xlib::screen-width (first (xlib::display-roots
			       (display-info-display
				(gv gem::*root-window* :display-info))))))



(defun compute-scaling-factor ()
  (let* ((height-mm (get-screen-height-mm))
	 (width-mm (get-screen-width-mm))
	 (height-inch (* 0.0393701 height-mm))
	 (width-inch (* 0.0393701 width-mm))
	 (height-pixels (get-screen-height-pixels))
	 (width-pixels (get-screen-width-pixels))
	 (dpi (/ height-pixels height-inch)))
    (/ dpi 72)))


