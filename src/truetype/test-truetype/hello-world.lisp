;;;;
;;;; REPL playing

(defpackage #:clx-truetype-test
  (:nicknames :xft-test)
  (:use #:cl #:xft)
  (:export show-window))

(in-package :clx-truetype-test)

(defparameter *display* nil)
;;(defparameter *display* (xlib:open-default-display "192.168.1.101:0.0"))

(defparameter *screen* nil)
(defparameter *root* nil)

(defparameter xsize 800)
(defparameter ysize 400)
(defparameter *w* nil)

(defparameter *w-agg* nil)

(defparameter *font* nil)


(defun show-window ()
  (setf *w*
	(kr:create-instance nil inter:interactor-window
	  (:title "Test")
	  (:width xsize) (:height ysize)
	  (:left (floor (- opal:*screen-width* xsize) 2)) (:top 350)
	  (:double-buffered-p t)
	  (:foreground-color opal:blue)
	  (:background-color opal:white))
	*w-agg*
	(kr:create-instance *w-agg*  opal:aggregate))

  (kr:s-value *w* :aggregate *w-agg*)
  
  (setf *font* (kr:create-instance nil opal::truetype-font
		 (:family "Linux Libertine G")
		 (:subfamily "Regular")
		 (:size 20)))

    (unwind-protect
         (progn
           (xlib:map-window window)
           (setf (xlib:gcontext-foreground grackon) black)
           (xlib:event-case (*display* :force-output-p t
                                  :discard-p t)
             (:exposure ()
                        (xlib:clear-area window :width (xlib:drawable-width window)
                                         :height (xlib:drawable-height window))
                        (draw-text window grackon font 
				   "The quick brown fox jumps over the lazy dog." 
				   100 100 :draw-background-p t)
                        (when (= 0 (random 2))
                          (rotatef (xlib:gcontext-foreground grackon) (xlib:gcontext-background grackon)))
                        (draw-text-line window grackon font
					"Съешь же ещё этих мягких французских булок, да выпей чаю." 
					100 (+ 100 (baseline-to-baseline window font)) :draw-background-p t)
                        (setf (font-antialias font) (= 0 (random 2)))
                        (if (= 0 (random 2))
                            (setf (font-subfamily font) "Regular")
#-(and)                            (setf (font-subfamily font) "Italic"))
                        (draw-text window grackon font 
;;				   "Жебракують філософи при ґанку церкви в Гадячі, ще й шатро їхнє п’яне знаємо." 
				   "Ἐν ἀρχῇ ἦν ὁ λόγος, καὶ ὁ λόγος ἦν πρὸς τὸν θεόν, καὶ θεὸς ἦν ὁ λόγος."
				   100 (+ 100 (* 2 (baseline-to-baseline window font))) :draw-background-p t)
                        (draw-text window grackon font
				   "Press space to exit. Нажмите пробел для выхода." 
				   100 (+ 100 (* 3 (baseline-to-baseline window font))) :draw-background-p t)
                        nil)
             (:button-press () t)
             (:key-press (code state) (char= #\Space (xlib:keycode->character *display* code state)))))
      (progn
        (xlib:free-gcontext grackon)
        (xlib:destroy-window window)
        (xlib:close-display *display*)))))



		       
	    
