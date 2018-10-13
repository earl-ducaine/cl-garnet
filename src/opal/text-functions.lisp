;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.


(in-package :opal)


(defparameter Opal-Version-Number "1.3")

(defun char-width (fnt char &optional
			      (display (g-value gem:device-info :current-root)))
  (or (g-value fnt :char-width)
      (gem:character-width display fnt (char-code char))))

(defun string-width (fnt str &key (start 0) end display)
  (unless display
    (setq display (g-value gem:DEVICE-INFO :current-root)))
  ;; If it's a fixed-width font, get the stored width of a single character
  (let ((char-width (g-value fnt :char-width)))
    (if (and char-width (not (find #\newline str)))
	;; For fixed-width single line, just do multiplication
	(let ((str-len (length str)))
	  (if (null end)
	      (if (zerop start)
		  (* char-width str-len)
		  (* char-width (- str-len start)))
	      (if (zerop start)
		  (* char-width end)
		  (* char-width (- end start)))))
	;; For multiple lines, find max width of all lines
	(do* ((width 0)
	      (line-end (position #\newline str)
			(position #\newline remaining-str))
	      (current-line (if line-end (subseq str 0 line-end) str)
			    (if line-end
				(subseq remaining-str 0 line-end)
				remaining-str))
	      (remaining-str (if line-end (subseq str (1+ line-end)))
			     (if line-end
				 (subseq remaining-str (1+ line-end))))
	      (width (max width (if char-width
				    (* char-width (length current-line))
				    (if (or start end)
					(gem:text-width
					 display fnt
					 (subseq current-line start end))
					(gem:text-width display fnt current-line))))
		     (max width (if char-width
				    (* char-width (length current-line))
				    (if (or start end)
					(gem:text-width
					 display fnt
					 (subseq current-line start end))
					(gem:text-width display fnt
							current-line))))))
	     ((null line-end) width)))))

(defun string-height (fnt str &key (actual-heightp nil) display)
  (unless display
    (setq display (g-value gem:DEVICE-INFO :current-root)))
  (if (and actual-heightp (not (find #\newline str)))
      (multiple-value-bind (width ascent descent)
	  (gem:text-extents display fnt str)
	(declare (ignore width))
	(+ ascent descent))
      (* (1+ (count #\newline str))
	 (g-value fnt :font-height))))


(defun sign (n) (if (eq n 0) 0 (/ n (abs n))))

;; Given a string written in a certain font, find the index of the string
;; so that the xlib:text-width of (subseq str 0 index) is closest to
;; target.
(defun get-index (str font target)
  (let* ((root (g-value gem:device-info :current-root))
	 (string-width (gem:text-width root font str))
	 (string-length (length str)))
    (cond ((<= target 0) 0)
	  ((>= target string-width) string-length)
	  (t
	   (let ((char-width (g-value font :char-width)))
	     (if char-width          ;fixed width
		 (round target char-width)
		 (dotimes (n string-length)
		   (let ((low (gem:text-width root font (subseq str 0 n)))
			 (high (gem:text-width root font (subseq str 0 (1+ n)))))
		     (when (<= low target high)
		       (return (if (> (- target low) (- high target))
				   (1+ n)
				   n)))))))))))

(defun get-cursor-index (txt x y)
  "Given an object of type opal:text and two coordinates x and y, returns
   the index of the character in (g-value txt :string) that the point lies
   on."
  (when (point-in-gob txt x y)
    (let ((font (g-value txt :font))
	  (cut-strings (g-value txt :cut-strings))
	  (root (g-local-value txt :window)))
      (if cut-strings
	  (let* ((line-number
		  (max 0
		       (min (1- (length cut-strings))
			    (floor (- y (g-value txt :top))
				   (+ (gem:max-character-ascent root font)
				      (gem:max-character-descent root font))))))
		 (cut-string (nth line-number cut-strings))
		 (relative-index 0))
	    (dotimes (i line-number)
	      (incf relative-index
		    (1+ (length (cut-string-string (nth i cut-strings))))))
	    (+ relative-index
	       (get-index (cut-string-string cut-string)
			  font
			  (- x
			     (g-value txt :left)
			     (case (g-value txt :justification)
			       (:right (- (g-value txt :width)
					  (cut-string-width cut-string)))
			       (:center
				(floor (- (g-value txt :width)
					  (cut-string-width cut-string))
				       2))
			       (t 0))))))
	  (get-index (g-value txt :string) font
		     (- x (g-value txt :left)))))))
