;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;; 
;; Originally written by meltsner@chipman.crd.ge.com,
;;

;;; $Id$


;;; CHANGE LOG:
;;
;; [2003/11/20:rpg]        - Fred Gilham's fix does not seem to me to work.
;;                           When one specifies depth as a parameter to
;;                           gem:create-image, one can get it wrong, whereas
;;                           gem:create-image will ask the display and get
;;                           a correct number of bits per pixel from the depth.
;; 17-DEC-1999 Fred Gilham - Fix problem where pixmap format doesn't match valid
;;                           pixmap formats in displays with different `depth' and
;;                           `bits-per-pixel' values.
;; 10/20/99 Fred Gilham    - Fixed bug in write-xpm-file that caused it to write
;;                           unnecessarily long xpm files when dumping pixmaps
;;                           with many colors.
;; 09/01/98 Fred Gilham    - Re-wrote write-xpm-file to use new algorithm not
;;                           dependent on display depth.
;; 08/01/98 Fred Gilham    - Fixed problems with depth > 8 bits.
;;                           Write-xpm-file still broken for > 8-bit displays.
;; 9/22/93 Bruno Haible    - CLISP doesn't allow declarations in flet
;; 8/26/93 Andrew Mickish  - Added Reset-Pixmap-Images
;; 6-Apr-93 koz            - Converted with-*-styles macros to set-*-style fns
;;                           And omitted "clip-mask" as argument to draw function.
;; 12/01/92 Andrew Mickish - Added bitmap-p switches so pixmaps will work
;;                           on b/w screens
;; 10/05/92 Martin Sjolin  - Removed CMCUL compiler warnings
;; 09/12/92 Andrew Mickish - Added :xpm-format switch to Write-xpm-File,
;;                           implemented XPM2 output format.
;; 09/10/92 Andrew Mickish - Repaired bugs introduced when adding pedro-format
;; 09/09/92 Pedro Szekely  - Added :format :z-pixmap parameter to
;;                           xlib:get-image call in Window-To-Pximap-Image;
;;                           reimplemented Write-xpm-File with write-char instead of format
;; 08/11/92 Andrew Mickish - Added pedro-format to read-xpm-file
;; 09/16/03 Robert Goldman - XPM files seem to sometime have a colormap entry of "None,"
;;                           causing crash in read-xpm-file.  Modified read-xpm-file to
;;                           ignore these entries.


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(pixmap write-xpm-file read-xpm-file
	    create-pixmap-image window-to-pixmap-image)))

;;    This function was originally written to handle two types of
;;    pixmaps --
;; those of the meltsner-format (XPM2) and those not of the meltsner-format
;; (XPM1).  I added the pedro-format, which is a subset of the meltsner-format
;; (an XPM2 format from the Sun Icon Editor).
;; Non-metlsner (XPM1) format files look like
;;     #define foo_format 1
;;     #define foo_width  32
;;     #define foo_height 32
;;     #define foo_ncolors 4
;;     #define foo_chars_per_pixel 1
;;     static char *foo_colors[] = {
;;        " ", "#FFFFFFFFFFFF",
;;     ...
;; Meltsner-format files look like
;;     /* XPM */
;;     static char * move_3xpm[] = {
;;     "16 16 3 1",
;;     " 	c #FFFFFFFFFFFF",
;;     ...
;; while pedro-format files look like
;;     ! XPM2  
;;     16 16 3 1
;;       c #FFFFFFFFFFFF
;;     ...
;;
;;    Some files have garbage between the color information and the pixel
;; information, and care should be taken to ignore the garbage only after
;; ascertaining that it is definitely not data!  This may be difficult,
;; because spaces that are garbage and spaces that are data tend to look
;; similar.

(defun read-xpm-file (pathname &optional root-window)
  (declare (type (or pathname string stream) pathname))
  (unless root-window
    (setq root-window (g-value DEVICE-INFO :current-root)))
  
  ;; DZG (declare (values xlib:image))
  (with-open-file (fstream pathname :direction :input)
    (let ((line "")
	  (properties nil)
	  (name nil)
	  (name-end nil)
	  (meltsner-format nil)
	  (pedro-format nil))
      ;; DZG (declare				; (type string line)
      ;; DZG  (type xlib:stringable name)
      ;; DZG  (type list properties))
      ;; Get properties
      (loop
       (setq line (read-line fstream))
       (unless (search "XPM" line) (return)))
      (setq meltsner-format (not (eq #\# (aref line 0))))
      ;; The pedro-format does not have a line that begins with "static char"
      (setq pedro-format (not (search "static char" line)))
      (flet ((read-keyword (line start end)
	       (intern
		;; DZG  xlib::kintern
		(substitute
		 #\- #\_
		 (string-upcase
		  (subseq line start end))
		 :test #'char=)
		(find-package "KEYWORD"))))
	(when (null name)
	  (if meltsner-format
	    (if pedro-format
	      ;; In the pedro-format, the line that usually gives you the
	      ;; name of the pixmap does not exist
	      (setq name :untitled)
	      (setq name-end (position #\[ line :test #'char= :from-end t)
		    name (read-keyword line 13 name-end)))
	    (setq name-end (search "_format " line)
		  name (read-keyword line 8 name-end)))
	  (unless (eq name :image)
	    (setf (getf properties :name) name))))

      ;; Calculate sizes
      ;; In the meltsner-format, read until you get to a line beginning with
      ;; a #\".  In the pedro-format, the desired line is already current,
      ;; and #\" characters aren't used anyway.
      (when (and meltsner-format (not pedro-format))
	(loop
	 (when (char= (aref line 0) #\") (return))
	 (setq line (read-line fstream)))
	(setq line (read-from-string line)))
      (let ((bitmap-p (not (g-value color :color-p)))
	    ;; RGA This next will loose on a Mac.  ***TODO:  Fix
	    ;; this.*** 
	    (depth (gem::x-window-depth root-window))
	    width height ncolors left-pad chars-per-pixel)
	;; DZG (declare (type (or null xlib:card16) width height)
	;; DZG	 (type (or null xlib:image-depth) depth)
	;; DZG	 (type (or null xlib:card8) left-pad))
	(if meltsner-format
	  (with-input-from-string (params line)
	    (setq width (read params))
	    (setq height (read params))
	    (setq ncolors  (read params))
	    ;; (setq depth (if bitmap-p 1 depth))
	    (setq chars-per-pixel (read params))
	    (setq left-pad 0))
	  (progn
	    (setq line (read-line fstream))
	    (setq width (read-from-string
			 (subseq line (1+ (position #\space line
						    :from-end t)))))
	    (setq line (read-line fstream))
	    (setq height (read-from-string
			  (subseq line (1+ (position #\space line
						     :from-end t)))))
	    (setq line (read-line fstream))
	    (setq ncolors (read-from-string
			   (subseq line (1+ (position #\space line
						      :from-end t)))))
	    ;; (setq depth (if bitmap-p 1 8))
	    (setq line (read-line fstream))
	    (setq chars-per-pixel (read-from-string
				   (subseq line (1+ (position #\space line
							      :from-end t)))))
	    (setq left-pad 0)))

	(unless (and width height) (error "Not a BITMAP file"))
	(let* ((color-sequence (make-array ncolors))
	       (chars-sequence (make-array ncolors))
	       (bits-per-pixel
		(if bitmap-p 1 (gem::depth-to-bits-per-pixel depth)))
	       (data (gem:create-image-array root-window
					     width height
					     ;; Change: use `bits-per-pixel' when creating
					     ;; the image array because the depth may
					     ;; differ from the bits-per-pixel.  If so, using
					     ;; `depth' here gives an error.
					     bits-per-pixel))
	       (pixel (make-sequence 'string chars-per-pixel)))

	  (flet ((parse-hex (char)
		   (second (assoc char
				  '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
				    (#\4  4) (#\5  5) (#\6  6) (#\7  7)
				    (#\8  8) (#\9  9) (#\a 10) (#\b 11)
				    (#\c 12) (#\d 13) (#\e 14) (#\f 15))
				  :test #'char-equal))))
	    (declare (inline parse-hex))
	    (dotimes (cind ncolors)
	      ;; Eat garbage until we get to a line like
	      ;; " c #FFFFFFFFFFFF...",
	      (loop
	       (setq line (read-line fstream))
	       (when (or (search "\"," line)
			 (and (not (search "static" line))
			      (search "c " line)))
		 (return)))

	      (if meltsner-format
		(progn
		  ;; If not in pedro-format, line is currently a string of
		  ;; a string -- remove one layer of stringness
		  (unless pedro-format
		    (setq line (read-from-string line)))
					;  Got the pixel characters
		  (setf (aref chars-sequence cind)
			(subseq line 0 chars-per-pixel))
		  (setq line
			(subseq line (+ 2 (position #\c line
						    :start chars-per-pixel)))))
		(progn
		  (setf (aref chars-sequence cind) (read-from-string line))
		  (setq line
			(read-from-string
			 (subseq line (1+ (position #\, line)))))))

	      (cond
		((char-equal #\# (aref line 0))
		 (let* ((vals (map 'list #'parse-hex
				   (subseq line 1
					   (position #\space line :start 2))))
			(clength (/ (length vals) 3))
			(divisor (- (expt 16 clength) 1)))

		   (setf (aref color-sequence cind)
			 (gem:colormap-property
			  root-window
			  :ALLOC-COLOR
			  (gem:colormap-property
			   root-window
			   :MAKE-COLOR
			   (/ (let ((accum 0)) ; red
				(dotimes (mm clength accum)
				  (setq accum (+ (* 16 accum) (pop vals)))))
			      divisor)
			   (/ (let ((accum 0)) ; green
				(dotimes (mm clength accum)
				  (setq accum (+ (* 16 accum) (pop vals)))))
			      divisor)
			   (/ (let ((accum 0)) ; blue
				(dotimes (mm clength accum)
				  (setq accum (+ (* 16 accum) (pop vals)))))
			      divisor))))))
		(t
		 (if meltsner-format
		   (setq line (read-from-string line)))
		 ;;		 (break)
		 ;; I don't know why some xpms have a "None" entry in their
		 ;; colormap, but some I got from DOI topographic
		 ;; pictures do.... [2003/09/15:rpg]
		 ;; Original fix broke under SBCL.
		 (unless (search "NONE" line :test #'string-equal)
		   (setf (aref color-sequence cind)
			 (gem:colormap-property
			  root-window
			  :ALLOC-COLOR
			  (gem:colormap-property root-window
						 :LOOKUP-COLOR line)))))))

	    ;; Eat garbage between color information and pixels.
	    ;; Some pixmap files have no garbage, some have a single line
	    ;; of the comment /* pixels */, and non-meltsner-format files
	    ;; have two lines of garbage.
	    (if meltsner-format
	      ;; Only eat the line if it is a /* pixel */ comment
	      (if (char= #\/ (peek-char NIL fstream))
		(setq line (read-line fstream)))
	      (setq line (read-line fstream)
		    line (read-line fstream)))

	    ;; Read data
	    ;; Note: using read-line instead of read-char would be 20% faster,
	    ;;       but would cons a lot of garbage...
	    ;; I'm not sure I should follow the above -- egc might be faster.
	    (dotimes (i height)
	      (if (char= #\" (peek-char NIL fstream))
		(read-char fstream))	;burn quote mark
	      (dotimes (j width)
		(dotimes (k chars-per-pixel)
		  (setf (aref pixel k)
			(read-char fstream)))
		(let* ((value (aref color-sequence
				    (position pixel chars-sequence
					      :test #'string=)))
		       (adjusted-value (if bitmap-p
					 (if (eql value 0) value 1)
					 value)))
		  (setf (aref data i j) adjusted-value)))
	      (read-line fstream)))	;burn junk at end

	  ;; Compensate for left-pad in width and x-hot
	  (unless (zerop left-pad)
	    (decf width left-pad)
	    (decf (getf properties :x-hot) left-pad))
          (gem:create-image root-window width height depth T ; from data
                            data properties bits-per-pixel
                            left-pad data))))))


(defun digit-to-hex (n)
  (code-char (+ (if (< n 10) 48 55) n)))


(defun print-hex (f n)
  (format f "~A~A~A~A" (digit-to-hex (mod (floor n 4096) 16))
		       (digit-to-hex (mod (floor n 256) 16))
		       (digit-to-hex (mod (floor n 16) 16))
		       (digit-to-hex (mod n 16))))



;;;
;; The following info is obtained from scan.c in XPM-3.4k library.
;;
#+sbcl
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))
(#+sbcl define-constant
 #-sbcl defconstant
 +printable+
  ".XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|"
  "Sequence of printable characters; taken from scan.c in XPM-3.4k library.")


(defun write-xpm-file (pixmap pathname &key (xpm-format :xpm2))
  (let ((f (open pathname :direction :output :if-exists :supersede))
	(name (substitute #\_ #\- (pathname-name pathname)))
	(image (g-value pixmap :image))
	(root-window (g-value device-info :current-root)))
    (multiple-value-bind (width height)
	(gem:image-size root-window image)
      (let ((pixarray (gem:image-to-array root-window image))
	    (pixhash (make-hash-table))
	    (max-printable (length +printable+))
	    (ncolors 0))
	
	;; Scan pixmap array to figure out how many distinct colors are in it.
	(dotimes (i height)
	  (dotimes (j width)
	    (unless (gethash (aref pixarray i j) pixhash)
	      ;; Found a new color.
	      (setf (gethash (aref pixarray i j) pixhash) ncolors)
	      (incf ncolors))))

	(let ((strings (make-array ncolors :initial-element nil))
	      (chars-per-color (ceiling (log ncolors max-printable))))

	  ;; Set the strings.
	  (dotimes (i ncolors)
	    (do* ((string (make-array chars-per-color
				      :element-type 'character
				      :initial-element #\Space))
		  (color i (truncate (- color index) (length +printable+)))
		  (index (mod color (length +printable+))
			 (mod color (length +printable+)))
		  (j 0 (1+ j)))
		((= j chars-per-color) (setf (aref strings i) string) nil)
	      (setf (aref string j) (aref +printable+ index))))

	  ;; Write out header.
	  (case xpm-format
	    (:xpm1 (format f "#define ~A_format 1~%" name)
		   (format f "#define ~A_width  ~A~%" name width)
		   (format f "#define ~A_height ~A~%" name height)
		   (format f "#define ~A_ncolors ~A~%" name ncolors)
		   (format f "#define ~A_chars_per_pixel ~A~%" name chars-per-color)
		   (format f "static char *~A_colors[] = {~%" name))
	    (:xpm2 (format f "/* XPM2 C */~%")
		   (format f "static char * ~A[] = {~%" name)
		   (format f "/* ~A pixmap~%" name)
		   (format f " * width height ncolors chars_per_pixel */~%")
		   (format f "\"~A ~A ~A ~A \",~%" width height ncolors chars-per-color)))

	  ;; Write out color entries.
	  (maphash #'(lambda (pixel color)
		       (multiple-value-bind (red green blue)
			   (gem:colormap-property root-window :QUERY-COLORS pixel)
			 (let ((string (aref strings color)))
			   (case xpm-format
			     (:xpm1 (format f "   \"~A\", \"#" string)
				    (print-hex f red)
				    (print-hex f green)
				    (print-hex f blue)
				    (format f "\",~%"))
			     (:xpm2 (format
				     f
				     "\"~A  c #~4,'0X~4,'0X~4,'0X~30,5ts s_~4,'0X~4,'0X~4,'0X \",~%"
				     string red green blue red green blue))))))
		   pixhash)

	  ;; Write out pixels.
	  (case xpm-format
	    (:xpm1 (format f "};~%")
		   (format f "static char *~A_pixels[] = {~%" name))
	    (:xpm2 (format f "/* pixels */~%")))
	  (dotimes (i height)
	    (write-char #\" f)
	    (dotimes (j width)
	      (dotimes (k chars-per-color)
		(write-char (aref (aref strings (gethash (aref pixarray i j) pixhash)) k) f)))
	    (write-char #\" f)
	    (unless (eq i (1- height))
	      (write-char #\, f))
	    (terpri f))
	  (format f "};~%")
	  (close f))))))


(create-instance 'pixmap bitmap
  :declare ((:parameters :left :top :image :line-style :draw-function :visible)
	    ;; Have to recompute the image when you change displays
	    (:maybe-constant T :except :image))
  (:line-style default-line-style)
  (:pixarray (o-formula (if (gvl :image)
			    (gem:image-to-array (gv-local :self :window)
						(gvl :image))))))



(define-method :draw pixmap (gob a-window)
  (let* ((update-vals (get-local-value gob :update-slots-values))
	 (function (aref update-vals +bm-draw-function+))
	 (image (aref update-vals +bm-image+))
	 (line-style (aref update-vals +bm-lstyle+))
	 (left (aref update-vals +bm-left+))
	 (top (aref update-vals +bm-top+)))
    (multiple-value-bind (width height depth)
	(gem:image-size a-window image)
      (if (and image line-style)
	(if (or (= depth 1)
		(g-value color :color-p))
	  (gem:draw-image a-window left top width height image function
			  (aref update-vals +bm-fstyle+))
	  (gem:draw-rectangle a-window left top width height
			      function line-style nil))))))



(defun create-pixmap-image (width height
			    &optional 
			      color
			      (window (g-value device-info :current-root)))
  (let ((depth (gem::x-window-depth window)))
    ;; Passing the display depth as the `bits-per-pixel' optional
    ;; parameter seems to avoid problems of displays where the depth
    ;; of the pixmap formats is different from the bits-per-pixel of
    ;; the pixmap formats, i.e. 24-bit depth but 32-bit
    ;; bits-per-pixel.

    ;; Actually, no, for me at least this CREATES problems.  E.g.,
    ;; this overrides gem:create-image being smart about the number of
    ;; bits per pixel diverging from the number of depth
    ;; planes. [2003/11/20:rpg]
    (gem:create-image window width height depth NIL color NIL)))


;; Creates a pixmap image containing whatever is in the window.
;;
(defun window-to-pixmap-image (window &key left top width height)
  (gem:window-to-image window
		       (or left 0)
		       (or top 0)
		       (or width (g-value window :width))
		       (or height (g-value window :height))))


(defun reset-pixmap-images ()
  (do-all-instances pixmap
    #'(lambda (p)
	(recompute-formula p :image))))

(push #'reset-pixmap-images *auxilliary-reconnect-routines*)
