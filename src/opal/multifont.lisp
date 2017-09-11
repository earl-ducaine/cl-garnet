;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;         The Garnet User Interface Development Environment.        ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;; $Id$
;;


;; Notes to maintainers:
;; A) The multifont-text is an aggregate.  The components of the aggregate
;;    are multifont-lines which draw the strings of the text and one
;;    multifont-cursor which is a line showing the position of the cursor.
;; B) Multifont-line are kept as a linked list within the multifont-text:
;;    the slot, :prev-line, points to the line above and :next-line to the
;;    line below.  Each line contains a single linked list of "fragments."
;; C) A fragment is a structure (frag) that holds the actual strings.  There
;;    is exactly the number of fragments in a line as there are different
;;    fonts.  If two lines are merged and the first and last fragments of
;;    corresponding lines contain the same font (which they must), the
;;    fragments must be merged to form one.  Also, there is not allowed to be
;;    a fragment of zero length with one exception.  If the first character
;;    of a line has a different font than the last character of the line
;;    above, there must be a zero length fragment on that line with the font
;;    from the line above.  The reason for the extra fragment is to make new
;;    characters at the beginning of the line the same font as the character
;;    at the end of the last line.  The procedure calculate-size-of-line is
;;    able to make a line follow the above conventions.
;; D) The state of the cursor has five parts: line, character position,
;;    fragment, fragment position, and x offset.  The cursor is not allowed
;;    to point to the very beginning of a fragment (frag-pos = 0) unless
;;    the cursor is at the very beginning of the line.  If the fragment
;;    position is zero and there exists a previous fragment, the cursor must
;;    be set to point to the last character in the previous fragment.
;; E) The state of the selection pointer consists of four parts: line,
;;    position, fragment, and fragment position.  A selection area is
;;    highlighted by setting the multifont-text's :selection-p to true,
;;    and having the highlight-start and highlight-end components of the
;;    fragments to be nonequal.

;;XXX
;;(declaim (optimize (speed 2) (safety 3) (debug 3)))


(in-package "OPAL")

;;; Moved exports to exports.lisp. FMG

;; Global Variables

(defvar *default-color* nil)
#| DZG
(defvar *default-ascent* (g-value opal:default-font :max-char-ascent))
(defvar *default-descent* (g-value opal:default-font :max-char-descent))
|#
(defvar *delim-chars* '(#\space #\newline #\tab))
(defparameter *Free-Line-List* nil)
(defparameter *Free-Frag-Head* nil)
(defparameter *Free-Mark-Head* nil)

;; TYPE w/ print function

;; FRAG : A fragment of text, with just one font
(defstruct (frag (:print-function print-the-frag))
  object-p        ; whether frag is a string or an object
  object          ; field to hold object
  string
  fcolor
  bcolor
  font
  (length  0 :type fixnum)          ; length of string in characters
  (width   0 :type fixnum)          ; width of string in pixels
  (ascent  0 :type fixnum)
  (descent 0 :type fixnum)
  line-style
  (start-highlight 0 :type fixnum)  ; character position to begin selection highlight
  (end-highlight   0 :type fixnum)  ; position to end highlight
  prev
  next
  break-p         ; T if end-of-line is a break, not a true \newline
)


(defun print-the-frag (frag stream depth)
  (declare (ignore depth))
  (if (frag-object-p frag)
    (format stream "#FRAG<:OBJECT ~A :WIDTH ~A :ASCENT ~A :HIGHLIGHT ~A"
            (frag-object frag) (frag-width frag) (frag-ascent frag)
            (not (= (frag-start-highlight frag) (frag-end-highlight frag))))
    (format stream "#FRAG<\"~A\" :LENGTH ~A :WIDTH ~A :START-H ~A :END-H ~A"
            (frag-string frag) (frag-length frag) (frag-width frag)
            (frag-start-highlight frag) (frag-end-highlight frag)))
  (if (frag-next frag)
    (format stream ">")
    (if (frag-break-p frag)
      (format stream " (BREAK)>")
      (format stream " (EOLN)>"))))


(defstruct (mark (:print-function print-the-mark))
  sticky-left     ; T if mark should stick to left, nil if right.
  name            ; user can specify identifier for mark, or nil if generic
  info            ; user can hold whatever information he wants in mark
  prev
  next
  frag
  line
)


(defun print-the-mark (mark stream depth)
  (declare (ignore depth))
  (format stream "#MARK<\"~A\" STICKY-~A>"
	  (or (mark-name mark) (mark-info mark) "Generic")
	  (if (mark-sticky-left mark) :LEFT :RIGHT)))



;; INSTANCES

(declaim (fixnum +multifont-top+ +multifont-left+
		 +multifont-height+ +multifont-width+
		 +multifont-lstyle+ +multifont-force-update+))
(defconstant +multifont-top+ 2)
(defconstant +multifont-left+ 3)
(defconstant +multifont-height+ 4)
(defconstant +multifont-width+ 5)
(defconstant +multifont-lstyle+ 6)
(defconstant +multifont-force-update+ 9)

;; MULTIFONT-LINE : A single line of text
(create-instance 'MULTIFONT-LINE graphical-object
  :declare ((:type (fixnum :top :left :length :ascent :descent :width :height)))
  (:update-slots '(:visible :fast-redraw-p :top :left :height :width
                   :line-style :draw-function :fill-background-p
		   :force-update :show-marks))
  (:fast-redraw-p (o-formula (gvl :parent :fast-redraw-p)))
  (:fast-redraw-line-style (o-formula (gvl :parent :fast-redraw-line-style)))
  (:fast-redraw-filling-style
                         (o-formula (gvl :parent :fast-redraw-filling-style)))
  (:top (o-formula (let ((prev-line (gvl :prev-line)))
		     (if prev-line
			 (+ (gv-fixnum prev-line :top)
			    (gv-fixnum prev-line :height))
			 (gvl :parent :top)))))
  (:left (o-formula (gvl :parent :left)))
  (:line-style (o-formula (gvl :parent :line-style)))
  (:draw-function (o-formula (gvl :parent :draw-function)))
  (:fill-background-p (o-formula (gvl :parent :fill-background-p)))
  (:show-marks (o-formula (gvl :parent :show-marks)))
  (:force-update NIL)
  (:length 0)
  (:ascent 0)
  (:descent 0)
  (:width 0)
  (:height (o-formula (+ (gvl-fixnum :ascent) (gvl-fixnum :descent))))
  (:first-frag nil)  ; points to beginning of doubly linked list of frags
  (:last-frag nil)
  (:prev-line nil)
  (:next-line nil))

;; MULTIFONT-TEXT : An aggregate of multifont-lines plus a cursor
(create-instance 'MULTIFONT-TEXT aggregate
  :declare ((:parameters :left :top :initial-text :word-wrap-p :text-width
			 :current-font :fill-background-p :draw-function
			 :line-style :show-marks)
	    (:type ((or list string) :initial-text)
		   (kr-boolean :word-wrap-p :fill-background-p)
		   (fixnum :text-width :base-line)
		   (fixnum :cursor-position :cursor-frag-pos :cursor-x-offset)
		   ((or (is-a-p font) (is-a-p font-from-file))
		    :current-font)
		   ((or (is-a-p line-style) null) :line-style)
		   ((member :copy :xor :no-op :or :clear :set :copy-inverted
			    :invert :and :equiv :nand :nor :and-inverted
			    :and-reverse :or-inverted :or-reverse)
		    :draw-function))
	    (:update-slots :word-wrap-p :text-width))
  (:first-line nil)
  (:last-line nil)
  (:first-object nil)			; list of non-multifont objects in text
  (:last-object nil)
  (:first-mark nil)			; list of marks in text
  (:last-mark nil)
  (:show-marks nil)			; set to T if marks should be visible
  (:cursor-line)			; pointer to line that cursor is in
  (:cursor-frag)			; pointer to frag containing the cursor
  (:cursor-position 0)			; character position of cursor within line
  (:cursor-frag-pos 0)			; character position of cursor within frag
  (:cursor-x-offset 0)			; x position of cursor
  (:selection-p nil)			; selection highlight is on or not
  (:select-line)			; line that selection box is on
  (:select-position)			; character position of cursor within line
  (:select-frag)			; frag that selection box is on
  (:select-frag-pos)			; character position of cursor within frag
  (:base-line (o-formula (let ((cursor-line (gvl :cursor-line)))
			   (+ (gv-fixnum cursor-line :top)
			      (gv-fixnum cursor-line :ascent)))))
  (:CURRENT-FONT (o-formula (let ((cursor-frag (gvl :cursor-frag)))
			      (if (frag-object-p cursor-frag)
				  (search-for-font (gvl :cursor-line)
						   cursor-frag)
				  (frag-font cursor-frag)))))
  (:CURRENT-FCOLOR (o-formula (let ((cursor-frag (gvl :cursor-frag)))
				(if (frag-object-p cursor-frag)
				    (nth-value 0
					       (search-for-color (gvl :cursor-line)
								 cursor-frag))
				    (frag-fcolor cursor-frag)))))
  (:CURRENT-BCOLOR (o-formula (let ((cursor-frag (gvl :cursor-frag)))
				(if (frag-object-p cursor-frag)
				    (nth-value 1
					       (search-for-color (gvl :cursor-line)
								 cursor-frag))
				    (frag-bcolor cursor-frag)))))

  (:LEFT 0)
  (:TOP 0)
  (:HEIGHT (o-formula (let ((last-line (gvl :last-line)))
			(+ (- (gv-fixnum last-line :top) (gvl-fixnum :top))
			   (gv-fixnum last-line :height)))))
  (:WIDTH (o-formula (if (gvl :word-wrap-p)
			 (gvl :text-width)
			 (let ((w 0))
			   (declare (fixnum w))
			   (do ((line (gvl :first-line)
				      (g-value line :next-line)))
			       ((null line))
			     (setq w (max w (gv-fixnum line :width))))
			   w))))
  (:INITIAL-TEXT (list ""))
  (:WORD-WRAP-P NIL)
  (:TEXT-WIDTH 300)
  (:DRAW-FUNCTION :COPY)
  (:FILL-BACKGROUND-P T)
  (:LINE-STYLE (o-formula (let ((parent-win (gvl :window)))
			    (if parent-win
				(let ((bgc (gv parent-win :background-color)))
				  (if bgc
				      (create-instance nil line-style
					(:background-color
					 bgc))
				      DEFAULT-LINE-STYLE))
				DEFAULT-LINE-STYLE))))
  (:do-not-dump-objects :me)
  )


;; MULTIFONT-TEXT-CURSOR : Cursor for multifont-text
(create-instance 'MULTIFONT-TEXT-CURSOR opal:rectangle
  :declare ((:type (fixnum :ascent :descent :top :left :width :height)))
  (:update-slots '(:visible :fast-redraw-p :top :left :width :height
		   :line-style :filling-style :draw-function :force-update))
  (:draw-function :xor)
  (:filling-style opal:black-fill)
  (:line-style nil)
  (:fast-redraw-p T)
  (:visible nil)
  (:ascent (o-formula
	    (let* ((parent (gvl :parent))
		   (cursor-frag (gv parent :cursor-frag)))
	      (if (frag-object-p cursor-frag)
		  (frag-ascent cursor-frag)
		  (min (gv-fixnum parent :current-font :max-char-ascent)
		       (gv-fixnum parent :cursor-line :ascent))))))
  (:descent (o-formula
	     (let* ((parent (gvl :parent))
		    (cursor-frag (gv parent :cursor-frag)))
	       (if (frag-object-p cursor-frag)
		   (frag-descent cursor-frag)
		   (min (gv-fixnum parent :current-font :max-char-descent)
			(gv-fixnum parent :cursor-line :descent))))))
  (:top (o-formula (- (gvl-fixnum :parent :base-line) (gvl-fixnum :ascent))))
  (:left (o-formula (let ((parent (gvl :parent)))
		      (+ (gv-fixnum parent :left)
			 (gv-fixnum parent :cursor-x-offset)))))
  (:width 2)
  (:height (o-formula (+ (gvl-fixnum :ascent) (gvl-fixnum :descent)))))


(s-value MULTIFONT-TEXT :do-not-dump-slots
	  (append '(:cursor :cursor-frag :cursor-line :first-line :last-line
		    :select-line :select-frag :first-object :last-object
		    :first-mark :last-mark)
		  (g-value MULTIFONT-TEXT :do-not-dump-slots)))

;;; Helper Functions for Methods

;; This function will check an input in the :strings format to see
;; whether or not it is syntactically correct.
(defun check-text (text)
  (if (listp text)
      (if (cdr (last text))
	  (error "Input text must be a string or a list of lines.")
	  (let ((lineno 0)
		(fragno 0))
	    (declare (fixnum lineno fragno))
	    (dolist (line text)
	      (incf lineno)
	      (if (listp line)
		  (if (cdr (last line))
		      (error "Line ~s must be a string, a view-object, or a list of fragments, but it was ~S" lineno line)
		      (progn
			(setq fragno 0)
			(dolist (frag line)
			  (incf fragno)
			  (unless
			      (or (stringp frag) (is-a-p frag opal:view-object)
				  (mark-p frag) (eq (car frag) :mark)
				  (and (stringp (car frag))
				       (let ((specs-list (cdr frag)))
					 (if (listp specs-list)
					     (and
					      (let ((font (first specs-list)))
						(or (is-a-p font opal:font)
						    (is-a-p font opal:font-from-file)))
					      (let ((fcolor (second specs-list)))
						(or (null fcolor)
						    (and (is-a-p fcolor opal:color)
							 (let ((bcolor (third specs-list)))
							   (or (null bcolor)
							       (is-a-p bcolor
								       opal:color)))))))
					     (or (is-a-p specs-list opal:font)
						 (is-a-p specs-list opal:font-from-file))))))
			    (error "Fragment ~s of line ~s must be a string, a view-object, a mark, a cons of a string with a font, or a list of a string, font, and (optional) colors, but it was ~S"
				   fragno lineno frag)))))
		  (unless (or (stringp line) (is-a-p line opal:view-object))
		    (error "Line ~s must be a string, a view-object, or a list of fragments, but it was ~S."
			   lineno line))))))
      (unless (stringp text)
	(error "Input text must be a string or a list of lines.")))
  nil)


;;; XXX These resource lists are thread-unsafe.

;; If the *Free-Line-List* is non-nil, a line is fetched from it to be used
;; as a new line.  Otherwise create-instance is used to generate a new line.
;;(defun new-line ()
;;  (if *Free-Line-List*
;;    (pop *Free-Line-List*)
;;    (create-instance nil MULTIFONT-LINE)))

(defun new-line ()
  (create-instance nil MULTIFONT-LINE))

;; Fetches a fragment from the *Free-Frag-Head* list.  If the list is nil then
;; a new fragment is generated.
(defun new-frag ()
  (make-frag))

;;(defun new-frag ()
;;  (if *Free-Frag-Head*
;;    (let ((val *Free-Frag-Head*))
;;      (setf *Free-Frag-Head* (frag-next *Free-Frag-Head*))
;;      (setf (frag-object val) nil)
;;      val)
;;    (make-frag)))


;; Use this to add a fragment to the free list.
(defun free-frag (frag)
  (declare (ignore frag)))

;;(defun free-frag (frag)
;;  (setf (frag-next frag) *Free-Frag-Head*)
;;  (setf *Free-Frag-Head* frag))


;; Use this to add a group of fragments to the free list.
(defun free-frag-line (first-frag last-frag)
  (declare (ignore first-frag last-frag))
  )

;;(defun free-frag-line (first-frag last-frag)
;;  (when (and first-frag last-frag)
;;    (setf (frag-next last-frag) *Free-Frag-Head*)
;;    (setf *Free-Frag-Head* first-frag)))


;; Fetches a mark from the *Free-Mark-Head* list.  If the list is nil then
;; a new mark is generated.
(defun new-mark ()
  (make-mark))

;;(defun new-mark ()
;;  (if *Free-Mark-Head*
;;    (let ((val *Free-Mark-Head*))
;;      (setf *Free-Mark-Head* (mark-next *Free-Mark-Head*))
;;      val)
;;    (make-mark)))


;; Use this to add a mark to the free list.
(defun free-mark (mark)
  (declare (ignore mark)))

;;(defun free-mark (mark)
;;  (setf (mark-next mark) *Free-Mark-Head*)
;;  (setf *Free-Mark-Head* mark))



;; Use this to add a group of marks to the free list.
(defun free-mark-line (first-mark last-mark)
  (declare (ignore first-mark last-mark)))

;;(defun free-mark-line (first-mark last-mark)
;;  (when (and first-mark last-mark)
;;    (setf (mark-next last-mark) *Free-Mark-Head*)
;;    (setf *Free-Mark-Head* first-mark)))


;; Removes the line from its containing aggregate.  Puts it into the
;; *Free-Line-List* for potential later use.
(defun destroy-line (my-line)
  (opal:remove-component (g-value my-line :parent) my-line)
  (free-frag-line (g-value my-line :first-frag) (g-value my-line :last-frag))
  ;; XXX thread unsafe
  #-(and)(push my-line *Free-Line-List*))


;; Turns string str into a list of strings, split up at #\newlines.
(defun break-at-newlines (str)
  (declare (simple-string str))
  (let ((ans nil))
    (do ((pos (position #\newline str :from-end t :test #'eql)
	      (position #\newline str :from-end t :test #'eql)))
	((null pos)
	 ;; Add space to first string as well.
	 (push (concatenate 'string str " ") ans))
      (push (concatenate 'string (subseq str (1+ pos)) " ") ans)
      (setq str (subseq str 0 pos)))))


;; Locates all spaces in the line that can be used to word wrap.  Returns a
;; list of pairs (position width) indicating the character position and pixel
;; position of each space.  Returned list is in "reversed" order ie. the
;; higher widths are returned first.
(defun find-spaces (my-line)
  (let ((output nil)
	(width 0)
	(my-position 0)
	(spc nil)
	(spc-spc nil)
	char)
    (declare (fixnum width my-position))
    (do ((frag (g-value my-line :first-frag) (frag-next frag)))
	((null frag) output)
      (if (frag-object-p frag)
	(when (not (mark-p (frag-object frag)))
	  (when spc
	    (push (list my-position width) output)
	    (setq spc nil spc-spc nil))
	  (incf my-position)
	  (incf width (frag-width frag)))
	(dotimes (i (frag-length frag))
	  (setq char (schar (frag-string frag) i))
	  (if (eq #\space char)
	    (if spc
	      (if spc-spc
		(push (list my-position width) output)
		(setq spc-spc t))
	      (setq spc t))
	    (when spc
	      (push (list my-position width) output)
	      (setq spc nil spc-spc nil)))
	  (incf my-position)
	  (incf width (the fixnum (opal:char-width (frag-font frag) char))))))))


;; Return the character position to break the line.  The break must occur such
;; that the width of the left part must be less than or equal to parameter,
;; width.
(defun width-break (width my-line)
  (declare (fixnum width))
  (let ((my-position 0)
	(accum 0)
	cut-frag)
    (declare (fixnum my-position accum))
    (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	  (f-width (frag-width frag) (frag-width frag)))
	 ((> (+ accum f-width) width) (setq cut-frag frag))
      (incf my-position (frag-length frag))
      (incf accum f-width))
    (max 2 (if (not (frag-object-p cut-frag))
	     (let ((font (frag-font cut-frag))
		   (string (frag-string cut-frag)))
	       (do ((i 0 (1+ i)))
		   ((> accum width) (1- my-position))
		 (incf accum (the fixnum (opal:char-width font (schar string i))))
		 (incf my-position)))
	     my-position))))


;; Return the character position to break the line.  The break could be
;; contained in the spec which is a list of (position width) pairs
;; (see function find-spaces).
(defun find-wrap (width my-line spec)
  (declare (fixnum width))
  (do ((pair (pop spec) (pop spec)))
      ((or (null pair) (< (the fixnum (cadr pair)) width))
       (if pair
	 (car pair)
	 (width-break width my-line)))))


;; Compares the fonts of the fragments along with the line-style and color,
;; and whether or not either frag is an object.
(defun fonts-equal-p (frag1 frag2)
  (and (not (frag-object-p frag1))
       (not (frag-object-p frag2))
       (eq (frag-font frag1) (frag-font frag2))
       (eq (frag-fcolor frag1) (frag-fcolor frag2))
       (eq (frag-bcolor frag1) (frag-bcolor frag2))
       (eq (frag-line-style frag1) (frag-line-style frag2))))


;; Merge first-frag into second-frag if fonts & colors are equal.
;; Return second frag if successful; otherwise, return first-frag.
(defun merge-frags (my-line first-frag second-frag)
  (cond
    ((and (zerop (frag-length first-frag))
	  (not (mark-p (frag-object first-frag))))
     (let ((prev-frag (frag-prev first-frag)))
       (setf (frag-prev second-frag) prev-frag)
       (if prev-frag
	 (setf (frag-next prev-frag) second-frag)
	 (s-value my-line :first-frag second-frag)))
     second-frag)
    ((zerop (frag-length second-frag))
     (setf (frag-object-p second-frag) (frag-object-p first-frag))
     (setf (frag-start-highlight second-frag)
	   (frag-start-highlight first-frag))
     (setf (frag-end-highlight second-frag) (frag-end-highlight first-frag))
     (setf (frag-width second-frag) (frag-width first-frag))
     (setf (frag-ascent second-frag) (frag-ascent first-frag))
     (setf (frag-descent second-frag) (frag-descent first-frag))
     (let ((prev-frag (frag-prev first-frag)))
       (setf (frag-prev second-frag) prev-frag)
       (if prev-frag
	 (setf (frag-next prev-frag) second-frag)
	 (s-value my-line :first-frag second-frag)))
     (if (frag-object-p first-frag)
       (progn
	 (let ((obj (frag-object first-frag)))
	   (setf (frag-object second-frag) obj)
	   (setf (frag-length second-frag) (if (mark-p obj) 0 1)))
	 (setf (frag-font second-frag) NIL)
	 (setf (frag-fcolor second-frag) NIL)
	 (setf (frag-bcolor second-frag) NIL))
       (progn
	 (setf (frag-string second-frag) (frag-string first-frag))
	 (setf (frag-length second-frag) (frag-length first-frag))
	 (setf (frag-font second-frag) (frag-font first-frag))
	 (setf (frag-fcolor second-frag) (frag-fcolor first-frag))
	 (setf (frag-bcolor second-frag) (frag-bcolor first-frag))))
     second-frag)
    ((fonts-equal-p first-frag second-frag)
     (let ((prev-frag (frag-prev first-frag)))
       (setf (frag-prev second-frag) prev-frag)
       (if prev-frag
	 (setf (frag-next prev-frag) second-frag)
	 (s-value my-line :first-frag second-frag)))
     (setf (frag-string second-frag)
	   (concatenate 'string
			(the simple-string (frag-string first-frag))
			(the simple-string (frag-string second-frag))))
     (incf (frag-length second-frag) (frag-length first-frag))
     (incf (frag-width second-frag) (frag-width first-frag))
     (setf (frag-prev first-frag) nil)
     (setf (frag-next first-frag) nil)
     (cond
       ((= (frag-start-highlight second-frag)
	   (frag-end-highlight second-frag))
	(setf (frag-start-highlight second-frag)
	      (frag-start-highlight first-frag))
	(setf (frag-end-highlight second-frag)
	      (frag-end-highlight first-frag)))
       ((= (frag-start-highlight first-frag)
	   (frag-end-highlight first-frag))
	(incf (frag-start-highlight second-frag)
	      (frag-length first-frag))
	(incf (frag-end-highlight second-frag)
	      (frag-length first-frag)))
       (T
	(setf (frag-start-highlight second-frag)
	      (frag-start-highlight first-frag))
	(incf (frag-end-highlight second-frag)
	      (frag-length first-frag))))
     (free-frag first-frag)
     second-frag)
    (T
     (setf (frag-next first-frag) second-frag)
     (setf (frag-prev second-frag) first-frag)
     first-frag)))


;; Splits a frag into two pieces, the first (old) being left-frag
;; and the second (new) being right-frag.  Returns right-frag.
(defun split-frag (left-frag cursor-sub-index)
  (declare (fixnum cursor-sub-index))
  (let ((right-frag (new-frag)))
    (setf (frag-line-style right-frag) NIL)
    (let ((next (frag-next left-frag)))
      (setf (frag-next right-frag) next)
      (if next
	(setf (frag-prev next) right-frag)))
    (setf (frag-next left-frag) nil)
    (setf (frag-prev right-frag) nil)
    (setf (frag-break-p right-frag) (frag-break-p left-frag))
    (if (frag-object-p left-frag)
      (progn
	(if (zerop cursor-sub-index)
	  (progn
	    (s-value (frag-object left-frag) :multifont-frag right-frag)
	    (setf (frag-length left-frag) 0)
	    (setf (frag-object-p right-frag) T)
	    (let ((obj (frag-object left-frag)))
	      (setf (frag-object right-frag) obj)
	      (setf (frag-length right-frag) (if (mark-p obj) 0 1)))
	    (setf (frag-font right-frag) NIL)
	    (setf (frag-fcolor right-frag) NIL)
	    (setf (frag-bcolor right-frag) NIL)
	    (setf (frag-width right-frag) (frag-width left-frag))
	    (setf (frag-ascent right-frag) (frag-ascent left-frag))
	    (setf (frag-descent right-frag) 0)
	    (setf (frag-start-highlight right-frag)
		  (frag-start-highlight left-frag))
	    (setf (frag-end-highlight right-frag)
		  (frag-end-highlight left-frag)))
	  (setf (frag-length right-frag) 0)))
      (progn
	(setf (frag-object-p right-frag) NIL)
	(setf (frag-font right-frag) (frag-font left-frag))
	(setf (frag-fcolor right-frag) (frag-fcolor left-frag))
	(setf (frag-bcolor right-frag) (frag-bcolor left-frag))
	(setf (frag-ascent right-frag) (frag-ascent left-frag))
	(setf (frag-descent right-frag) (frag-descent left-frag))

	(setf (frag-string right-frag)
	      (subseq (the simple-string (frag-string left-frag)) cursor-sub-index))
	(setf (frag-string left-frag)
	      (subseq (the simple-string (frag-string left-frag)) 0 cursor-sub-index))
	(setf (frag-length right-frag) (- (frag-length left-frag)
					  cursor-sub-index))
	(setf (frag-length left-frag) cursor-sub-index)
	(setf (frag-width right-frag)
	      (opal:string-width (frag-font right-frag)
				 (frag-string right-frag)))
	(decf (frag-width left-frag) (frag-width right-frag))

	(cond
	  ((= (frag-start-highlight left-frag) (frag-end-highlight left-frag))
	   (setf (frag-start-highlight right-frag) 0)
	   (setf (frag-end-highlight right-frag) 0))
	  ((< (frag-start-highlight left-frag) cursor-sub-index)
	   (setf (frag-start-highlight right-frag) 0)
	   (if (<= (frag-end-highlight left-frag) cursor-sub-index)
	     (setf (frag-end-highlight right-frag) 0)
	     (progn
	       (setf (frag-end-highlight right-frag)
		     (- (frag-end-highlight left-frag) cursor-sub-index))
	       (setf (frag-end-highlight left-frag) cursor-sub-index))))
	  (T
	   (setf (frag-start-highlight right-frag)
		 (- (frag-start-highlight left-frag) cursor-sub-index))
	   (setf (frag-end-highlight right-frag)
		 (- (frag-end-highlight left-frag) cursor-sub-index))
	   (setf (frag-start-highlight left-frag) 0)
	   (setf (frag-end-highlight left-frag) 0)))))
    right-frag))

;; Determine all attributes (other than fragments) of the given line
;; by running through all of its constituent fragments.  Remove zero
;; length fragments from line (except for the first fragment which is
;; a special case)
(defun calculate-size-of-line (gob my-line)
  (let ((length 0)
	(width 0)
	(ascent 0)
	(descent 0)
	(cursor-frag (g-value gob :cursor-frag))
	prev-frag next-frag)
    (declare (fixnum length width ascent descent))
    (do ((frag (g-value my-line :first-frag) next-frag))
	((null frag))
      (setq prev-frag (frag-prev frag))
      (setq next-frag (frag-next frag))
      (if (and (zerop (frag-length frag)) ;**********2
	       (not (frag-object-p frag))
	       (not (and prev-frag next-frag
			 (mark-p (frag-object prev-frag))
			 (mark-p (frag-object next-frag))))
	       (not (and (not prev-frag) next-frag (frag-object-p next-frag))))
	(progn
	  (if prev-frag
	    (progn
	      (setf (frag-break-p prev-frag) (frag-break-p frag))
	      (setf (frag-next prev-frag) next-frag)
	      (let ((my-frag prev-frag)
		    (my-pos (frag-length prev-frag))
		    (prev-object (frag-object prev-frag)))
		(when (eq frag cursor-frag)
		  (when (mark-p prev-object)
		    (if (mark-sticky-left prev-object)
		      (do ((obj prev-object
				(and my-frag (frag-object my-frag))))
			  ((not (mark-p obj)))
			(setq my-frag (frag-next my-frag))
			(setq my-pos 0))
		      (do ((obj prev-object
				(and my-frag (frag-object my-frag))))
			  ((not (mark-p obj))
			   (setq my-pos (frag-length my-frag)))
			(setq my-frag (frag-prev my-frag)))))
		  (s-value gob :cursor-frag my-frag)
		  (s-value gob :cursor-frag-pos my-pos))))
	    (progn
	      (s-value my-line :first-frag next-frag)
	      (when (eq frag cursor-frag)
		(do* ((my-frag next-frag (frag-next my-frag))
		      (obj (and my-frag (frag-object my-frag))
			   (and my-frag (frag-object my-frag))))
		     ((not (mark-p obj))
		      (s-value gob :cursor-frag my-frag)
		      (s-value gob :cursor-frag-pos 0))))))
	  (if next-frag
	    (setf (frag-prev next-frag) prev-frag)
	    (s-value my-line :last-frag prev-frag))
	  (free-frag frag))
	(progn
	  (when (frag-object-p frag)
	    (let ((obj (frag-object frag)))
	      (if (mark-p obj)
		(setf (mark-line obj) my-line)
		(progn
		  (s-value obj :multifont-x-offset width)
		  (s-value obj :multifont-line my-line)))))
	  (incf length (frag-length frag))
	  (incf width (frag-width frag))
	  (when (and prev-frag (fonts-equal-p prev-frag frag))
	    (if (eq cursor-frag prev-frag)
		(setq cursor-frag (s-value gob :cursor-frag frag))
		(when (eq cursor-frag frag)
		  (incf (g-value-fixnum gob :cursor-frag-pos)
			(frag-length prev-frag))))
	    (merge-frags my-line prev-frag frag))
	  (setq ascent (max (frag-ascent frag) ascent))
	  (setq descent (max (frag-descent frag) descent)))))
    (s-value my-line :length length)
    (s-value my-line :width width)
    (s-value my-line :ascent ascent)
    (s-value my-line :descent descent)))

;; This returns necessary computations to update the position of the
;; cursor.  Given the line and character offset of the cursor, this
;; will return the fragment, fragment offset, and pixel offset.
(defun calculate-cursor-pos (my-line my-position)
  (declare (fixnum my-position))
  (let ((frag-offset my-position)
	(x-offset 0)
	cursor-frag)
    (declare (fixnum frag-offset x-offset))
    (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	  (length (and frag (frag-length frag)) (and frag (frag-length frag))))
	 ((or (null frag) (>= length frag-offset))
	  (setq cursor-frag frag)
	  (if (frag-object-p cursor-frag)
	    (if (mark-p (frag-object cursor-frag))
	      (do* ((my-frag (frag-next cursor-frag) (frag-next my-frag))
		    (obj (frag-object my-frag) (frag-object my-frag)))
		   ((not (mark-p obj))
		    (setq cursor-frag my-frag)
		    (setq frag-offset 0)))
	      (unless (zerop frag-offset)
		(incf x-offset (frag-width cursor-frag))))
	    (incf x-offset
		  (the fixnum (opal:string-width
			       (frag-font cursor-frag)
			       (subseq (the simple-string (frag-string cursor-frag))
				       0 frag-offset)))))
	  (when (= length frag-offset)
	    (let* ((next-frag (frag-next frag))
		   (mark (and next-frag (frag-object next-frag))))
	      (when (and (mark-p mark) (mark-sticky-left mark))
		(do* ((my-frag (frag-next next-frag) (frag-next my-frag))
		      (obj (frag-object my-frag) (frag-object my-frag)))
		     ((not (mark-p obj))
		      (setq cursor-frag my-frag)
		      (setq frag-offset 0)))))))
      (decf frag-offset length)
      (incf x-offset (frag-width frag)))
    (values cursor-frag frag-offset x-offset)))


;; Break the line at the given character position.  The cursor position is
;; changed correctly if it is on the line.  The parameter break-p is used to
;; fill the break-p slot in the broken frag.
;;
(defun break-line (gob my-line my-position break-p)
  (declare (fixnum my-position))
  (let ((length my-position)
	(next-line (g-value my-line :next-line))
	cut-frag
	new-frag
	new-line
	(last-frag (g-value my-line :last-frag)))
    (declare (fixnum length))

    (if (frag-break-p last-frag)

	(progn
	  (merge-frags my-line last-frag (g-value next-line :first-frag))
	  (let ((my-length (g-value my-line :length)))
	    (declare (fixnum my-length))
	    (when (eq next-line (g-value gob :cursor-line))
	      (s-value gob :cursor-line my-line)
	      (incf (g-value-fixnum gob :cursor-position) my-length))
	    (when (and (g-value gob :selection-p)
		       (eq next-line (g-value gob :select-line)))
	      (s-value gob :select-line my-line)
	      (incf (g-value-fixnum gob :select-position) my-length)))
	  (setq new-line next-line))

	(progn
	  (setq new-line (new-line))
	  (s-value new-line :prev-line my-line)
	  (s-value new-line :next-line next-line)
	  (if next-line
	      (s-value next-line :prev-line new-line)
	      (s-value gob :last-line new-line))
	  (s-value my-line :next-line new-line)
	  (opal:add-component gob new-line :behind (g-value gob :cursor))
	  (s-value new-line :last-frag last-frag)))

    (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	  (length-frag (frag-length frag) (frag-length frag)))
	 ((> length-frag length) (setq cut-frag frag))
      (decf length length-frag))

    (if (zerop length)

	(let ((prev-frag (frag-prev cut-frag)))
	  (if prev-frag

	      (progn
		(do ((object (frag-object prev-frag) (frag-object prev-frag)))
		    ((not (and (frag-object-p prev-frag) (mark-p object)
			       (not (mark-sticky-left object)))))
		  (setq cut-frag prev-frag)
		  (setq prev-frag (frag-prev prev-frag)))
		(setf (frag-next prev-frag) NIL)
		(setf (frag-break-p prev-frag) break-p))

	      (s-value my-line :first-frag NIL))

	  (s-value new-line :first-frag cut-frag)
	  (s-value my-line :last-frag prev-frag)
	  (setf (frag-prev cut-frag) NIL))

	(progn
	  (setq new-frag (split-frag cut-frag length))
	  (s-value new-line :first-frag new-frag)
	  (s-value my-line :last-frag cut-frag)
	  (setf (frag-break-p cut-frag) break-p)

	  (let ((next-frag (frag-next new-frag)))
	    (if next-frag
		(setf (frag-prev next-frag) new-frag)
		(s-value new-line :last-frag new-frag)))))

    (when (g-value my-line :first-frag)
      (calculate-size-of-line gob my-line))

    (calculate-size-of-line gob new-line)

    (when (eq my-line (g-value gob :cursor-line))
      (when (<= my-position (g-value-fixnum gob :cursor-position))
	(decf (g-value-fixnum gob :cursor-position) my-position)
	(s-value gob :cursor-line new-line))
      (multiple-value-bind (frag frag-pos x-offset)
	  (calculate-cursor-pos (g-value gob :cursor-line)
				(g-value gob :cursor-position))
	(s-value gob :cursor-frag frag)
	(s-value gob :cursor-frag-pos frag-pos)
	(s-value gob :cursor-x-offset x-offset)))

    (when (and (g-value gob :selection-p)
	       (eq my-line (g-value gob :select-line)))
      (when (<= my-position (g-value-fixnum gob :select-position))
	(decf (g-value-fixnum gob :select-position) my-position)
	(s-value gob :select-line new-line))
      (multiple-value-bind (frag frag-pos)
	  (calculate-cursor-pos (g-value gob :select-line)
				(g-value gob :select-position))
	(s-value gob :select-frag frag)
	(s-value gob :select-frag-pos frag-pos)))))


;; If line is too long, wrap excess onto next line.
(defun wrap-line (gob my-line)
  (if my-line
      (let ((width (g-value gob :text-width)))
	(declare (fixnum width))
	(when (> (g-value my-line :width) width)
	  (break-line gob my-line
		      (find-wrap width my-line (find-spaces my-line)) t)
	  (wrap-line gob (g-value my-line :next-line))))))


;; Break line at newlines, and put a space at the end of the line.
(defun add-space-to-line (my-line)
  (let ((ans nil))
    (cond

      ((stringp my-line)
       (dolist (sub-my-line (break-at-newlines my-line) (reverse ans))
	 (push (append (list sub-my-line) (list (list " "  opal:default-font
						      *default-color* *default-color*)))
	       ans)))

      ((is-a-p my-line opal:view-object)
       (push (append (list my-line) (list (list " " opal:default-font
						*default-color* *default-color*)))
	     ans))

      (T
       (let ((frags nil)
	     (len (length my-line)))
	 (declare (fixnum len))
	 (dotimes (i len
		   (reverse ans))
	   (let* ((frag (elt my-line  i))
		  (has-specs (listp frag))
		  (string (if has-specs (car frag) frag))
		  (specs-list (and has-specs (cdr frag)))
		  (true-list (and (stringp string)
				  specs-list (listp specs-list)))
		  (last-font (if has-specs
				 (if true-list
				     (second frag)
				     specs-list)
				 opal:default-font))
		  (last-fcolor (if true-list (third frag) *default-color*))
		  (last-bcolor (if true-list (fourth frag)  *default-color*)))

	     (if (stringp string)

		 (progn
		   (dolist (sub-my-line (break-at-newlines string))
		     (push (append (pop frags)
				   (list (list sub-my-line last-font
					       last-fcolor last-bcolor)))
			   ans))
		   (push (pop ans) frags))

		 (progn
		   (when (eq string :mark)
		     (let ((mark (new-mark)))
		       (setf (mark-sticky-left mark) (first specs-list))
		       (setf (mark-name mark) (second specs-list))
		       (setf (mark-info mark) (third specs-list))
		       (setq string mark)))
		   (push (append (pop frags) (list string)) frags)))

	     (when (= i (1- len))
	       (push (append
		      (pop frags)
		      (list (list " " last-font last-fcolor last-bcolor)))
		     ans)))))))))


;; Put a space at the end of every line.  The space represents a newline.
(defun add-spaces (text)
  (let ((ans nil))
    (dolist (my-line text)
      (dolist (sub-my-line (add-space-to-line my-line))
	(push sub-my-line ans)))
    (reverse ans)))


;; Puts the whole text into a completely empty multifont object.  This is used
;; for initialization and the function SET-STRINGS.
(defun install-text (gob text)
  (let ((cursor (g-value gob :cursor))
	(prev-line nil) prev-frag  (prev-object nil)
	(prev-mark nil) new-line   new-frag
	substring font fcolor bcolor)
    (cond
      ((stringp text) (setq text (add-space-to-line text)))
      ((atom text)    (setq text (list " ")))
      (T              (setq text (add-spaces text))))
    (dolist (my-line text)
      (setq new-line (new-line))
      (opal:add-component gob new-line :behind cursor)
      (if prev-line
	(s-value prev-line :next-line new-line)
	(s-value gob :first-line new-line))
      (s-value new-line :prev-line prev-line)
      (setq prev-line new-line)
      (setq prev-frag nil)
      (dolist (frag (if (listp my-line) my-line (list my-line)))
	(setq new-frag (new-frag))
	(setf (frag-line-style new-frag) NIL)
	(setf (frag-start-highlight new-frag) 0)
	(setf (frag-end-highlight new-frag) 0)
	(setf (frag-break-p new-frag) nil)
	(setf (frag-next new-frag) nil)
	(if prev-frag
	  (if (let ((prev-object (frag-object prev-frag)))
		(and (mark-p prev-object)
		     (mark-sticky-left prev-object)
		     (mark-p frag)
		     (not (mark-sticky-left frag))))
	    (let ((last-frag (or (frag-prev prev-frag)
				 (let ((prev-line
					(g-value new-line :prev-line)))
				   (when prev-line
				     (g-value prev-line :last-frag)))))
		  font fcolor bcolor)
	      (if last-frag
		(setq font (frag-font last-frag)
		      fcolor (frag-fcolor last-frag)
		      bcolor (frag-bcolor last-frag))
		(setq font opal:default-font
		      fcolor *default-color*
		      bcolor *default-color*))
	      (new-frag-with-font font fcolor bcolor
				  new-line prev-frag new-frag))
	    (progn
	      (setf (frag-next prev-frag) new-frag)
	      (setf (frag-prev new-frag) prev-frag)))
	  (if (or (is-a-p frag opal:view-object) (mark-p frag))
	    (let* ((prev-line (g-value new-line :prev-line))
		   (last-frag (and prev-line (g-value prev-line :last-frag)))
		   font fcolor bcolor)
	      (if last-frag
		(setq font (frag-font last-frag)
		      fcolor (frag-fcolor last-frag)
		      bcolor (frag-bcolor last-frag))
		(setq font opal:default-font
		      fcolor *default-color*
		      bcolor *default-color*))
	      (new-frag-with-font font fcolor bcolor
				  new-line prev-frag new-frag))
	    (progn
	      (s-value new-line :first-frag new-frag)
	      (setf (frag-prev new-frag) prev-frag))))
	(setq prev-frag new-frag)
	(cond
	  ((is-a-p frag opal:view-object)
	   (setf (frag-object-p new-frag) T)
	   (setf (frag-object new-frag) frag)
	   (setf (frag-font new-frag) nil)
	   (setf (frag-fcolor new-frag) nil)
	   (setf (frag-bcolor new-frag) nil)
	   (setf (frag-length new-frag) 1)
	   (setf (frag-width new-frag) (g-value frag :width))
	   (setf (frag-ascent new-frag) (g-value frag :height))
	   (setf (frag-descent new-frag) 0)
	   (opal:add-component gob frag :back)
	   (s-value frag :multifont-object gob)
	   (s-value frag :multifont-line new-line)
	   (s-value frag :multifont-frag new-frag)
	   (s-value frag :top (o-formula (gvl :multifont-line :top)))
	   (s-value frag :left (o-formula (+ (gvl-fixnum :multifont-line :left)
					     (gvl-fixnum :multifont-x-offset))))
	   (s-value frag :mf-prev-object prev-object)
	   (if prev-object
	     (s-value prev-object :mf-next-object frag)
	     (s-value gob :first-object frag))
	   (setq prev-object frag))
	  ((mark-p frag)
	   (setf (frag-object-p new-frag) T)
	   (setf (frag-object new-frag) frag)
	   (setf (frag-font new-frag) nil)
	   (setf (frag-fcolor new-frag) nil)
	   (setf (frag-bcolor new-frag) nil)
	   (setf (frag-length new-frag) 0)
	   (setf (frag-width new-frag) 0)
	   (setf (frag-ascent new-frag) 0)
	   (setf (frag-descent new-frag) 0)
	   (setf (mark-frag frag) new-frag)
	   (setf (mark-line frag) new-line)
	   (setf (mark-prev frag) prev-mark)
	   (if prev-mark
	     (setf (mark-next prev-mark) frag)
	     (s-value gob :first-mark frag))
	   (setq prev-mark frag))
	  (T
	   (setf (frag-object-p new-frag) NIL)
	   (if (stringp frag)
	     (setq substring frag
		   font opal:default-font
		   fcolor *default-color*
		   bcolor *default-color*)
	     (progn
	       (setq substring (car frag))
	       (let ((specs-list (cdr frag)))
		 (if (listp specs-list)
		   (setq font (or (second frag) opal:default-font)
			 fcolor (or (third frag) *default-color*)
			 bcolor (or (fourth frag) *default-color*))
		   (setq font specs-list
			 fcolor *default-color*
			 bcolor *default-color*)))))
	   (setf (frag-string new-frag) substring)
	   (setf (frag-length new-frag) (length substring))
	   (setf (frag-font new-frag) font)
	   (setf (frag-fcolor new-frag) fcolor)
	   (setf (frag-bcolor new-frag) bcolor)
	   (setf (frag-width new-frag)
		 (opal:string-width font substring
				    :display (g-value gob :window)))
	   (setf (frag-ascent new-frag) (g-value font :max-char-ascent))
	   (setf (frag-descent new-frag) (g-value font :max-char-descent)))))
      (s-value new-line :last-frag new-frag)
      (calculate-size-of-line gob new-line))
    (s-value new-line :next-line nil)
    (s-value gob :last-line new-line)
    (if prev-object
      (progn
	(s-value prev-object :mf-next-object nil)
	(s-value gob :last-object prev-object))
      (progn
	(s-value gob :first-object nil)
	(s-value gob :last-object nil)))
    (let* ((first-line (g-value gob :first-line))
	   (first-frag (g-value first-line :first-frag)))
      (s-value gob :cursor-line first-line)
      (s-value gob :cursor-frag first-frag)
      (s-value gob :cursor-position 0)
      (s-value gob :cursor-frag-pos 0)
      (s-value gob :cursor-x-offset 0)
      (s-value gob :select-line first-line)
      (s-value gob :select-frag first-frag)
      (s-value gob :select-position 0)
      (s-value gob :select-frag-pos 0)
      (s-value gob :selection-p nil))
    (when (g-value gob :word-wrap-p)
      (do ((my-line (g-value gob :first-line) (g-value my-line :next-line)))
	  ((null my-line))
	(wrap-line gob my-line)))))


;;; METHODS

(define-method :set-styles MULTIFONT-LINE (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +multifont-lstyle+) line-style)))


(define-method :set-frr-bbox MULTIFONT-LINE (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +multifont-left+)
		     (aref update-vals +multifont-top+)
		     (aref update-vals +multifont-width+)
		     (aref update-vals +multifont-height+))))


(define-method :write-slots opal:multifont-text (agget normal-proto components
						       behaviors
						       suppress-children?)
  (s-value agget :initial-text (get-text agget))
  (call-prototype-method agget normal-proto components
			 behaviors suppress-children?))

(define-method :initialize-copy opal:multifont-text (orig copy)
  (s-value copy :initial-text (opal:get-text orig))
  (kr-send copy :initialize copy))

(define-method :fix-update-slots opal:MULTIFONT-TEXT (gob)
  (if (g-value gob :word-wrap-p)
      (let ((text-width (g-value gob :text-width)))
	(declare (fixnum text-width))
	(do ((my-line (g-value gob :first-line) (g-value my-line :next-line)))
	    ((null my-line))
	  (if (> (g-value my-line :width) text-width)
	      (wrap-line gob my-line)
	      (do ()
		  ((null (undo-wrap-line gob my-line)))))))
      (do ((my-line (g-value gob :first-line) (g-value my-line :next-line)))
	  ((null my-line))
	(when (frag-break-p (g-value my-line :last-frag))
	  (merge-lines gob my-line (g-value my-line :next-line))))))


;; Method :INITIALIZE : create initial data for text box.
(define-method :initialize opal:MULTIFONT-TEXT (gob &optional (first-time t))
  (when first-time
    (kr-send opal:aggregate :initialize gob))
  (let ((cursor (create-instance nil multifont-text-cursor))
	(text (g-value gob :initial-text)))
    (check-text text)
    (s-value gob :cursor cursor)
    (opal:add-component gob cursor :front)
    (install-text gob text)))


(define-method :draw opal::MULTIFONT-TEXT-CURSOR (gob a-window)
  (call-prototype-method gob a-window)
  (with-demons-disabled
      (s-value gob :force-update NIL)))


;; Method :DRAW : draws a single line.
;; Remaining g-values:
;;   (g-value gob :ascent)
;;   (g-value gob :first-frag)
;;   (g-value (frag-font frag) :xfont)
;;   (g-value line-style :foreground-color :colormap-index)
;;   (g-value line-style :background-color :colormap-index)
;;
(define-method :draw opal::MULTIFONT-LINE (gob a-window)

  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (show-marks (g-value gob :show-marks))
	 (left (aref update-vals +text-left+))
	 (top (+ (the fixnum (aref update-vals +text-top+))
		 (g-value-fixnum gob :ascent)))
	 (line-style (aref update-vals +mf-text-lstyle+))
	 (function (aref update-vals +mf-text-draw-function+))
	 (fill-p (aref update-vals +mf-text-fill-background-p+)))
    (declare (fixnum left top))

    (setf (aref update-vals +multifont-force-update+) NIL)

    (with-demons-disabled
	(s-value gob :force-update NIL))

    (when line-style

      (do ((frag (g-value gob :first-frag) (frag-next frag)))
	  ((null frag))

	(if (frag-object-p frag)

	    ;; This is not a string.
	    (progn
	      ;; skip writing text if it's a schema
	      (unless (= (frag-start-highlight frag) (frag-end-highlight frag))
		;; XXX do something to hightlight object
		)
	      (let ((obj (frag-object frag)))
		(when (and (mark-p obj) show-marks)
		  (let* ((sticky-left (mark-sticky-left obj))
			 (draw-left (if sticky-left (- left 3) (- left 5)))
			 (string (if sticky-left "<" ">")))
		    (gem:draw-text a-window draw-left (+ top 4) string
				   (opal:get-standard-font :fixed :bold :small)
				   function line-style))))
	      (incf left (frag-width frag)))

	    ;; This is a string.
	    (let* ((new-line-style nil)
		   (foreground-color
		    (let ((fcolor (frag-fcolor frag)))
		      (if fcolor
			  (setq new-line-style
				(create-instance nil line-style
				  (:constant nil)
				  (:foreground-color fcolor)))
			  (setq fcolor (g-value line-style :foreground-color)))
		      #+comment
		      (g-value fcolor :colormap-index)))
		   (background-color
		    (let ((bcolor (frag-bcolor frag)))
		      (if bcolor
			  (if new-line-style
			      (s-value new-line-style :background-color bcolor)
			      (setq new-line-style
				    (create-instance nil line-style
				      (:constant nil)
				      (:background-color bcolor))))
			  (setq bcolor (g-value line-style :background-color)))
		      #+comment
		      (g-value bcolor :colormap-index)))
		   (string (frag-string frag)))

	      (declare (ignore foreground-color background-color))
	      ;;  (opal::set-gc line-style-gc xlib-gc-line :foreground foreground-color)
	      ;;  (opal::set-gc line-style-gc xlib-gc-line :background background-color)

	      (setf new-line-style (or new-line-style line-style))

	      (let ((start-highlight (frag-start-highlight frag))
		    (end-highlight (frag-end-highlight frag)))
		(declare (fixnum start-highlight end-highlight))
		(if (= start-highlight end-highlight)
		    (progn
		      (gem:draw-text
		       a-window left top string (frag-font frag) function
		       new-line-style fill-p)
		      (incf left (frag-width frag))) ;***0
		    (if (and (= start-highlight 0)
			     (=  end-highlight (frag-length frag)))
			(progn
			  (gem:draw-text
			   a-window left top string (frag-font frag) function
			   new-line-style fill-p T)
			  (incf left (frag-width frag)))

			(let* ((end-highlight2 (min end-highlight
						    (length string)))
			       (left-str (subseq string
						 0 start-highlight))
			       (mid-str (subseq string
						start-highlight
						end-highlight2))
			       (right-str (subseq string
						  end-highlight2))
			       (font (frag-font frag)))
			  (declare (fixnum end-highlight2))

			  ;; Draw left portion of the fragment
			  (gem:draw-text
			   a-window left top left-str font function
			   new-line-style fill-p)
			  (incf left (opal:string-width font left-str
							:display a-window))

			  ;; Draw middle portion, switch fore/back
			  (gem:draw-text
			   a-window left top mid-str font function
			   new-line-style fill-p T)
			  (incf left (opal:string-width font mid-str
							:display a-window))

			  ;; Draw the right portion
			  (gem:draw-text
			   a-window left top right-str font function
			   new-line-style fill-p)
			  (incf left (opal:string-width font right-str
							:display a-window))))))))))))



;;; Helper functions for Operations

;; This returns neccessary computations to update the position of the cursor.
;; Given the line and pixel offset of the cursor, this will return the
;; fragment, fragment offset, pixel offset, and character offset.
;;
(defun calculate-cursor-x (my-line x-offset)
  (declare (fixnum x-offset))
  (when (>= x-offset (g-value-fixnum my-line :width))
    (setq x-offset (1- (g-value-fixnum my-line :width))))
  (let ((cursor-position 0)
	(cursor-offset 0)
	(cursor-frag-pos 0)
	cursor-frag)
    (declare (fixnum cursor-position cursor-offset cursor-frag-pos))
    (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	  (f-width (frag-width frag) (frag-width frag)))
	 ((> (+ cursor-offset f-width) x-offset)
	  (setq cursor-frag frag))
      (declare (fixnum f-width))
      (incf cursor-position (the fixnum (frag-length frag)))
      (incf cursor-offset f-width))
    (unless (frag-object-p cursor-frag)
      (let ((string (frag-string cursor-frag))
	    (font (frag-font cursor-frag)))
	(do* ((frag-pos 0 (1+ frag-pos))
	      (c-width (opal:char-width font (schar string frag-pos))
		       (opal:char-width font (schar string frag-pos))))
	     ((> (+ cursor-offset c-width) x-offset)
	      (setq cursor-frag-pos frag-pos))
	  (declare (fixnum c-width))
	  (incf cursor-position)
	  (incf cursor-offset c-width))))
    (if (zerop cursor-frag-pos)
      (do ((prev (frag-prev cursor-frag) (frag-prev prev)))
	  ((or (null prev) (not (frag-object-p prev))
	       (not (mark-p (frag-object prev))))
	   (if prev
	     (values prev (frag-length prev) cursor-offset cursor-position)
	     (do ((frag (g-value my-line :first-frag) (frag-next frag)))
		 ((not (and (frag-object-p frag)
			    (mark-p (frag-object frag))))
		  (values frag 0 0 0))))))
      (values cursor-frag cursor-frag-pos cursor-offset cursor-position))))


;; Makes certain that line gets redrawn even though no slot in the line schema
;; has changed.
(defmacro invalidate-line (line)
   `(s-value ,line :force-update T))


;; Reset font and colors.  This is neccessary since it is possible for
;;the user to set the font with a s-value to :current-font.  This must be
;;reset whenever any operation other than adding characters is performed.
(defmacro reset-font (gob)
  `(let* ((frag (g-value ,gob :cursor-frag))
	  (prev (frag-prev frag)))
     (when (and prev (zerop (g-value-fixnum ,gob :cursor-frag-pos)))
       (if (frag-object-p prev)
	   (setq frag (frag-prev prev))
	   (setq frag prev)))
     (when (not (frag-object-p frag))
       (s-value ,gob :current-font
		(frag-font frag))
       (s-value ,gob :current-fcolor
		(frag-fcolor frag))
       (s-value ,gob :current-bcolor
		(frag-bcolor frag)))))


;; Returns t if cursor position 1 is higher (or equal) to cursor position 2,
;; nil otherwise.
(defun higher-cursor (line1 pos1 line2 pos2)
  (declare (fixnum pos1 pos2))
  (if (< (g-value-fixnum line1 :top) (g-value-fixnum line2 :top))
      t
      (if (eq line1 line2)
	  (<= pos1 pos2)
	  nil)))


;; Switch highlight on for the single line between positions pos1 and pos2.
(defun turn-on-segment-mid (my-line pos1 pos2)
  (declare (fixnum pos1 pos2))
  (unless (= pos1 pos2)
    (invalidate-line my-line)
    (let ((dec-pos1 pos1)
	  (dec-pos2 pos2))
      (declare (fixnum dec-pos1 dec-pos2))
      (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((>= length dec-pos1)
	    (if (>= length dec-pos2)
		(let ((start-highlight (frag-start-highlight frag)))
		  (if (= start-highlight (frag-end-highlight frag))
		      (progn
			(setf (frag-start-highlight frag) dec-pos1)
			(setf (frag-end-highlight frag) dec-pos2))
		      (if (> start-highlight dec-pos1)
			  (setf (frag-start-highlight frag) dec-pos1)
			  (setf (frag-end-highlight frag) dec-pos2))))
		(progn
		  (let ((start-highlight (frag-start-highlight frag)))
		    (if (= start-highlight (frag-end-highlight frag))
			(setf (frag-start-highlight frag) dec-pos1)
			(when (> start-highlight dec-pos1)
			  (setf (frag-start-highlight frag) dec-pos1))))
		  (setf (frag-end-highlight frag) length)
		  (decf dec-pos2 length)
		  (do* ((nfrag (frag-next frag) (frag-next nfrag))
			(nlength (frag-length nfrag) (frag-length nfrag)))
		       ((>= nlength dec-pos2)
			(let ((end-highlight (frag-end-highlight nfrag)))
			  (if (= (frag-start-highlight nfrag)
				 end-highlight)
			      (setf (frag-end-highlight nfrag) dec-pos2)
			      (when (< end-highlight dec-pos2)
				(setf (frag-end-highlight nfrag) dec-pos2))))
			(setf (frag-start-highlight nfrag) 0))
		    (setf (frag-start-highlight nfrag) 0)
		    (setf (frag-end-highlight nfrag) nlength)
		    (decf dec-pos2 nlength)))))
	(declare (fixnum length))
	(decf dec-pos1 length)
	(decf dec-pos2 length)))))


;; Switch highlight on for the single line between start of line and pos.
(defun turn-on-segment-left (my-line pos)
  (declare (fixnum pos))
  (unless (= pos 0)
    (invalidate-line my-line)
    (do* ((dec-pos pos (- dec-pos length))
	  (frag (g-value my-line :first-frag) (frag-next frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((>= length dec-pos)
	  (let ((end-highlight (frag-end-highlight frag)))
	    (if (= (frag-start-highlight frag) end-highlight)
		(setf (frag-end-highlight frag) dec-pos)
		(when (< end-highlight dec-pos)
		  (setf (frag-end-highlight frag) dec-pos)))
	    (setf (frag-start-highlight frag) 0)))
	(declare (fixnum dec-pos))
;;	(decf dec-pos length)
	(setf (frag-start-highlight frag) 0)
	(setf (frag-end-highlight frag) length))))


;; Switch highlight on for the single line between pos and end of line.
(defun turn-on-segment-right (my-line pos)
  (declare (fixnum pos))
  (unless (= pos (g-value my-line :length))
    (invalidate-line my-line)
    (do* ((dec-pos (g-value my-line :length) (- dec-pos length))
	    (frag (g-value my-line :last-frag) (frag-prev frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((<= (- dec-pos length) pos)
	    (let ((frag-pos (- (+ pos length) dec-pos))
		  (start-highlight (frag-start-highlight frag)))
	      (if (= start-highlight (frag-end-highlight frag))
		  (setf (frag-start-highlight frag) frag-pos)
		  (when (> start-highlight frag-pos)
		    (setf (frag-start-highlight frag) frag-pos))))
	    (setf (frag-end-highlight frag) length))
	(declare (fixnum dec-pos))
;;	(decf dec-pos length)
	(setf (frag-start-highlight frag) 0)
	(setf (frag-end-highlight frag) length))))


;; Switch highlight on for the single line.
(defun turn-on-line (my-line)
  (invalidate-line my-line)
  (do ((frag (g-value my-line :first-frag) (frag-next frag)))
      ((null frag))
    (setf (frag-start-highlight frag) 0)
    (setf (frag-end-highlight frag) (frag-length frag))))


;; Makes selection box visible between the two points given.
(defun turn-on-select (line1 pos1 line2 pos2)
  (declare (fixnum pos1 pos2))
  (if (eq line1 line2)
      (turn-on-segment-mid line1 pos1 pos2)
      (progn
	(turn-on-segment-right line1 pos1)
	(do ((my-line (g-value line1 :next-line)
		      (g-value my-line :next-line)))
	    ((eq my-line line2))
	  (turn-on-line my-line))
	(turn-on-segment-left line2 pos2))))


;; Switch highlight off for the single line between pos1 and pos2.
(defun turn-off-segment-mid (my-line pos1 pos2)
  (declare (fixnum pos1 pos2))
  (unless (= pos1 pos2)
    (invalidate-line my-line)
    (do* ((dec-pos1 pos1 (- dec-pos1 length))
	  (dec-pos2 pos2 (- dec-pos2 length))
	  (frag (g-value my-line :first-frag) (frag-next frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((>= length dec-pos1)
	  (if (>= length dec-pos2)
	      (let ((start-highlight (frag-start-highlight frag)))
		(unless (= start-highlight (frag-end-highlight frag))
		  (if (= start-highlight dec-pos1)
		      (setf (frag-start-highlight frag) dec-pos2)
		      (setf (frag-end-highlight frag) dec-pos1))))
	      (progn
		(unless (= (frag-start-highlight frag)
			   (frag-end-highlight frag))
		  (setf (frag-end-highlight frag) dec-pos1))
		(decf dec-pos2 length)
		(do* ((nfrag (frag-next frag) (frag-next nfrag))
		      (nlength (frag-length nfrag) (frag-length nfrag)))
		     ((>= nlength dec-pos2)
		      (unless (= (frag-start-highlight nfrag)
				 (frag-end-highlight nfrag))
			(setf (frag-start-highlight nfrag) dec-pos2)))
		  (setf (frag-start-highlight nfrag) 0)
		  (setf (frag-end-highlight nfrag) 0)
		  (decf dec-pos2 nlength)))))
      (declare (fixnum dec-pos1 dec-pos2)))))


;; Switch highlight off for the single line between start of line and pos.
(defun turn-off-segment-left (my-line pos)
  (declare (fixnum pos))
  (unless (= pos 0)
    (invalidate-line my-line)
    (do* ((dec-pos pos (- dec-pos length))
	  (frag (g-value my-line :first-frag) (frag-next frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((>= length dec-pos)
	  (setf (frag-start-highlight frag) dec-pos))
      (setf (frag-start-highlight frag) 0)
      (setf (frag-end-highlight frag) 0))))


;; Switch highlight off for the single line between pos and end of line.
(defun turn-off-segment-right (my-line pos)
  (declare (fixnum pos))
  (unless (= pos (g-value my-line :length))
    (invalidate-line my-line)
    (do* ((dec-pos (g-value my-line :length) (- dec-pos length))
	  (frag (g-value my-line :last-frag) (frag-prev frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((<= (- dec-pos length) pos)
	  (setf (frag-end-highlight frag)
		(- (+ pos length) dec-pos)))
      (declare (fixnum dec-pos))
      (setf (frag-start-highlight frag) 0)
      (setf (frag-end-highlight frag) 0))))


;; Switch highlight off for the single line.
(defun turn-off-line (my-line)
  (invalidate-line my-line)
  (do ((frag (g-value my-line :first-frag) (frag-next frag)))
      ((null frag))
    (setf (frag-start-highlight frag) 0)
    (setf (frag-end-highlight frag) 0)))


;; Makes selection box invisible between the two points given.
(defun turn-off-select (line1 pos1 line2 pos2)
  (declare (fixnum pos1 pos2))
  (if (eq line1 line2)
      (turn-off-segment-mid line1 pos1 pos2)
      (progn
	(turn-off-segment-right line1 pos1)
	(do ((my-line (g-value line1 :next-line)
		      (g-value my-line :next-line)))
	    ((eq my-line line2))
	  (turn-off-line my-line))
	(turn-off-segment-left line2 pos2))))


;; When the cursor gets moved to a random position.  The selection box may
;; need to be updated aggressively.  This function performs an aggressive
;; change to the selection box.  change-line, change-pos is the end-point of
;; the selection that was moved.  stable-line, stable-pos is the other
;; end-point of the selection.  new-line, new-pos is the new end-point.
(defun reset-selection (change-line change-pos stable-line stable-pos
			new-line new-pos)
  (declare (fixnum change-pos stable-pos new-pos))
  (if (higher-cursor stable-line stable-pos change-line change-pos)
      (if (higher-cursor new-line new-pos stable-line stable-pos)
	  (progn
	    (turn-on-select new-line new-pos stable-line stable-pos)
	    (turn-off-select stable-line stable-pos change-line change-pos))
	  (if (higher-cursor new-line new-pos change-line change-pos)
	      (turn-off-select new-line new-pos change-line change-pos)
	      (turn-on-select change-line change-pos new-line new-pos)))
      (if (higher-cursor stable-line stable-pos new-line new-pos)
	  (progn
	    (turn-on-select stable-line stable-pos new-line new-pos)
	    (turn-off-select change-line change-pos stable-line stable-pos))
	  (if (higher-cursor change-line change-pos new-line new-pos)
	      (turn-off-select change-line change-pos new-line new-pos)
	      (turn-on-select new-line new-pos change-line change-pos)))))


;; Merges first-line and second-line.
(defun merge-lines (gob first-line second-line)
  (let ((last-frag-of-first-line (g-value first-line :last-frag))
	(first-frag-of-second-line (g-value second-line :first-frag))
	(third-line (g-value second-line :next-line))
	(first-line-length (g-value first-line :length)))
    (declare (fixnum first-line-length))
    (s-value first-line :next-line third-line)
    (if third-line
	(s-value third-line :prev-line first-line)
	(s-value gob :last-line first-line))
    (s-value first-line :last-frag (g-value second-line :last-frag))
    (merge-frags first-line last-frag-of-first-line first-frag-of-second-line)
    (let ((cursor-line (g-value gob :cursor-line))
	  (cursor-position (g-value gob :cursor-position)))
      (declare (fixnum cursor-position))
      (if (eq first-line cursor-line)
	  (multiple-value-bind (frag frag-pos x-offset)
	      (calculate-cursor-pos first-line cursor-position)
	    (s-value gob :cursor-frag frag)
	    (s-value gob :cursor-frag-pos frag-pos)
	    (s-value gob :cursor-x-offset x-offset))
	  (when (eq second-line cursor-line)
	    (let ((length (+ cursor-position first-line-length)))
	      (declare (fixnum length))
	      (s-value gob :cursor-line first-line)
	      (s-value gob :cursor-position length)
	      (multiple-value-bind (frag frag-pos x-offset)
		  (calculate-cursor-pos first-line length)
		(s-value gob :cursor-frag frag)
		(s-value gob :cursor-frag-pos frag-pos)
		(s-value gob :cursor-x-offset x-offset))))))
    (when (g-value gob :selection-p)
      (let ((select-line (g-value gob :select-line))
	    (select-position (g-value gob :select-position)))
	(declare (fixnum select-position))
	(if (eq first-line select-line)
	    (multiple-value-bind (frag frag-pos)
		(calculate-cursor-pos first-line select-position)
	      (declare (fixnum frag-pos))
	      (s-value gob :select-frag frag)
	      (s-value gob :select-frag-pos frag-pos))
	    (when (eq second-line select-line)
	      (let ((length (+ select-position first-line-length)))
		(declare (fixnum length))
		(s-value gob :select-line first-line)
		(s-value gob :select-position length)
		(multiple-value-bind (frag frag-pos)
		    (calculate-cursor-pos first-line length)
		  (declare (fixnum frag-pos))
		  (s-value gob :select-frag frag)
		  (s-value gob :select-frag-pos frag-pos)))))))
    (incf (g-value first-line :length) (g-value second-line :length))
    (incf (g-value first-line :width) (g-value second-line :width))
    (s-value first-line :ascent
	     (max (g-value first-line :ascent) (g-value second-line :ascent)))
    (s-value first-line :descent
	     (max (g-value first-line :descent) (g-value second-line :descent)))

    (calculate-size-of-line gob first-line) ;*************3
    (s-value second-line :first-frag nil)
    (s-value second-line :last-frag nil)
    (destroy-line second-line)
    (when (g-value gob :word-wrap-p)
      (wrap-line gob first-line))))


;; Returns non-nil if there exists enough space for part of the second line
;; to be merged into the first.  Returns nil otherwise.
(defun unwrap-space-check (gob first-line second-line)
  (let ((text-width (g-value gob :text-width))
	(first-width (g-value first-line :width))
	(second-width (g-value second-line :width)))
    (declare (fixnum text-width first-width second-width))
    (if (<= (+ first-width second-width) text-width)
	T
	(let ((spaces (find-spaces second-line))
	      (size (- text-width first-width)))
	  (do ((item (pop spaces) (pop spaces)))
	      ((or (null item) (<= (second item) size))
	       item))))))


;; Merges line with its next line if the line has enough space to accomodate
;; a word of the next line.  Returns non-nil if merge occurs, nil otherwise.
(defun undo-wrap-line (gob my-line)
  (let ((next-line (g-value my-line :next-line)))
    (when next-line
      (let* ((last-frag (g-value my-line :last-frag))
	     (last-frag-length (frag-length last-frag))
	     (first-frag (g-value next-line :first-frag))
	     (prev-length (g-value my-line :length)))
	(declare (fixnum last-frag-length prev-length))
	(when (and (frag-break-p last-frag)
		   (or (zerop last-frag-length)
		       (frag-object-p last-frag)
		       (not (eq #\space (schar (frag-string last-frag)
					       (1- last-frag-length))))
		       (and (not (frag-object-p first-frag))
			    (not (zerop (frag-length first-frag))) ;***
			    (eq #\space (schar (frag-string first-frag) 0)))
		       (unwrap-space-check gob my-line next-line)))
	  (merge-lines gob my-line next-line)
	  (not (= prev-length (g-value my-line :length))))))))


;; Put the pertinent information about a font into a convenient format.
(defun extract-key-from-font (font)
  (list (g-value font :family) (g-value font :face) (g-value font :size)))

(defun update-font (old-font my-font family size italic bold first-face)
  (if my-font
      my-font
      (let ((key (extract-key-from-font old-font)))
	(when family
	  (setf (first key) family))
	(unless (eq italic :not-supplied)
	  (if italic
	      (case italic
		(:toggle-first
		 (if (or (eq first-face :roman) (eq first-face :bold))
		     (setf (second key)
			   (case (second key)
			     (:roman :italic)
			     (:bold :bold-italic)
			     (:italic :italic)
			     (:bold-italic :bold-italic)))
		     (setf (second key)
			   (case (second key)
			     (:roman :roman)
			     (:bold :bold)
			     (:italic :roman)
			     (:bold-italic :bold)))))
		(:toggle (setf (second key)
			       (case (second key)
				 (:roman :italic)
				 (:bold :bold-italic)
				 (:italic :roman)
				 (:bold-italic :bold))))
		(t (setf (second key)
			 (case (second key)
			   (:roman :italic)
			   (:bold :bold-italic)
			   (:italic :italic)
			   (:bold-italic :bold-italic)))))
	      (setf (second key)
		    (case (second key)
		      (:roman :roman)
		      (:bold :bold)
		      (:italic :roman)
		      (:bold-italic :bold)))))
	(unless (eq bold :not-supplied)
	  (if bold
	      (case bold
		(:toggle-first
		 (if (or (eq first-face :roman) (eq first-face :italic))
		     (setf (second key)
			   (case (second key)
			     (:roman :bold)
			     (:bold :bold)
			     (:italic :bold-italic)
			     (:bold-italic :bold-italic)))
		     (setf (second key)
			   (case (second key)
			     (:roman :roman)
			     (:bold :roman)
			     (:italic :italic)
			     (:bold-italic :italic)))))
		(:toggle (setf (second key)
			       (case (second key)
				 (:roman :bold)
				 (:bold :roman)
				 (:italic :bold-italic)
				 (:bold-italic :italic))))
		(t (setf (second key)
			 (case (second key)
			   (:roman :bold)
			   (:bold :bold)
			   (:italic :bold-italic)
			   (:bold-italic :bold-italic)))))
	      (setf (second key)
		    (case (second key)
		      (:roman :roman)
		      (:bold :roman)
		      (:italic :italic)
		      (:bold-italic :italic)))))
	(when size
	  (if (eq size :bigger)
	      (setf (third key)
		    (case (third key)
		      (:small :medium)
		      (:medium :large)
		      (:large :very-large)
		      (:very-large :very-large)))
	      (if (eq size :smaller)
		  (setf (third key)
			(case (third key)
			  (:small :small)
			  (:medium :small)
			  (:large :medium)
			  (:very-large :large)))
		  (setf (third key) size))))
	(opal:get-standard-font (first key) (second key) (third key)))))

(defun change-font-frag (frag my-font family size italic bold first-face)
  (unless (frag-object-p frag)
    (let* ((old-font (frag-font frag))
	   (new-font (update-font old-font my-font family size italic
				  bold first-face)))
      (unless (eq old-font new-font)
	(setf (frag-font frag) new-font)
	(setf (frag-ascent frag) (g-value new-font :max-char-ascent))
	(setf (frag-descent frag) (g-value new-font :max-char-descent))
	(setf (frag-width frag)
	      (opal:string-width new-font (frag-string frag)))))))


(defun change-font-mid (gob my-line start-pos end-pos
			my-font family size italic bold key)
  (declare (fixnum start-pos end-pos))
  (unless (= start-pos end-pos)
    (invalidate-line my-line)
    (let ((dec-pos1 start-pos)
	  (dec-pos2 end-pos)
	  new-frag)
      (declare (fixnum dec-pos1 dec-pos2))
      (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((>= length dec-pos1)
	    (setq new-frag (split-frag frag dec-pos1))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (decf dec-pos2 (frag-length frag)))
	(declare (fixnum length))
	(decf dec-pos1 length)
	(decf dec-pos2 length))
      (do* ((frag new-frag (frag-next frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((<= dec-pos2 length)
	    (setq new-frag (split-frag frag dec-pos2))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (change-font-frag frag my-font family size italic bold key))
	(declare (fixnum length))
	(decf dec-pos2 length)
	(change-font-frag frag my-font family size italic bold key)))
    (calculate-size-of-line gob my-line)))


(defun change-font-right (gob my-line pos my-font family size italic bold key)
  (declare (fixnum pos))
  (let ((line-length (g-value my-line :length)))
    (declare (fixnum line-length))
    (unless (= pos line-length)
      (invalidate-line my-line)
      (do* ((new-frag)
	    (dec-pos (- line-length pos))
	    (frag (g-value my-line :last-frag) (frag-prev frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((<= dec-pos length)
	    (setq dec-pos (- length dec-pos))
	    (setq new-frag (split-frag frag dec-pos))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (change-font-frag new-frag my-font
			      family size italic bold key))
	(declare (fixnum dec-pos))
	(change-font-frag frag my-font
			  family size italic bold key))
      (calculate-size-of-line gob my-line))))


(defun change-font-left (gob my-line pos my-font family size italic bold key)
  (declare (fixnum pos))
  (unless (= pos 0)
    (invalidate-line my-line)
    (do* (new-frag
	  (dec-pos pos (- dec-pos length))
	  (frag (g-value my-line :first-frag) (frag-next frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((<= dec-pos length)
	  (setq new-frag (split-frag frag dec-pos))
	  (setf (frag-prev new-frag) frag)
	  (setf (frag-next frag) new-frag)
	  (if (frag-next new-frag)
	      (setf (frag-prev (frag-next new-frag)) new-frag)
	      (s-value my-line :last-frag new-frag))
	  (change-font-frag frag my-font family size italic bold key))
      (declare (fixnum dec-pos))
      (change-font-frag frag my-font family size italic bold key))
    (calculate-size-of-line gob my-line)))


(defun change-font-line (gob my-line my-font family size italic bold key)
  (invalidate-line my-line)
  (do ((frag (g-value my-line :first-frag) (frag-next frag)))
      ((null frag))
    (change-font-frag frag my-font family size italic bold key))
  (calculate-size-of-line gob my-line))


;; Change the font of all character between the given positions.
(defun change-font (gob start-line start-pos end-line end-pos my-font
		    family size italic bold first-face)
  (if (eq start-line end-line)
      (change-font-mid gob start-line start-pos end-pos
		       my-font family size italic bold first-face)
      (progn
	(change-font-right gob start-line start-pos
			   my-font family size italic bold first-face)
	(do ((my-line (g-value start-line :next-line)
		      (g-value my-line :next-line)))
	    ((eq my-line end-line))
	  (change-font-line gob my-line my-font
			    family size italic bold first-face))
	(change-font-left gob end-line end-pos my-font family size
			  italic bold first-face))))


;;; COLOR stuff

(defun change-color-frag (frag fcolor &optional bcolor)
  (unless (frag-object-p frag)
    (let* ((old-fcolor (frag-fcolor frag))
	   (old-bcolor (frag-bcolor frag)))
      (unless (eq old-fcolor fcolor)
	(setf (frag-fcolor frag) fcolor))
      (when bcolor
	(unless (eq old-bcolor bcolor)
	  (setf (frag-bcolor frag) bcolor))))))

(defun change-color-mid (gob my-line start-pos end-pos
			 fcolor &optional bcolor)
  (declare (fixnum start-pos end-pos))
  (unless (= start-pos end-pos)
    (invalidate-line my-line)
    (let ((dec-pos1 start-pos)
	  (dec-pos2 end-pos)
	  new-frag)
      (declare (fixnum dec-pos1 dec-pos2))
      (do* ((frag (g-value my-line :first-frag) (frag-next frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((>= length dec-pos1)
	    (setq new-frag (split-frag frag dec-pos1))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (decf dec-pos2 (frag-length frag)))
	(decf dec-pos1 length)
	(decf dec-pos2 length))
      (do* ((frag new-frag (frag-next frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((<= dec-pos2 length)
	    (setq new-frag (split-frag frag dec-pos2))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (change-color-frag frag fcolor bcolor))
	(decf dec-pos2 length)
	(change-color-frag frag fcolor bcolor)))
    (calculate-size-of-line gob my-line)))


(defun change-color-right (gob my-line pos fcolor &optional bcolor)
  (declare (fixnum pos))
  (let ((line-length (g-value my-line :length)))
    (unless (= pos line-length)
      (invalidate-line my-line)
      (do* (new-frag
	    (dec-pos (- line-length pos) (- dec-pos length))
	    (frag (g-value my-line :last-frag) (frag-prev frag))
	    (length (frag-length frag) (frag-length frag)))
	   ((<= dec-pos length)
	    (setq dec-pos (- length dec-pos))
	    (setq new-frag (split-frag frag dec-pos))
	    (setf (frag-prev new-frag) frag)
	    (setf (frag-next frag) new-frag)
	    (if (frag-next new-frag)
		(setf (frag-prev (frag-next new-frag)) new-frag)
		(s-value my-line :last-frag new-frag))
	    (change-color-frag new-frag fcolor bcolor))
	(declare (fixnum dec-pos))
	(change-color-frag frag fcolor bcolor))
      (calculate-size-of-line gob my-line))))


(defun change-color-left (gob my-line pos fcolor &optional bcolor)
  (declare (fixnum pos))
  (unless (= pos 0)
    (invalidate-line my-line)
    (do* (new-frag
	  (dec-pos pos (- dec-pos length))
	  (frag (g-value my-line :first-frag) (frag-next frag))
	  (length (frag-length frag) (frag-length frag)))
	 ((<= dec-pos length)
	  (setq new-frag (split-frag frag dec-pos))
	  (setf (frag-prev new-frag) frag)
	  (setf (frag-next frag) new-frag)
	  (if (frag-next new-frag)
	      (setf (frag-prev (frag-next new-frag)) new-frag)
	      (s-value my-line :last-frag new-frag))
	  (change-color-frag frag fcolor bcolor))
      (declare (fixnum dec-pos))
      (change-color-frag frag fcolor bcolor))
    (calculate-size-of-line gob my-line)))


(defun change-color-line (gob my-line fcolor &optional bcolor)
  (invalidate-line my-line)
  (do ((frag (g-value my-line :first-frag) (frag-next frag)))
      ((null frag))
    (change-color-frag frag fcolor bcolor))
  (calculate-size-of-line gob my-line))


;; Change the color of all character between the given positions.
(defun change-color (gob start-line start-pos end-line end-pos
		     fcolor &optional bcolor)
  (if (eq start-line end-line)
      (change-color-mid gob start-line start-pos end-pos fcolor bcolor)
      (progn
	(change-color-right gob start-line start-pos fcolor bcolor)
	(do ((my-line (g-value start-line :next-line)
		      (g-value my-line :next-line)))
	    ((eq my-line end-line))
	  (change-color-line gob my-line fcolor bcolor))
	(change-color-left gob end-line end-pos fcolor bcolor))))



;;; OPERATIONS

(defun SET-CURSOR-VISIBLE (gob vis)
  (s-value (g-value gob :cursor) :visible vis))


(defun SET-CURSOR-TO-X-Y-POSITION (gob x y)
  (declare (fixnum x y))
  (let (new-line)
    (do ((my-line (g-value gob :first-line) (g-value my-line :next-line)))
	((or (null my-line) (> (g-value-fixnum my-line :top) y))
	 (setq new-line
	       (if my-line
		   (or (g-value my-line :prev-line) my-line)
		   (g-value gob :last-line)))))
    (multiple-value-bind (frag frag-pos x-offset my-position)
	(calculate-cursor-x new-line (max 0 (- x (g-value-fixnum gob :left))))
      (when (g-value gob :selection-p)
	(reset-selection (g-value gob :cursor-line)
			 (g-value gob :cursor-position) (g-value gob :select-line)
			 (g-value gob :select-position) new-line my-position))
      (s-value gob :cursor-line new-line)
      (s-value gob :cursor-frag frag)
      (s-value gob :cursor-frag-pos frag-pos)
      (s-value gob :cursor-x-offset x-offset)
      (s-value gob :cursor-position my-position)))
  (reset-font gob)
  t)


(defun SET-CURSOR-TO-LINE-CHAR-POSITION (gob my-line char)
  (let ((new-line (g-value gob :first-line)))
    (dotimes (i my-line)
      (when new-line
	(setq new-line (g-value new-line :next-line))))
    (unless new-line
      (setq new-line (g-value gob :last-line)))
    (setq char (min char (1- (g-value-fixnum new-line :length))))
    (when (g-value gob :selection-p)
      (reset-selection (g-value gob :cursor-line)
		       (g-value gob :cursor-position) (g-value gob :select-line)
		       (g-value gob :select-position) new-line char)
      )
    (s-value gob :cursor-line new-line)
    (s-value gob :cursor-position char)
    (multiple-value-bind (frag frag-pos x-offset)
	(calculate-cursor-pos new-line char)
      (s-value gob :cursor-frag frag)
      (s-value gob :cursor-frag-pos frag-pos)
      (s-value gob :cursor-x-offset x-offset)
      )
    )
  (reset-font gob)
  t
  )


;; Returns multiple values.  First line then char position.
 (defun GET-CURSOR-LINE-CHAR-POSITION (gob)
   (do ((target (g-value gob :cursor-line))
	(my-line (g-value gob :first-line) (g-value my-line :next-line))
	(i 0 (1+ i)))
       ((eq my-line target)
	(values i (g-value gob :cursor-position)))
     (declare (fixnum i))))


;; Returns character cursor passed over, or #\newline if we went to a
;; new line, or nil if at end of text.
(defun GO-TO-NEXT-CHAR (gob)
  (let* ((my-line (g-value gob :cursor-line))
	 (next-line (g-value my-line :next-line))
	 (frag (g-value gob :cursor-frag))
	 (frag-pos (g-value gob :cursor-frag-pos)))
    (declare (fixnum frag-pos))
    (when (= frag-pos (the fixnum (frag-length frag)))
      (setq frag (frag-next frag))
      (do () ((or (null frag) (not (mark-p (frag-object frag)))))
	(setq frag (frag-next frag)))
      (setq frag-pos 0))
    (when (not (frag-object-p frag))
      (s-value gob :current-font (frag-font frag))
      (s-value gob :current-fcolor (frag-fcolor frag))
      (s-value gob :current-bcolor (frag-bcolor frag)))
    (if (and (null next-line) (>= frag-pos (1- (frag-length frag)))
	     (null (frag-next frag)))
	nil
	(let ((char (if (frag-object-p frag)
			(frag-object frag)
			(schar (frag-string frag) frag-pos))))
	  (when (g-value gob :selection-p)
	    (invalidate-line my-line)
	    (if (= (frag-start-highlight frag) (frag-end-highlight frag))
		(progn
		  (setf (frag-start-highlight frag) frag-pos)
		  (setf (frag-end-highlight frag) (1+ frag-pos)))
		(if (= (frag-end-highlight frag) frag-pos)
		    (incf (frag-end-highlight frag))
		    (incf (frag-start-highlight frag)))))
	  (incf frag-pos)
	  (if (= frag-pos (frag-length frag))
	      (let ((next (frag-next frag)))
		(if (null next)
		    (progn
		      (unless (frag-break-p frag)
			(setq char #\newline))
		      (setq frag (g-value next-line :first-frag))
		      (setq next (frag-next frag))
		      (setq frag-pos 0)
		      (s-value gob :cursor-line next-line)
		      (s-value gob :cursor-position 0)
		      (s-value gob :cursor-x-offset 0))
		    (progn
		      (incf (g-value-fixnum gob :cursor-position))
		      (incf (g-value gob :cursor-x-offset)
			    (the fixnum
				 (if (frag-object-p frag)
				     (frag-width frag)
				     (opal:char-width (frag-font frag) char))))))
		(if (and (= frag-pos (frag-length frag))
			 (let ((obj (frag-object next)))
			   (and (mark-p obj) (mark-sticky-left obj))))
		    (progn
		      (do* ((my-frag (frag-next next) (frag-next my-frag))
			    (my-obj (frag-object my-frag) (frag-object my-frag)))
			   ((not (mark-p my-obj))
			    (s-value gob :cursor-frag my-frag)))
		      (s-value gob :cursor-frag-pos 0))
		    (progn
		      (s-value gob :cursor-frag frag)
		      (s-value gob :cursor-frag-pos frag-pos))))
	      (progn
		(s-value gob :cursor-frag frag)
		(s-value gob :cursor-frag-pos frag-pos)
		(incf (g-value-fixnum gob :cursor-position))
		(incf (g-value gob :cursor-x-offset)
		      (if (frag-object-p frag)
			  (frag-width frag)
			  (opal:char-width (frag-font frag) char)))))
	  (reset-font gob)
	  char))))


;; Returns character cursor passed over, or #\newline if cursor went to a
;; new line, or nil if at beginning of text.
(defun GO-TO-PREV-CHAR (gob)
  (reset-font gob)
  (block zero
    (let ((my-line (g-value gob :cursor-line))
	  (frag (g-value gob :cursor-frag))
	  (frag-pos (1- (g-value-fixnum gob :cursor-frag-pos)))
	  (char-size 0)
	  char)
      (declare (fixnum frag-pos char-size))
      (do ((prev-frag (frag-prev frag) (frag-prev frag)))
	  ((or (null prev-frag) (>= frag-pos 0)))
	(setq frag prev-frag)
	(setq frag-pos (1- (frag-length frag))))
      (if (< frag-pos 0)
	  (progn
	    (unless (setq my-line (g-value my-line :prev-line))
	      (return-from zero nil)
	      )
	    (s-value gob :cursor-line my-line)
	    (setq frag
		  (s-value gob :cursor-frag (g-value my-line :last-frag)))
	    (s-value gob :cursor-position (g-value my-line :length))
	    (setq frag-pos
		  (s-value gob :cursor-frag-pos (1- (frag-length frag))))
	    (s-value gob :cursor-x-offset (g-value my-line :width))
	    (if (frag-break-p frag)
		(if (frag-object-p frag)
		    (progn
		      (setq char (frag-object frag))
		      (setq char-size (frag-width frag)))
		    (progn
		      (setq char (schar (frag-string frag) frag-pos))
		      (setq char-size (opal:char-width (frag-font frag) char))))
		(progn
		  (setq char #\newline)
		  (setq char-size (opal:char-width (frag-font frag) #\space))
		  )
		)
	    )
	  (if (frag-object-p frag)
	      (progn
		(setq char (frag-object frag))
		(setq char-size (frag-width frag)))
	      (progn
		(setq char (schar (frag-string frag) frag-pos))
		(setq char-size
		      (opal:char-width (frag-font frag) char))))
	  )
      (when (g-value gob :selection-p)
	(invalidate-line my-line)
	(let ((start-highlight (frag-start-highlight frag)))
	  (if (= start-highlight (frag-end-highlight frag))
	      (progn
		(setf (frag-start-highlight frag) frag-pos)
		(setf (frag-end-highlight frag) (1+ frag-pos))
		)
	      (if (> start-highlight frag-pos)
		  (decf (the fixnum (frag-start-highlight frag)))
		  (decf (the fixnum (frag-end-highlight frag)))
		  )
	      )
	  ))
      (let ((prev-frag (frag-prev frag)))
	(when (and (= frag-pos 0) prev-frag
		   (let ((obj (frag-object prev-frag)))
		     (or (not (mark-p obj))
			 (not (mark-sticky-left obj)))))
	  (setq frag (frag-prev frag))
	  (setq frag-pos (frag-length frag))
	  )
	)
      (do ((prev-frag (frag-prev frag) (frag-prev frag)))
	  ((not (mark-p (frag-object frag))))
	(setq frag prev-frag)
	(setq frag-pos (frag-length frag))
	)
      (s-value gob :cursor-line my-line)
      (s-value gob :cursor-frag frag)
      (s-value gob :cursor-frag-pos frag-pos)
      (decf (g-value-fixnum gob :cursor-position))
      (decf (g-value gob :cursor-x-offset) char-size)
      (reset-font gob)
      char
      )
    )
  )


(defmacro delim-char-p (char)
  `(member ,char *delim-chars* :test #'eq))


(defun GO-TO-NEXT-WORD (gob)
  (let ((str ""))
    (declare (simple-string str))
    (do ((char (GO-TO-NEXT-CHAR gob) (GO-TO-NEXT-CHAR gob)))
	((or (null char) (and (characterp char)
			      (delim-char-p char)))
	 (do ((space char (GO-TO-NEXT-CHAR gob))
	      (next-char (FETCH-NEXT-CHAR gob) (FETCH-NEXT-CHAR gob)))
	     ((or (null space) (not (characterp next-char))
		  (not (delim-char-p next-char)))
	      (concatenate 'string str
			   (if space
			     (string space)
			     "")))
	   (setq str (concatenate 'string str (string space)))))
      (setq str (concatenate 'string str
			     (if (characterp char)
			       (string char)
			       (format NIL "~s" char)))))))


(defun GO-TO-PREV-WORD (gob)
  (let ((str ""))
    (declare (simple-string str))
    (do ((char (GO-TO-PREV-CHAR gob) (GO-TO-PREV-CHAR gob)))
	((or (null char) (and (characterp char) (delim-char-p char)))
	 (do ((space char (GO-TO-PREV-CHAR gob))
	      (prev-char (FETCH-PREV-CHAR gob) (FETCH-PREV-CHAR gob)))
	     ((or (null space) (not (characterp prev-char))
		  (not (delim-char-p prev-char)))
	      (concatenate 'string
			   (if space
			     (string space)
			     "") str))
	   (setq str (concatenate 'string (string space) str))))
      (setq str (concatenate 'string
			     (if (characterp char)
			       (string char)
			       (format NIL "~s" char)) str)))))


 (defun GO-TO-NEXT-LINE (gob)
   (let ((next-line (g-value gob :cursor-line :next-line)))
     (when next-line
       (multiple-value-bind (frag frag-pos x-offset my-position)
	   (calculate-cursor-x next-line (g-value gob :cursor-x-offset))
	 (declare (fixnum frag-pos x-offset))
	 (when (g-value gob :selection-p)
	   (reset-selection (g-value gob :cursor-line)
			    (g-value gob :cursor-position) (g-value gob :select-line)
			    (g-value gob :select-position) next-line my-position)
	   )
	 (s-value gob :cursor-line next-line)
	 (s-value gob :cursor-frag frag)
	 (s-value gob :cursor-frag-pos frag-pos)
	 (s-value gob :cursor-x-offset x-offset)
	 (s-value gob :cursor-position my-position))))
   (reset-font gob))


(defun GO-TO-PREV-LINE (gob)
   (let ((prev-line (g-value gob :cursor-line :prev-line)))
      (when prev-line
         (multiple-value-bind (frag frag-pos x-offset my-position)
               (calculate-cursor-x prev-line (g-value gob :cursor-x-offset))
            (when (g-value gob :selection-p)
               (reset-selection (g-value gob :cursor-line)
                     (g-value gob :cursor-position) (g-value gob :select-line)
                     (g-value gob :select-position) prev-line my-position)
            )
            (s-value gob :cursor-line prev-line)
            (s-value gob :cursor-frag frag)
            (s-value gob :cursor-frag-pos frag-pos)
            (s-value gob :cursor-x-offset x-offset)
            (s-value gob :cursor-position my-position)
         )
      )
   )
   (reset-font gob)
)


(defun GO-TO-BEGINNING-OF-TEXT (gob)
   (let ((my-line (g-value gob :first-line)))
      (when (g-value gob :selection-p)
         (reset-selection (g-value gob :cursor-line)
               (g-value gob :cursor-position) (g-value gob :select-line)
               (g-value gob :select-position) my-line 0)
      )
      (s-value gob :cursor-line my-line)
      (do ((my-frag (g-value my-line :first-frag) (frag-next my-frag)))
	  ((or (null my-frag) (not (mark-p (frag-object my-frag))))
	   (s-value gob :cursor-frag my-frag)))
      (s-value gob :cursor-frag-pos 0)
      (s-value gob :cursor-x-offset 0)
      (s-value gob :cursor-position 0)
   )
   (reset-font gob)
)


(defun GO-TO-END-OF-TEXT (gob)
   (let* ((my-line (g-value gob :last-line))
          (length (1- (g-value-fixnum my-line :length))))
      (when (g-value gob :selection-p)
         (reset-selection (g-value gob :cursor-line)
               (g-value gob :cursor-position) (g-value gob :select-line)
               (g-value gob :select-position) my-line length)
      )
      (s-value gob :cursor-line my-line)
      (s-value gob :cursor-position length)
      (multiple-value-bind (frag frag-pos x-offset)
            (calculate-cursor-pos my-line length)
         (s-value gob :cursor-frag frag)
         (s-value gob :cursor-frag-pos frag-pos)
         (s-value gob :cursor-x-offset x-offset)
      )
   )
   (reset-font gob)
)


(defun GO-TO-BEGINNING-OF-LINE (gob)
   (let ((my-line (g-value gob :cursor-line)))
      (when (g-value gob :selection-p)
         (reset-selection (g-value gob :cursor-line)
               (g-value gob :cursor-position) (g-value gob :select-line)
               (g-value gob :select-position) my-line 0)
      )
      (s-value gob :cursor-line my-line)
      (do ((my-frag (g-value my-line :first-frag) (frag-next my-frag)))
	  ((or (null my-frag) (not (mark-p (frag-object my-frag))))
	   (s-value gob :cursor-frag my-frag)))
      (s-value gob :cursor-frag-pos 0)
      (s-value gob :cursor-x-offset 0)
      (s-value gob :cursor-position 0)
   )
   (reset-font gob)
)


(defun GO-TO-END-OF-LINE (gob)
   (let* ((my-line (g-value gob :cursor-line))
          (length (1- (g-value-fixnum my-line :length))))
      (when (g-value gob :selection-p)
         (reset-selection (g-value gob :cursor-line)
               (g-value gob :cursor-position) (g-value gob :select-line)
               (g-value gob :select-position) my-line length)
      )
      (s-value gob :cursor-line my-line)
      (s-value gob :cursor-position length)
      (multiple-value-bind (frag frag-pos x-offset)
            (calculate-cursor-pos my-line length)
         (s-value gob :cursor-frag frag)
         (s-value gob :cursor-frag-pos frag-pos)
         (s-value gob :cursor-x-offset x-offset)
      )
   )
   (reset-font gob)
)


(defun FETCH-NEXT-CHAR (gob)
  (let ((frag (g-value gob :cursor-frag))
	(frag-pos (g-value gob :cursor-frag-pos)))
    (declare (fixnum frag-pos))
    (do () ((not (= frag-pos (frag-length frag))))
      (setq frag (frag-next frag))
      (setq frag-pos 0))
    (if (frag-object-p frag)
      (frag-object frag)
      (let ((char (schar (frag-string frag) frag-pos)))
	(when (and (= (1+ frag-pos) (frag-length frag))
		   (null (frag-next frag)))
	  (unless (frag-break-p frag)
	    (if (g-value gob :cursor-line :next-line)
	      (setq char #\newline)
	      (setq char nil))))
	char))))


(defun FETCH-PREV-CHAR (gob)
  (let ((frag (g-value gob :cursor-frag))
	(frag-pos (1- (g-value-fixnum gob :cursor-frag-pos)))
	char)
    (declare (fixnum frag-pos))
    (if (and (< frag-pos 0) (frag-prev frag))
      (do ((prev-frag (frag-prev frag) (frag-prev prev-frag)))
	  ((or (null prev-frag) (not (mark-p (frag-object prev-frag))))
	      (setq frag prev-frag)
	      (setq frag-pos (if frag (1- (frag-length frag)) -1)))))
    (if (< frag-pos 0)
      (let ((prev-line (g-value gob :cursor-line :prev-line)))
	(unless prev-line
	  (return-from fetch-prev-char nil))
	(setq frag (g-value prev-line :last-frag))
	(do () ((or (null frag) (not (mark-p (frag-object frag)))))
	  (setq frag (frag-prev frag))
	  (setq frag-pos (1- (frag-length frag))))
	(if (frag-object-p frag)
	  (setq char (frag-object frag))
	  (if (frag-break-p frag)
	    (setq char (schar (frag-string frag) (1- (the fixnum (frag-length frag)))))
	    (setq char #\newline))))
      (if (frag-object-p frag)
	(setq char (frag-object frag))
	(setq char (schar (frag-string frag) frag-pos))))
    char))


(defun TOGGLE-SELECTION (gob value)
  ;; The cursor must be forced to redraw in the following situation:
  ;; If the multifont text is a rectangle-type fast-redraw object, and you
  ;; click on top of the current cursor position, then the line will be
  ;; invalidated without invalidating the cursor.  The cursor must be
  ;; explicitly redrawn after the multifont line is erased and redrawn.
  ;; Since the selection technically changes with each mouse click, this
  ;; function is a good place for the explicit forced update.
  (s-value (g-value gob :cursor) :force-update T)
  (unless (eq (null value) (null (g-value gob :selection-p)))
      (s-value gob :selection-p value)
      (if value
         (progn
            (s-value gob :select-line (g-value gob :cursor-line))
            (s-value gob :select-frag (g-value gob :cursor-frag))
            (s-value gob :select-position (g-value gob :cursor-position))
            (s-value gob :select-frag-pos (g-value gob :cursor-frag-pos))
         )
         (let ((cursor-line (g-value gob :cursor-line))
               (cursor-pos (g-value gob :cursor-position))
               (select-line (g-value gob :select-line))
               (select-pos (g-value gob :select-position)))
            (if (higher-cursor cursor-line cursor-pos select-line select-pos)
               (do ((my-line cursor-line (g-value my-line :next-line)))
                   ((eq my-line select-line) (turn-off-line my-line))
                  (turn-off-line my-line))
               (do ((my-line select-line (g-value my-line :next-line)))
                   ((eq my-line cursor-line) (turn-off-line my-line))
                  (turn-off-line my-line))
            )
         )
      )
   )
)


(defun SET-SELECTION-TO-X-Y-POSITION (gob x y)
  (declare (fixnum x y))
  (unless (g-value gob :selection-p)
    (TOGGLE-SELECTION gob t))
  (let (new-line)
    (do ((my-line (g-value gob :first-line) (g-value my-line :next-line)))
	((or (null my-line) (> (g-value-fixnum my-line :top) y))
	 (setq new-line
	       (if my-line
		   (or (g-value my-line :prev-line) my-line)
		   (g-value gob :last-line)
		   ))
	 )
      )
    (multiple-value-bind (frag frag-pos x-offset my-position)
	(calculate-cursor-x new-line (max 0 (- x (g-value gob :left))))
      (declare (ignore x-offset))
      (reset-selection (g-value gob :select-line)
		       (g-value gob :select-position) (g-value gob :cursor-line)
		       (g-value gob :cursor-position) new-line my-position)
      (s-value gob :select-line new-line)
      (s-value gob :select-frag frag)
      (s-value gob :select-frag-pos frag-pos)
      (s-value gob :select-position my-position)
      )
    )
  )


(defun SET-SELECTION-TO-LINE-CHAR-POSITION (gob my-line char)
  (unless (g-value gob :selection-p)
    (TOGGLE-SELECTION gob t))
  (let ((new-line (g-value gob :first-line)))
    (dotimes (i my-line)
      (when new-line
	(setq new-line (g-value new-line :next-line))))
    (unless new-line
      (setq new-line (g-value gob :last-line)))
    (setq char (min char (the fixnum (1- (g-value new-line :length)))))
    (reset-selection (g-value gob :select-line)
		     (g-value gob :select-position) (g-value gob :cursor-line)
		     (g-value gob :cursor-position) new-line char)
    (s-value gob :select-line new-line)
    (s-value gob :select-position char)
    (multiple-value-bind (frag frag-pos)
	(calculate-cursor-pos new-line char)
      (s-value gob :select-frag frag)
      (s-value gob :select-frag-pos frag-pos)
      )
    )
  )


(defun GET-SELECTION-LINE-CHAR-POSITION (gob)
   (if (g-value gob :selection-p)
      (let ((target (g-value gob :select-line)))
         (do ((my-line (g-value gob :first-line) (g-value my-line :next-line))
              (i 0 (1+ i)))
             ((eq my-line target)
	      (values i (g-value gob :select-position)))
	   (declare (fixnum i))))
      (values nil nil)))

(defun search-for-font (start-line start-frag)
  (do ((frag start-frag (frag-next frag)))
      ((null frag))
    (unless (frag-object-p frag)
      (return-from search-for-font (frag-font frag))))
  (do ((line (g-value start-line :next-line) (g-value line :next-line)))
      ((null line))
    (do ((frag (g-value line :first-frag) (frag-next frag)))
        ((null frag))
      (unless (frag-object-p frag)
        (return-from search-for-font (frag-font frag)))))
  opal:default-font)

(defun search-backwards-for-font (start-line start-frag)
  (do ((frag start-frag (frag-prev frag)))
      ((null frag))
    (unless (frag-object-p frag)
      (return-from search-backwards-for-font (frag-font frag))))
  (do ((line (g-value start-line :prev-line) (g-value line :prev-line)))
      ((null line))
    (do ((frag (g-value line :last-frag) (frag-prev frag)))
        ((null frag))
      (unless (frag-object-p frag)
        (return-from search-backwards-for-font (frag-font frag)))))
  opal:default-font)

(defun CHANGE-FONT-OF-SELECTION (gob my-font &key family size
                                 (italic :not-supplied) (bold :not-supplied))
  (if (g-value gob :selection-p)
    (let* ((cursor-line (g-value gob :cursor-line))
           (cursor-pos (g-value gob :cursor-position))
           (select-line (g-value gob :select-line))
           (select-pos (g-value gob :select-position))
           (cursor-high (higher-cursor cursor-line cursor-pos
				       select-line select-pos))
	   (frag-pos 0)
	   frag first-font first-face)
     (declare (fixnum frag-pos))
      (if cursor-high
	(progn
          (setq frag (g-value gob :cursor-frag))
          (setq frag-pos (g-value gob :cursor-frag-pos)))
	(progn
          (setq frag (g-value gob :select-frag))
          (setq frag-pos (g-value gob :select-frag-pos))))
      (when (= frag-pos (frag-length frag))
	 (setq frag (frag-next frag)))
      (setq first-font
	    (search-for-font (if cursor-high cursor-line select-line) frag))
      (setq first-face (g-value first-font :face))
      (if cursor-high
	(change-font gob cursor-line cursor-pos select-line select-pos
		     my-font family size italic bold first-face)
	(change-font gob select-line select-pos cursor-line cursor-pos
		     my-font family size italic bold first-face))
      (s-value gob :current-font (update-font first-font my-font family size
					      italic bold first-face))
      (multiple-value-bind (frag frag-pos x-offset)
			   (calculate-cursor-pos cursor-line cursor-pos)
	(s-value gob :cursor-frag frag)
	(s-value gob :cursor-frag-pos frag-pos)
	(s-value gob :cursor-x-offset x-offset))
      (multiple-value-bind (frag frag-pos)
			   (calculate-cursor-pos select-line select-pos)
	(s-value gob :select-frag frag)
	(s-value gob :select-frag-pos frag-pos))
      (if (g-value gob :word-wrap-p)
	  (let ((text-width (g-value gob :text-width)))
	    (declare (fixnum text-width))
	  (if cursor-high
	    (do ((my-line (or (g-value cursor-line :prev-line)
			      cursor-line) (g-value my-line :next-line)))
		((eq my-line (g-value gob :select-line :next-line)))
	      (if (> (g-value-fixnum my-line :width) text-width)
		(wrap-line gob my-line)
		(do ()
		    ((null (undo-wrap-line gob my-line))))))
	    (do ((my-line (or (g-value select-line :prev-line)
			      select-line) (g-value my-line :next-line)))
		((eq my-line (g-value gob :cursor-line :next-line)))
	      (if (> (g-value-fixnum my-line :width) text-width)
		(wrap-line gob my-line)
		(do ()
		    ((null (undo-wrap-line gob my-line))))))))))
    (let* ((old-font (or (g-value gob :current-font) opal:default-font))
	   (first-face (g-value old-font :face)))
      (s-value gob :current-font (update-font old-font my-font family size
					      italic bold first-face)))))


(defun search-for-color (start-line start-frag)
  (do ((frag start-frag (frag-next frag)))
      ((null frag))
    (unless (frag-object-p frag)
      (return-from search-for-color
	(values (frag-fcolor frag) (frag-bcolor frag)))))
  (do ((line (g-value start-line :next-line) (g-value line :next-line)))
      ((null line))
    (do ((frag (g-value line :first-frag) (frag-next frag)))
        ((null frag))
      (unless (frag-object-p frag)
        (return-from search-for-color
	  (values (frag-fcolor frag) (frag-bcolor frag))))))
  (values *default-color* *default-color*))

(defun search-backwards-for-color (start-line start-frag)
  (do ((frag start-frag (frag-prev frag)))
      ((null frag))
    (unless (frag-object-p frag)
      (return-from search-backwards-for-color
	(values (frag-fcolor frag) (frag-bcolor frag)))))
  (do ((line (g-value start-line :prev-line) (g-value line :prev-line)))
      ((null line))
    (do ((frag (g-value line :last-frag) (frag-prev frag)))
        ((null frag))
      (unless (frag-object-p frag)
        (return-from search-backwards-for-color
	  (values (frag-fcolor frag) (frag-bcolor frag))))))
  (values *default-color* *default-color*))

(defun CHANGE-COLOR-OF-SELECTION (gob fcolor &optional bcolor)
  (if (g-value gob :selection-p)
    (let* ((cursor-line (g-value gob :cursor-line))
           (cursor-pos (g-value gob :cursor-position))
           (select-line (g-value gob :select-line))
           (select-pos (g-value gob :select-position))
           (cursor-high (higher-cursor cursor-line cursor-pos
				       select-line select-pos))
	   (frag-pos 0)
	   frag
;;;          amickish - removed reference to first-fcolor and first-bcolor
;;;          first-fcolor first-bcolor
           )
      (declare (fixnum frag-pos))
      (if cursor-high
	(progn
          (setq frag (g-value gob :cursor-frag))
          (setq frag-pos (g-value gob :cursor-frag-pos)))
	(progn
          (setq frag (g-value gob :select-frag))
          (setq frag-pos (g-value gob :select-frag-pos))))
      (when (= frag-pos (frag-length frag))
	 (setq frag (frag-next frag)))
;;;     amickish - removed reference to first-fcolor and first-bcolor
;;;     (multiple-value-setq (first-fcolor first-bcolor)
	(search-for-color (if cursor-high cursor-line select-line)
			  frag)
;;;       )
;;;     amickish - removed reference to first-fcolor and first-bcolor
;;;     (multiple-value-setq (first-fcolor first-bcolor)
	(search-for-color (if cursor-high cursor-line select-line)
			  frag)
;;;       )
      (if cursor-high
	(change-color gob cursor-line cursor-pos select-line select-pos
		      fcolor bcolor)
	(change-color gob select-line select-pos cursor-line cursor-pos
		     fcolor bcolor))
      (s-value gob :current-fcolor fcolor)
      (s-value gob :current-bcolor bcolor)
      (multiple-value-bind (frag frag-pos x-offset)
			   (calculate-cursor-pos cursor-line cursor-pos)
	(s-value gob :cursor-frag frag)
	(s-value gob :cursor-frag-pos frag-pos)
	(s-value gob :cursor-x-offset x-offset))
      (multiple-value-bind (frag frag-pos)
			   (calculate-cursor-pos select-line select-pos)
	(s-value gob :select-frag frag)
	(s-value gob :select-frag-pos frag-pos))
      (if (g-value gob :word-wrap-p)
	  (let ((text-width (g-value gob :text-width)))
	    (declare (fixnum text-width))
	    (if cursor-high
		(do ((my-line (or (g-value cursor-line :prev-line)
				  cursor-line) (g-value my-line :next-line)))
		    ((eq my-line (g-value gob :select-line :next-line)))
		  (if (> (g-value-fixnum my-line :width) text-width)
		      (wrap-line gob my-line)
		      (do ()
			  ((null (undo-wrap-line gob my-line))))))
		(do ((my-line (or (g-value select-line :prev-line)
				  select-line) (g-value my-line :next-line)))
		    ((eq my-line (g-value gob :cursor-line :next-line)))
		  (if (> (g-value-fixnum my-line :width) text-width)
		      (wrap-line gob my-line)
		      (do ()
			  ((null (undo-wrap-line gob my-line))))))))))
    (progn
      (s-value gob :current-fcolor fcolor)
      (s-value gob :current-bcolor bcolor))))


(defun add-newline (gob)
  (let ((my-line (g-value gob :cursor-line))
	(width 0)
	frag font fcolor bcolor)
    (declare (fixnum width))
    (invalidate-line my-line)
    (break-line gob my-line (g-value gob :cursor-position) nil)
    (setq frag (g-value my-line :last-frag))
    (setq font (search-for-font my-line frag))
    (multiple-value-setq (fcolor bcolor) (search-for-color my-line frag))
    (setq width (opal:char-width font #\space))
    (if (or (null frag) (frag-object-p frag))
      (let ((new-frag (new-frag)))
        (setf (frag-line-style new-frag) NIL)
        (setf (frag-object-p new-frag) NIL)
        (setf (frag-string new-frag) " ")
        (setf (frag-length new-frag) 1)
        (setf (frag-font new-frag) font)
        (setf (frag-fcolor new-frag) fcolor)
        (setf (frag-bcolor new-frag) bcolor)
        (setf (frag-width new-frag) width)
        (setf (frag-ascent new-frag) (g-value font :max-char-ascent))
        (setf (frag-descent new-frag) (g-value font :max-char-descent))
        (setf (frag-start-highlight new-frag) 0)
        (setf (frag-end-highlight new-frag) 0)
        (setf (frag-prev new-frag) frag)
        (setf (frag-next new-frag) NIL)
        (setf (frag-break-p new-frag) NIL)
        (s-value my-line :last-frag new-frag)
        (if frag
          (setf (frag-next frag) new-frag)
          (progn
            (s-value my-line :first-frag new-frag)
            (s-value my-line :ascent (frag-ascent new-frag))
            (s-value my-line :descent (frag-descent new-frag))
            (s-value my-line :width 0)
            (s-value my-line :length 0))))
      (progn
	(setf (frag-string frag)
	      (concatenate 'string (the simple-string (frag-string frag)) " "))
        (incf (frag-length frag))
        (incf (frag-width frag) width)))
    (incf (g-value my-line :length))
    (incf (g-value my-line :width) width)
    (when (g-value gob :word-wrap-p)
      (wrap-line gob (g-value my-line :next-line))
      (let ((prev-line (g-value my-line :prev-line)))
	(when prev-line
	  (undo-wrap-line gob prev-line))))))


(defun reset-ascent-descent (my-line)
  (let ((ascent 0) (descent 0))
    (declare (fixnum ascent descent))
    (do ((frag (g-value my-line :first-frag) (frag-next frag)))
        ((null frag))
      (setq ascent (max ascent (or (frag-ascent frag) 0)))
      (setq descent (max descent (or (frag-descent frag) 0))))
    (s-value my-line :ascent ascent)
    (s-value my-line :descent descent)))

(defun new-frag-with-font (my-font fcolor bcolor my-line prev-frag next-frag)
  (let ((new-frag (new-frag)))
    (setf (frag-line-style new-frag) NIL)
    (setf (frag-start-highlight new-frag) 0)
    (setf (frag-end-highlight new-frag) 0)
    (setf (frag-prev new-frag) prev-frag)
    (setf (frag-next new-frag) next-frag)
    (if prev-frag
      (progn
        (setf (frag-break-p new-frag) (frag-break-p prev-frag))
	(setf (frag-next prev-frag) new-frag))
      (s-value my-line :first-frag new-frag))
    (if next-frag
      (setf (frag-prev next-frag) new-frag)
      (s-value my-line :last-frag new-frag))
    (if (or (is-a-p my-font opal:font) (is-a-p my-font opal:font-from-file))
      (progn
        (setf (frag-object-p new-frag) NIL)
        (setf (frag-string new-frag) "")
        (setf (frag-length new-frag) 0)
        (setf (frag-font new-frag) my-font)
	(setf (frag-fcolor new-frag) fcolor)
	(setf (frag-bcolor new-frag) bcolor)
        (setf (frag-width new-frag) 0)
        (setf (frag-ascent new-frag) (g-value my-font :max-char-ascent))
        (setf (frag-descent new-frag) (g-value my-font :max-char-descent)))
      (progn
        (setf (frag-object-p new-frag) T)
        (setf (frag-object new-frag) my-font)
        (setf (frag-font new-frag) NIL)
	(setf (frag-fcolor new-frag) NIL)
	(setf (frag-bcolor new-frag) NIL)
        (if (mark-p my-font)
	    (setf (frag-length new-frag) 0
		  (frag-width new-frag) 0
		  (frag-ascent new-frag) 0)
	    (setf (frag-length new-frag) 1
		  (frag-width new-frag) (g-value my-font :width)
		  (frag-ascent new-frag) (g-value my-font :height)))
        (setf (frag-descent new-frag) 0)))
    (reset-ascent-descent my-line)
    new-frag))

(defun create-frag-openning (my-line frag frag-position my-font fcolor bcolor)
  (declare (fixnum frag-position))
  (cond
    ((and (not (frag-object-p frag)) (eq my-font (frag-font frag))
	  (eq fcolor (frag-fcolor frag)) (eq bcolor (frag-bcolor frag)))
        (values frag frag-position))
    ((zerop frag-position)
        (let ((prev-frag (frag-prev frag)))
          (if (and prev-frag (not (frag-object-p prev-frag))
		   (eq my-font (frag-font prev-frag))
		   (eq fcolor (frag-fcolor frag))
		   (eq bcolor (frag-bcolor frag)))
	    (values prev-frag (frag-length prev-frag))
	    (let ((obj (and prev-frag (frag-object prev-frag))))
	      (if (and (mark-p obj) (not (mark-sticky-left obj)))
		(let ((prev-prev (frag-prev prev-frag)))
		  (if prev-prev
		    (create-frag-openning my-line prev-prev
					  (frag-length prev-prev)
					  my-font fcolor bcolor)
		    (values (new-frag-with-font my-font fcolor bcolor
						my-line nil prev-frag) 0)))
		(values (new-frag-with-font my-font fcolor bcolor
					    my-line prev-frag frag) 0))))))
    ((= frag-position (frag-length frag))
        (let ((next-frag (frag-next frag)))
          (if (and next-frag (not (frag-object-p next-frag))
		   (eq my-font (frag-font next-frag))
		   (eq fcolor (frag-fcolor frag))
		   (eq bcolor (frag-bcolor frag)))
	    (values next-frag 0)
	    (let ((obj (frag-object next-frag)))
	      (if (and (mark-p obj) (mark-sticky-left obj))
		(let ((next-next (frag-next next-frag)))
		  (if next-next
		    (create-frag-openning my-line next-next 0
					  my-font fcolor bcolor)
		    (values (new-frag-with-font my-font fcolor bcolor
						my-line next-frag nil) 0)))
		(values (new-frag-with-font my-font fcolor bcolor
					    my-line frag next-frag) 0))))))
    (T
        (let ((right-frag (split-frag frag frag-position)))
          (when (eq frag (g-value my-line :last-frag))
            (s-value my-line :last-frag right-frag))
          (values (new-frag-with-font my-font fcolor bcolor
				      my-line frag right-frag) 0)))))

(defun set-object-left (my-line)
  (do ((left 0)
       (frag (g-value my-line :first-frag) (frag-next frag)))
      ((null frag))
    (declare (fixnum left))
    (when (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(when (not (mark-p obj))
	  (s-value obj :multifont-x-offset left))))
    (incf left (frag-width frag))))

(defun ADD-CHAR (gob char &optional new-font new-fcolor new-bcolor lisp-mode-p)
  (if lisp-mode-p
    (inter::add-lisp-char gob char new-font new-fcolor new-bcolor)
    (progn
      (TOGGLE-SELECTION gob nil)
      (if (eq char #\newline)
	  (add-newline gob)
	  (when (and (characterp char) (graphic-char-p char))
	    (let* ((my-line (g-value gob :cursor-line))
		   (frag (g-value gob :cursor-frag))
		   (frag-pos (g-value gob :cursor-frag-pos))
		   (my-font (or new-font (g-value gob :current-font)
				(search-for-font my-line frag)))
		   (char-size (the fixnum (opal:char-width my-font char)))
		   (my-fcolor (or new-fcolor (g-value gob :current-fcolor)))
		   (my-bcolor (or new-bcolor (g-value gob :current-bcolor))))
	      (if (frag-object-p frag)
		  (multiple-value-setq (my-fcolor my-bcolor)
		    (search-for-color my-line frag)))
	      (invalidate-line my-line)
	      (multiple-value-setq (frag frag-pos)
		(create-frag-openning my-line frag frag-pos
				      my-font my-fcolor my-bcolor))
	      (setf (frag-string frag)
		    (let ((old-string (frag-string frag)))
		      (declare (simple-string old-string))
		      (concatenate 'string
				   (subseq old-string 0 frag-pos) (string char)
				   (subseq old-string frag-pos))))
	      (incf (frag-length frag))
	      (incf (g-value-fixnum my-line :length))
	      (incf (g-value-fixnum gob :cursor-position))
	      (incf (the fixnum (frag-width frag)) char-size)
	      (incf (g-value-fixnum my-line :width) char-size)
	      (incf (g-value-fixnum gob :cursor-x-offset) char-size)
	      (s-value gob :cursor-frag frag)
	      (s-value gob :cursor-frag-pos (1+ frag-pos))
	      (set-object-left my-line)
	      (when (g-value gob :word-wrap-p)
		(wrap-line gob my-line)
		(let ((prev-line (g-value my-line :prev-line)))
		  (when prev-line
		    (undo-wrap-line gob prev-line))))))))))

(defun remove-object (gob object)
  (let ((prev-object (g-value object :mf-prev-object))
	(next-object (g-value object :mf-next-object)))
    (opal:remove-component gob object)
    (setf (frag-object-p (g-value object :multifont-frag)) nil)
    (if prev-object
      (s-value prev-object :mf-next-object next-object)
      (s-value gob :first-object next-object))
    (if next-object
      (s-value next-object :mf-prev-object prev-object)
      (s-value gob :last-object prev-object)))
  object)

;; Deletes a character from text at the position of its cursor.
;; returns that character, or nil if cursor was at end of text.
(defun DELETE-CHAR (gob)
  (TOGGLE-SELECTION gob nil)
  (let* ((my-line (g-value gob :cursor-line))
         (next-line (g-value my-line :next-line))
         (frag (g-value gob :cursor-frag))
         (frag-pos (g-value gob :cursor-frag-pos)))
    (declare (fixnum frag-pos))
    (invalidate-line my-line)
    (when (= frag-pos (frag-length frag))
      (setq frag (frag-next frag))
      (setq frag-pos 0)
      (do ((obj (and frag (frag-object frag)) (and frag (frag-object frag)))
	   (removed-mark nil removed-mark))
	  ((or (null frag) (not (mark-p obj)))
	   (when removed-mark
	     (calculate-size-of-line gob my-line)
	     (setq frag-pos (g-value gob :cursor-frag-pos))
	     (setq frag (g-value gob :cursor-frag))))
	(when (not (mark-sticky-left obj))
	  (remove-mark gob obj)
	  (setq removed-mark t))
	(setq frag (frag-next frag))
	(setq frag-pos 0)))
    (when (= frag-pos (1- (frag-length frag)))
      (do* ((my-frag (frag-next frag) (frag-next my-frag))
	    (obj (and my-frag (frag-object my-frag))
		 (and my-frag (frag-object my-frag)))
	    (removed-mark nil removed-mark))
	   ((not (mark-p obj))
	    (when removed-mark
	      (calculate-size-of-line gob my-line)
	      (setq frag-pos (g-value gob :cursor-frag-pos))
	      (setq frag (g-value gob :cursor-frag))))
	(when (mark-sticky-left obj)
	  (remove-mark gob obj)
	  (setq removed-mark t))))
    (when (= frag-pos (frag-length frag))
      (setq frag (frag-next frag))
      (setq frag-pos 0))
    (if (and (null next-line) (>= frag-pos (1- (frag-length frag)))
             (null (frag-next frag)))
      nil
      (let (char char-size)
	(if (frag-object-p frag)
          (progn
            (setq char (remove-object gob (frag-object frag)))
            (setq char-size (frag-width frag)))
          (progn
            (setq char (schar (frag-string frag) frag-pos))
            (setq char-size (opal:char-width (frag-font frag) char))
            (setf (frag-string frag)
		(let ((old-string (frag-string frag)))
                  (concatenate 'string (subseq old-string 0 frag-pos)
                               (subseq old-string (1+ frag-pos)))))))
        (decf (frag-length frag))
        (decf (g-value my-line :length))
        (decf (frag-width frag) char-size)
        (decf (g-value my-line :width) char-size)
	(let ((length (frag-length frag)))
	  (when (and (= length 0)        ; ***** if -> when
		     (frag-next frag))
	    (calculate-size-of-line gob my-line)
	    (setq frag (g-value gob :cursor-frag))
	    (setq frag-pos length)))
	(when (and (= frag-pos (frag-length frag)) (null (frag-next frag)))
	  (unless (frag-break-p frag)
	    (setq char #\newline))
	  (merge-lines gob my-line next-line)
	  (setq next-line (g-value my-line :next-line)))
	(set-object-left my-line)
        (when (g-value gob :word-wrap-p)
          (undo-wrap-line gob my-line)
	  (let ((prev-line (g-value my-line :prev-line)))
	    (when prev-line
	      (undo-wrap-line gob prev-line))))
        (reset-font gob)
        char))))


(defun DELETE-PREV-CHAR (gob)
  (TOGGLE-SELECTION gob nil)
  (let* ((cursor-line (g-value gob :cursor-line))
	 (my-line cursor-line)
	 (frag (g-value gob :cursor-frag))
	 (frag-pos (1- (g-value gob :cursor-frag-pos)))
	 (char-size 0)
	 char)
    (declare (fixnum frag-pos char-size))
    ;; Don't invalidate cursor-line yet.  First check to see if we are at
    ;; the very beginning of an empty multifont string (i.e., at beginning
    ;; of first frag).
    (when (< frag-pos 0)
      (do ((prev-frag (frag-prev frag) (frag-prev frag))
	   (removed-mark nil removed-mark))
	  ((or (null prev-frag) (>= frag-pos 0))
	   (when removed-mark
	     (calculate-size-of-line gob my-line)
	     (setq frag-pos (1- (g-value gob :cursor-frag-pos)))
	     (setq frag (g-value gob :cursor-frag))
	     (setq prev-frag (frag-prev frag))
	     (when (and (< frag-pos 0) prev-frag)
	       (s-value gob :cursor-frag (setq frag prev-frag))
	       (let ((len (frag-length frag)))
		 (s-value gob :cursor-frag-pos len)
		 (setq frag-pos (1- len))))))
	(let ((obj (frag-object prev-frag)))
	  (when (and (mark-p obj) (mark-sticky-left obj))
	    (remove-mark gob obj)
	    (setq removed-mark t)))
	(setq frag prev-frag)
	(setq frag-pos (1- (frag-length frag)))))
    (when (= frag-pos 0)
      (do* ((prev-frag (frag-prev frag) (frag-prev prev-frag))
	    (obj (and prev-frag (frag-object prev-frag))
		 (and prev-frag (frag-object prev-frag)))
	    (removed-mark nil removed-mark))
	   ((not (mark-p obj))
	    (when removed-mark
	      (calculate-size-of-line gob my-line)
	      (setq frag-pos (1- (g-value gob :cursor-frag-pos)))
	      (setq frag (g-value gob :cursor-frag))))
	(when (not (mark-sticky-left obj))
	  (remove-mark gob obj)
	  (setq removed-mark t))))
    (when (< frag-pos 0)
      (unless (setq my-line (g-value my-line :prev-line))
        (return-from delete-prev-char nil))
      (s-value gob :cursor-line my-line)
      (s-value gob :cursor-frag (setq frag (g-value my-line :last-frag)))
      (s-value gob :cursor-position (g-value my-line :length))
      (setq frag-pos (1- (frag-length frag)))
      (s-value gob :cursor-x-offset (g-value my-line :width)))
    ;; At this point, we are definitely deleting a character or a frag,
    ;; so invalidate the line where the cursor started.
    (invalidate-line cursor-line)
    (if (frag-object-p frag)
      (progn
        (setq char (remove-object gob (frag-object frag)))
        (setq char-size (frag-width frag)))
      (progn
        (setq char (schar (frag-string frag) frag-pos))
        (setq char-size (opal:char-width (frag-font frag) char))
        (setf (frag-string frag)
	    (let ((old-string (frag-string frag)))
              (concatenate 'string (subseq old-string 0 frag-pos)
                           (subseq old-string (1+ frag-pos)))))))
    (decf (frag-length frag))
    (decf (g-value my-line :length))
    (decf (frag-width frag) char-size)
    (decf (g-value my-line :width) char-size)
    (if (and (= frag-pos (frag-length frag)) (null (frag-next frag)))
      (let ((next-line (g-value my-line :next-line)))
	(when (zerop (frag-length frag))
	  (do* ((prev (frag-prev frag) (frag-prev prev))
	        (obj (and prev (frag-object prev))
		     (and prev (frag-object prev)))
		(removed-mark nil removed-mark))
	       ((or (not (mark-p obj)) (mark-sticky-left obj))
		(when removed-mark
		  (calculate-size-of-line gob my-line)))
	    (when (mark-p obj)
	      (remove-mark gob obj)
	      (setq removed-mark t))))
        (s-value gob :cursor-line next-line)
        (s-value gob :cursor-frag (g-value next-line :first-frag))
        (s-value gob :cursor-frag-pos 0)
        (s-value gob :cursor-position 0)
        (s-value gob :cursor-x-offset 0)
        (unless (frag-break-p frag)
          (setq char #\newline)
          (merge-lines gob my-line next-line)))
      (let* ((next-frag (frag-next frag))
	     (obj (and next-frag (frag-object next-frag))))
	(if (and (= frag-pos (frag-length frag))
		 (and (mark-p obj) (mark-sticky-left obj)))
	  (do ((next-next (frag-next next-frag) (frag-next next-next)))
	      ((not (mark-p (frag-object next-next)))
	       (s-value gob :cursor-frag (setq frag next-next))
	       (setq frag-pos 0)))
	  (let ((prev-frag (frag-prev frag)))
	    (when (and (= frag-pos 0) prev-frag)
	      (let ((hold frag))
		(when (let ((obj (frag-object prev-frag)))
			(or (not (mark-p obj)) (not (mark-sticky-left obj))))
		  (do () ((not (mark-p (frag-object prev-frag))))
		    (setf prev-frag (frag-prev prev-frag)))
		  (s-value gob :cursor-frag (setq frag prev-frag))
		  (setq frag-pos (frag-length frag)))
		(when (= (frag-length hold) 0)
		  (calculate-size-of-line gob my-line)
		  (setq frag (g-value gob :cursor-frag)))))))
	(s-value gob :cursor-frag-pos frag-pos)
	(decf (g-value gob :cursor-position))
	(decf (g-value gob :cursor-x-offset) char-size)))
    (set-object-left my-line)
    (when (g-value gob :word-wrap-p)
      (undo-wrap-line gob my-line)
      (let ((prev-line (g-value my-line :prev-line)))
	(when prev-line
	  (undo-wrap-line gob prev-line))))
    (reset-font gob)
    char))


(defun INSERT-STRING (gob str &optional new-font new-fcolor new-bcolor)
  (declare (simple-string str))
  (let ((pos (position #\newline str)))
    (if pos
      (progn
        (INSERT-STRING gob (subseq str 0 pos) new-font)
        (add-newline gob)
        (INSERT-STRING gob (subseq str (1+ pos)) new-font))
      (when (> (length str) 0)
        (TOGGLE-SELECTION gob nil)
        (let* ((my-line (g-value gob :cursor-line))
               (frag (g-value gob :cursor-frag))
               (frag-pos (g-value gob :cursor-frag-pos))
               (my-font (or new-font (g-value gob :current-font)
			    (search-for-font my-line frag)))
               (text-size (opal:string-width
			   my-font str
			   :display (g-local-value gob :window)))
               (str-length (length str))
	       my-fcolor my-bcolor)
	  (if (or (null new-fcolor) (null new-bcolor))
	      (progn
		(multiple-value-setq (my-fcolor my-bcolor)
		  (search-for-color my-line frag))
		(setf my-fcolor (or new-fcolor my-fcolor))
		(setf my-bcolor (or new-bcolor my-bcolor)))
	      (setq my-fcolor new-fcolor
		    my-bcolor new-bcolor))
          (invalidate-line my-line)
          (multiple-value-setq (frag frag-pos)
	      (create-frag-openning my-line frag frag-pos
				    my-font my-fcolor my-bcolor))
          (setf (frag-string frag)
		(let ((old-string (frag-string frag)))
		  (concatenate 'string
			       (subseq old-string 0 frag-pos) str
			       (subseq old-string frag-pos))))
          (incf (frag-length frag) str-length)
          (incf (g-value my-line :length) str-length)
          (incf (g-value gob :cursor-position) str-length)
          (incf (frag-width frag) text-size)
          (incf (g-value my-line :width) text-size)
          (incf (g-value gob :cursor-x-offset) text-size)
          (s-value gob :cursor-frag-pos (+ frag-pos str-length))
          (s-value gob :cursor-frag frag)
          (set-object-left my-line)
          (when (g-value gob :word-wrap-p)
            (wrap-line gob my-line)
	    (let ((prev-line (g-value my-line :prev-line)))
	      (when prev-line
		(undo-wrap-line gob prev-line)))))))))


(defun ADD-OBJECT (gob object)
  (TOGGLE-SELECTION gob nil)
  (when (is-a-p object opal:view-object)
    (let* ((my-line (g-value gob :cursor-line))
           (frag (g-value gob :cursor-frag))
           (frag-pos (g-value gob :cursor-frag-pos))
	   (width (g-value object :width)))
      (declare (fixnum frag-pos width))
      (invalidate-line my-line)
      (multiple-value-setq (frag frag-pos)
	  (create-frag-openning my-line frag frag-pos object nil nil))
      (if (null (frag-prev frag))
	  (let ((font (search-backwards-for-font my-line frag))
		fcolor bcolor)
	    (multiple-value-setq (fcolor bcolor)
	      (search-backwards-for-color my-line frag))
	    (new-frag-with-font font fcolor bcolor my-line nil frag)))
      (incf (g-value my-line :length))
      (incf (g-value my-line :width) width)
      (incf (g-value gob :cursor-position))
      (incf (g-value gob :cursor-x-offset) width)
      (s-value gob :cursor-frag frag)
      (s-value gob :cursor-frag-pos (1+ frag-pos))
      (s-value object :multifont-object gob)
      (s-value object :multifont-line my-line)
      (s-value object :multifont-frag frag)
      (s-value object :top (o-formula (gvl :multifont-line :top)))
      (s-value object :left (o-formula (+ (gvl-fixnum :multifont-line :left)
	                                  (gvl-fixnum :multifont-x-offset))))
      (s-value object :mf-prev-object NIL)
      (let ((first-object (g-value gob :first-object)))
        (s-value object :mf-next-object first-object)
	(if first-object
          (s-value first-object :mf-prev-object object)
	  (s-value gob :last-object object)))
      (s-value gob :first-object object)
      (set-object-left my-line)
      (opal:add-component gob object :back)
      (when (g-value gob :word-wrap-p)
        (wrap-line gob my-line)
        (let ((prev-line (g-value my-line :prev-line)))
	  (when prev-line
	    (undo-wrap-line gob prev-line)))))))

(defun search-for-mark-from (line frag &key name info)
  (do ((obj (and frag (frag-object frag)) (and frag (frag-object frag))))
       ((or (null line) (mark-p obj))
	(do ((mark obj (mark-next mark)))
	    ((or (null mark) (and (mark-p mark)
				(or (not name) (equal name (mark-name mark)))
				(or (not info) (equal info (mark-info mark)))))
	     mark)))
    (setf frag (frag-next frag))
    (when (null frag)
      (setf line (g-value line :next-line))
      (when line
	(setf frag (g-value line :first-frag))))))

(defun SEARCH-FOR-MARK (gob &key name info)
  (search-for-mark-from (g-value gob :cursor-line) (g-value gob :cursor-frag)
			:name name :info info))

(defun search-backwards-for-mark-from (line frag &key name info)
  (do ((obj (and frag (frag-object frag)) (and frag (frag-object frag))))
       ((or (null line) (mark-p obj))
	(do ((mark obj (mark-prev mark)))
	    ((or (null mark)
		 (and (mark-p obj)
		      (or (not name) (equal name (mark-name mark)))
		      (or (not info) (equal info (mark-info mark)))))
	     mark)))
    (setf frag (frag-prev frag))
    (when (null frag)
      (setf line (g-value line :prev-line))
      (when line
	(setf frag (g-value line :last-frag))))))

(defun SEARCH-BACKWARDS-FOR-MARK (gob &key name info)
  (search-backwards-for-mark-from (g-value gob :cursor-line)
				  (g-value gob :cursor-frag)
				  :name name :info info))

(defun BETWEEN-MARKS-P (gob &key name info)
  (let* ((line (g-value gob :cursor-line))
	 (frag (g-value gob :cursor-frag))
	 (prev-mark (search-backwards-for-mark-from line frag
						    :name name :info info)))
    (and prev-mark (mark-sticky-left prev-mark)
	 (let ((next-mark (search-for-mark-from line frag
						:name name :info info)))
	   (and next-mark (not (mark-sticky-left next-mark)))))))

(defun MARK (&key sticky-left name info)
  (let ((mark (new-mark)))
    (setf (mark-sticky-left mark) sticky-left
	  (mark-name mark) name
	  (mark-info mark) info
	  (mark-frag mark) nil
	  (mark-next mark) nil
	  (mark-prev mark) nil
	  (mark-line mark) nil)
    mark))

(defun INSERT-MARK (gob sticky-left &key name info)
  (let* ((my-line (g-value gob :cursor-line))
	 (frag (g-value gob :cursor-frag))
	 (frag-pos (g-value gob :cursor-frag-pos))
	 (mark (MARK :sticky-left sticky-left :name name :info info)))
    (invalidate-line my-line)
    (let* ((prev (search-backwards-for-mark-from my-line frag))
	   (next (if prev (mark-next prev) (g-value gob :first-mark))))
      (setf (mark-prev mark) prev)
      (setf (mark-next mark) next)
      (if prev
	(setf (mark-next prev) mark)
	(s-value gob :first-mark mark))
      (if next
	(setf (mark-prev next) mark)
	(s-value gob :last-mark mark)))
    (multiple-value-setq (frag frag-pos)
      (create-frag-openning my-line frag frag-pos mark nil nil))
    (setf (mark-frag mark) frag)
    (setf (mark-line mark) my-line)
    (let ((prev (frag-prev frag)))
      (if (or (null prev)
	      (let* ((obj (frag-object prev))
		     (obj-sticky-left (and (mark-p obj)
					  (mark-sticky-left obj))))
		(and (mark-p obj) (not sticky-left) obj-sticky-left)))
	  (let ((font (search-backwards-for-font my-line frag))
		fcolor bcolor)
	    (multiple-value-setq (fcolor bcolor)
	      (search-backwards-for-color my-line frag))
	    (new-frag-with-font font fcolor bcolor my-line prev frag))
	  (let* ((next (frag-next frag))
		 (obj (and next (frag-object next)))
		 (obj-sticky-left (and (mark-p obj) (mark-sticky-left obj))))
	    (when (and (mark-p obj) sticky-left (not obj-sticky-left))
	      (let ((font (search-backwards-for-font my-line frag))
		    fcolor bcolor)
		(multiple-value-setq (fcolor bcolor)
		  (search-backwards-for-color my-line frag))
		(new-frag-with-font font fcolor bcolor my-line frag next))))))
    (if sticky-left
      (progn
	(s-value gob :cursor-frag (frag-next frag))
	(s-value gob :cursor-frag-pos 0))
      (let ((prev (frag-prev frag)))
	(s-value gob :cursor-frag prev)
	(s-value gob :cursor-frag-pos (frag-length prev))))
    mark)
  )

(defun remove-mark (gob mark)
  (let ((prev-mark (mark-prev mark))
	(next-mark (mark-next mark)))
    (setf (frag-object-p (mark-frag mark)) nil)
    (free-mark mark)
    (if prev-mark
      (setf (mark-next prev-mark) next-mark)
      (s-value gob :first-mark next-mark))
    (if next-mark
      (setf (mark-prev next-mark) prev-mark)
      (s-value gob :last-mark prev-mark)))
  (list :mark (mark-sticky-left mark) (mark-name mark) (mark-info mark)))

(defun insert-line (gob my-line)
  (cond
    ((stringp my-line) (INSERT-STRING gob my-line))
    ((is-a-p my-line opal:view-object) (ADD-OBJECT gob my-line))
    (T (dolist (frag my-line)
         (cond
	   ((stringp frag) (INSERT-STRING gob frag))
           ((is-a-p frag opal:view-object) (ADD-OBJECT gob frag))
           (T (let ((first-item (car frag))
		    (specs-list (cdr frag)))
		(if (eq first-item :mark)
		  (let ((sticky-left (first specs-list)))
		    (INSERT-MARK gob sticky-left :name (second specs-list)
				 :info (third specs-list))
		    (when (not sticky-left)
		      (s-value gob :cursor-frag
			    (frag-next (frag-next (g-value gob :cursor-frag))))
		      (s-value gob :cursor-frag-pos 0)))
		  (if (listp specs-list)
		    (let ((fcolor (third frag))
			  (bcolor (fourth frag))
			  line-style)
		      (when (not (and fcolor bcolor))
			(setq line-style (g-value gob :line-style)))
		      (INSERT-STRING gob (car frag) (second frag)
				     (or fcolor (g-value line-style
							 :foreground-color))
				     (or bcolor (g-value line-style
							 :background-color))))
		    (INSERT-STRING gob (car frag) specs-list))))))))))

(defun INSERT-TEXT (gob text)
  (check-text text)
  (if (stringp text)
    (INSERT-STRING gob text)
    (do ((my-line (pop text) (pop text)))
        ((null text)
            (insert-line gob my-line))
      (insert-line gob my-line)
      (add-newline gob))))


(defun DELETE-SUBSTRING (gob start-line start-char end-line end-char)
   (SET-CURSOR-TO-LINE-CHAR-POSITION gob start-line start-char)
   (SET-SELECTION-TO-LINE-CHAR-POSITION gob end-line end-char)
   (TEXT-TO-STRING (DELETE-SELECTION gob))
)


;; Returns word deleted.
(defun DELETE-WORD (gob)
  (TOGGLE-SELECTION gob nil)
  (let ((str ""))
    (declare (simple-string str))
    (do ((space (DELETE-CHAR gob) (DELETE-CHAR gob)))
        ((or (null space) (not (characterp space))
             (not (delim-char-p space)))
            (do ((char space (DELETE-CHAR gob))
                 (next-char (FETCH-NEXT-CHAR gob) (FETCH-NEXT-CHAR gob)))
                ((or (null char)
		     (and (characterp next-char)
			  (delim-char-p next-char)))
                    (concatenate 'string str
                                 (cond
                                   ((null char) "")
                                   ((characterp char) (string char))
                                   (T (format NIL "~s" char)))))
              (setq str (concatenate 'string str
                                     (if (characterp char)
                                       (string char)
                                       (format NIL "~s" char))))))
      (setq str (concatenate 'string str (string space))))))


;; Returns word deleted.
(defun DELETE-PREV-WORD (gob)
  (TOGGLE-SELECTION gob nil)
  (let ((str ""))
    (declare (simple-string str))
    (do ((space (DELETE-PREV-CHAR gob) (DELETE-PREV-CHAR gob)))
        ((or (null space) (not (characterp space))
             (not (delim-char-p space)))
            (do ((char space (DELETE-PREV-CHAR gob))
                 (prev-char (FETCH-PREV-CHAR gob) (FETCH-PREV-CHAR gob)))
                ((or (null char)
		     (and (characterp prev-char)
			  (delim-char-p prev-char)))
                    (concatenate 'string
                                 (cond
                                   ((null char) "")
                                   ((characterp char) (string char))
                                   (T (format NIL "~s" char))) str))
              (setq str (concatenate 'string
                                     (if (characterp char)
                                       (string char)
                                       (format NIL "~s" char)) str))))
      (setq str (concatenate 'string (string space) str)))))


(defun copy-frag-mid (frag start-pos end-pos)
  (declare (fixnum start-pos end-pos))
  (if (= start-pos end-pos)
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	  (list :mark (mark-sticky-left obj) (mark-name obj) (mark-info obj))
	  obj))
      (list (subseq (frag-string frag) start-pos end-pos)
	    (frag-font frag) (frag-fcolor frag) (frag-bcolor frag)))))

(defun delete-frag-mid (gob frag start-pos end-pos)
  (declare (fixnum start-pos end-pos))
  (if (= start-pos end-pos)
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	  (remove-mark gob obj)
	  (progn
	    (setf (frag-length frag) 0)
	    (remove-object gob obj))))
      (let* ((orig-string (frag-string frag))
             (str (subseq orig-string start-pos end-pos))
             (new-string (concatenate 'string (subseq orig-string 0 start-pos)
                                      (subseq orig-string end-pos)))
             (my-font (frag-font frag))
	     (my-fcolor (frag-fcolor frag))
	     (my-bcolor (frag-bcolor frag)))
        (setf (frag-string frag) new-string)
        (setf (frag-length frag) (length new-string))
        (setf (frag-width frag)
	      (opal:string-width my-font new-string
				 :display (g-local-value gob :window)))
        (list str my-font my-fcolor my-bcolor)))))

(defun copy-frag-left (frag frag-position)
  (declare (fixnum frag-position))
  (if (zerop frag-position)
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	    (list :mark (mark-sticky-left obj) (mark-name obj) (mark-info obj))
	  obj))
      (list (subseq (frag-string frag) 0 frag-position)
	    (frag-font frag) (frag-fcolor frag) (frag-bcolor frag)))))

(defun delete-frag-left (gob frag frag-position)
  (declare (fixnum frag-position))
  (if (zerop frag-position)
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	  (remove-mark gob obj)
	  (progn
	    (setf (frag-length frag) 0)
	    (remove-object gob obj))))
      (let* ((orig-string (frag-string frag))
             (str (subseq orig-string 0 frag-position))
             (new-string (subseq orig-string frag-position))
             (my-font (frag-font frag))
	     (my-fcolor (frag-fcolor frag))
	     (my-bcolor (frag-bcolor frag)))
        (setf (frag-string frag) new-string)
        (setf (frag-length frag) (length new-string))
        (setf (frag-width frag)
	      (opal:string-width my-font new-string
				 :display (g-local-value gob :window)))
        (list str my-font my-fcolor my-bcolor)))))


(defun copy-frag-right (frag frag-position)
  (declare (fixnum frag-position))
  (if (= frag-position (frag-length frag))
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	    (list :mark (mark-sticky-left obj) (mark-name obj) (mark-info obj))
	    obj))
      (list (subseq (frag-string frag) frag-position)
	    (frag-font frag) (frag-fcolor frag) (frag-bcolor frag)))))

(defun delete-frag-right (gob frag frag-position)
  (declare (fixnum frag-position))
  (if (= frag-position (frag-length frag))
    (list "" opal:default-font *default-color* *default-color*)
    (if (frag-object-p frag)
      (let ((obj (frag-object frag)))
	(if (mark-p obj)
	  (remove-mark gob obj)
	  (progn
	    (setf (frag-length frag) 0)
	    (remove-object gob obj))))
      (let* ((orig-string (frag-string frag))
             (str (subseq orig-string frag-position))
             (new-string (subseq orig-string 0 frag-position))
             (my-font (frag-font frag))
	     (my-fcolor (frag-fcolor frag))
	     (my-bcolor (frag-bcolor frag)))
        (setf (frag-string frag) new-string)
        (setf (frag-length frag) (length new-string))
        (setf (frag-width frag)
	      (opal:string-width my-font new-string
				 :display (g-local-value gob :window)))
        (list str my-font my-fcolor my-bcolor)))))

(defun copy-the-frag (frag)
  (if (frag-object-p frag)
    (let ((obj (frag-object frag)))
      (if (mark-p obj)
	  (list :mark (mark-sticky-left obj) (mark-name obj) (mark-info obj))
	obj))
    (list (frag-string frag) (frag-font frag)
	  (frag-fcolor frag) (frag-bcolor frag))))

(defun delete-frag (gob frag)
  (if (frag-object-p frag)
    (let ((obj (frag-object frag)))
      (if (mark-p obj)
	(remove-mark gob obj)
	(remove-object gob obj)))
    (list (frag-string frag) (frag-font frag)
	  (frag-fcolor frag) (frag-bcolor frag))))

(defun copy-line-mid (start-frag start-pos end-frag end-pos)
  (let ((out NIL))
    (if (eq start-frag end-frag)
      (setq out (list (copy-frag-mid start-frag start-pos end-pos)))
      (progn
        (setq out (list (copy-frag-left end-frag end-pos)))
        (do ((frag (frag-prev end-frag) (frag-prev frag)))
            ((eq frag start-frag))
          (push (copy-the-frag frag) out))
        (push (copy-frag-right start-frag start-pos) out)))
    out))

(defun delete-line-mid (gob line start-frag start-pos end-frag end-pos)
  (let ((out NIL))
    (if (eq start-frag end-frag)
      (setq out (list (delete-frag-mid gob start-frag start-pos end-pos)))
      (progn
        (setq out (list (delete-frag-left gob end-frag end-pos)))
        (do ((frag (frag-prev end-frag) (frag-prev frag)))
            ((eq frag start-frag))
          (push (delete-frag gob frag) out))
        (push (delete-frag-right gob start-frag start-pos) out)
        (unless (eq (frag-next start-frag) end-frag)
          (free-frag-line (frag-next start-frag) (frag-prev end-frag)))
        (setf (frag-next start-frag) end-frag)
        (setf (frag-prev end-frag) start-frag)))
    (calculate-size-of-line gob line)
    out))

(defun copy-line-left (frag frag-position)
  (let ((out (list (copy-frag-left frag frag-position))))
    (do ((cur-frag (frag-prev frag) (frag-prev cur-frag)))
        ((null cur-frag))
      (push (copy-the-frag cur-frag) out))
    out))

(defun delete-line-left (gob line frag frag-position)
  (let ((out (list (delete-frag-left gob frag frag-position)))
        (first-frag (g-value line :first-frag)))
    (do ((cur-frag (frag-prev frag) (frag-prev cur-frag)))
        ((null cur-frag))
      (push (delete-frag gob cur-frag) out))
    (unless (eq frag first-frag)
      (free-frag-line first-frag (frag-prev frag))
      (setf (frag-prev frag) NIL)
      (s-value line :first-frag frag))
    (calculate-size-of-line gob line)
    out))

(defun copy-line-right (line frag frag-position)
  (let* ((last-frag (g-value line :last-frag))
         (break-p (frag-break-p last-frag)))
    (if break-p
      (values (copy-line-mid frag frag-position last-frag
                             (1- (frag-length last-frag))) T)
      (let (out)
        (do ((cur-frag last-frag (frag-prev cur-frag)))
            ((eq cur-frag frag))
          (push (copy-the-frag cur-frag) out))
        (values (push (copy-frag-right frag frag-position) out) NIL)))))

(defun delete-line-right (gob line frag frag-position)
  (let* ((last-frag (g-value line :last-frag))
         (break-p (frag-break-p last-frag))
         (out NIL))
    (cond
      (break-p
          (do ((cur-frag last-frag (frag-prev cur-frag)))
              ((eq cur-frag frag))
            (push (delete-frag gob cur-frag) out))
          (push (delete-frag-right gob frag frag-position) out))
      ((eq frag last-frag)
          (setq out (list (copy-frag-mid frag frag-position
                                         (1- (frag-length frag)))))
          (delete-frag-right gob frag frag-position))
      (T
          (setq out (list (copy-frag-left last-frag
					  (1- (frag-length last-frag)))))
          (do ((cur-frag (frag-prev last-frag) (frag-prev cur-frag)))
              ((eq cur-frag frag))
            (push (delete-frag gob cur-frag) out))
          (push (delete-frag-right gob frag frag-position) out)))
    (unless (eq frag last-frag)
      (free-frag-line (frag-next frag) last-frag)
      (setf (frag-next frag) NIL)
      (s-value line :last-frag frag))
    (if (and (zerop (frag-length frag)) (null (frag-prev frag)))
      (progn
        (s-value line :width 0)
        (s-value line :length 0)
        (s-value line :ascent (frag-ascent frag))
        (s-value line :descent (frag-descent frag)))
      (calculate-size-of-line gob line))
    (values out break-p)))

(defun copy-line (line)
  (let* ((last-frag (g-value line :last-frag))
         (break-p (frag-break-p last-frag))
         line)
    (setq line
          (list (if break-p
                  (copy-the-frag last-frag)
                  (copy-frag-left last-frag (1- (frag-length last-frag))))))
    (do ((frag (frag-prev last-frag) (frag-prev frag)))
        ((null frag))
      (push (copy-the-frag frag) line))
    (values line break-p)))

(defun delete-line (gob line)
  (let* ((last-frag (g-value line :last-frag))
         (break-p (frag-break-p last-frag))
         line)
    (setq line
          (list (if break-p
                  (delete-frag gob last-frag)
                  (copy-frag-left last-frag (1- (frag-length last-frag))))))
    (do ((frag (frag-prev last-frag) (frag-prev frag)))
        ((null frag))
      (push (delete-frag gob frag) line))
    (values line break-p)))


;; Deletes remainder of line.  Returns text deleted as a string.
(defun KILL-REST-OF-LINE (gob)
  (TOGGLE-SELECTION gob nil)
  (let* ((my-line (g-value gob :cursor-line))
         (next-line (g-value my-line :next-line))
         (frag (g-value gob :cursor-frag))
         (frag-pos (g-value gob :cursor-frag-pos))
         (last-frag (g-value my-line :last-frag))
         text)
    (cond
      ((frag-break-p last-frag)
          (invalidate-line my-line)
          (setq text (list (delete-line-right gob my-line frag frag-pos)))
          (merge-lines gob my-line next-line)
          (when (g-value gob :word-wrap-p)
            (undo-wrap-line gob my-line)
            (when (g-value my-line :prev-line)
              (undo-wrap-line gob (g-value my-line :prev-line)))))
      ((= (g-value-fixnum gob :cursor-position) (1- (g-value-fixnum my-line :length)))
          (if next-line
            (let ((char (DELETE-CHAR gob)))
              (setq text (if (characterp char) (string char) (list char))))
              (setq text NIL)))
      (T
          (invalidate-line my-line)
          (setq text (list (delete-line-mid gob my-line frag frag-pos last-frag
                                            (1- (frag-length last-frag)))))))
    text))


(defun copy-text (start-line start-frag start-pos end-line end-frag end-pos)
  (if (eq start-line end-line)
    (list (copy-line-mid start-frag start-pos end-frag end-pos))
    (let ((text (list (copy-line-left end-frag end-pos))))
      (do ((line (g-value end-line :prev-line) (g-value line :prev-line)))
          ((eq line start-line))
	(multiple-value-bind (line break-p) (copy-line line)
          (if break-p
            (setf (car text) (append line (car text)))
            (push line text))))
      (multiple-value-bind (line break-p)
          (copy-line-right start-line start-frag start-pos)
        (if break-p
          (setf (car text) (append line (car text)))
          (push line text)))
      text)))

(defun delete-text (gob start-line start-frag start-pos
                        end-line end-frag end-pos)
  (let (text prev-line)
    (if (eq start-line end-line)
      (setq text (list (delete-line-mid gob start-line start-frag start-pos
                                        end-frag end-pos)))
      (progn
        (setq text (list (delete-line-left gob end-line end-frag end-pos)))
        (do ((line (g-value end-line :prev-line) prev-line))
            ((eq line start-line))
          (setq prev-line (g-value line :prev-line))
          (multiple-value-bind (line break-p) (delete-line gob line)
            (if break-p
              (setf (car text) (append line (car text)))
              (push line text)))
          (destroy-line line))
        (multiple-value-bind (line break-p)
            (delete-line-right gob start-line start-frag start-pos)
          (if break-p
            (setf (car text) (append line (car text)))
            (push line text)))
        (merge-lines gob start-line end-line)))
    (invalidate-line start-line)
    (when (g-value gob :word-wrap-p)
      (undo-wrap-line gob start-line)
      (let ((prev-line (g-value start-line :prev-line)))
	(when prev-line
	  (undo-wrap-line gob prev-line))))
    (reset-font gob)
    text))

;; Returns copied portion in text format.
(defun COPY-SELECTED-TEXT (gob)
  (unless (g-value gob :selection-p)
    (return-from copy-selected-text nil))
  (let ((cursor-line (g-value gob :cursor-line))
        (cursor-pos (g-value gob :cursor-position))
        (cursor-frag (g-value gob :cursor-frag))
        (cursor-frag-pos (g-value gob :cursor-frag-pos))
        (select-line (g-value gob :select-line))
        (select-pos (g-value gob :select-position))
        (select-frag (g-value gob :select-frag))
        (select-frag-pos (g-value gob :select-frag-pos)))
    (if (higher-cursor cursor-line cursor-pos select-line select-pos)
      (copy-text cursor-line cursor-frag cursor-frag-pos
                 select-line select-frag select-frag-pos)
      (copy-text select-line select-frag select-frag-pos
                 cursor-line cursor-frag cursor-frag-pos))))

;; Returns deleted portion in text format.  Turns off selection highlight.
(defun DELETE-SELECTION (gob &optional lisp-mode-p)
  (if lisp-mode-p
    (inter::delete-lisp-region gob)
    (progn
      (unless (g-value gob :selection-p)
	(return-from delete-selection nil))
      (TOGGLE-SELECTION gob nil)
      (let ((cursor-line (g-value gob :cursor-line))
	    (cursor-pos (g-value gob :cursor-position))
	    (cursor-frag (g-value gob :cursor-frag))
	    (cursor-frag-pos (g-value gob :cursor-frag-pos))
	    (select-line (g-value gob :select-line))
	    (select-pos (g-value gob :select-position))
	    (select-frag (g-value gob :select-frag))
	    (select-frag-pos (g-value gob :select-frag-pos)))
	(if (higher-cursor cursor-line cursor-pos select-line select-pos)
	    (delete-text gob cursor-line cursor-frag cursor-frag-pos
			 select-line select-frag select-frag-pos)
	    (progn
	      (s-value gob :cursor-line select-line)
	      (s-value gob :cursor-position select-pos)
	      (multiple-value-bind (frag frag-pos x-offset)
		  (calculate-cursor-pos select-line select-pos)
		(s-value gob :cursor-frag frag)
		(s-value gob :cursor-frag-pos frag-pos)
		(s-value gob :cursor-x-offset x-offset))
	      (delete-text gob select-line select-frag select-frag-pos
			   cursor-line cursor-frag cursor-frag-pos)))))))

(defun SET-TEXT (gob text)
  (check-text text)
  (let (next-line)
    (do ((my-line (g-value gob :first-line) next-line))
	((null my-line))
      (setq next-line (g-value my-line :next-line))
      (destroy-line my-line)))
  (do ((object (g-value gob :first-object) (g-value object :mf-next-object)))
      ((null object))
    (opal:remove-component gob object))
  (free-mark-line (g-value gob :first-mark) (g-value gob :last-mark))
  (s-value gob :first-mark nil)
  (s-value gob :last-mark nil)
  (install-text gob text))


(defun GET-STRING (gob)
  (TEXT-TO-STRING (GET-TEXT gob)))


(defun GET-TEXT (gob)
  (let ((text nil))
    (do ((my-line (g-value gob :last-line) (g-value my-line :prev-line)))
        ((null my-line))
      (let ((last-frag (g-value my-line :last-frag)))
        (if (frag-break-p last-frag)
          (push (if (frag-object-p last-frag)
		  (let ((obj (frag-object last-frag)))
		    (if (mark-p obj)
			(list :mark (mark-sticky-left obj)
			      (mark-name obj) (mark-info obj))
			obj))
		  (list (frag-string last-frag) (frag-font last-frag)
			(frag-fcolor last-frag) (frag-bcolor last-frag)))
		(car text))
          (push (list (list (subseq (frag-string last-frag)
				    0 (1- (frag-length last-frag)))
			    (frag-font last-frag) (frag-fcolor last-frag)
			    (frag-bcolor last-frag))) text))
	(do ((frag (frag-prev last-frag) (frag-prev frag)))
	    ((null frag))
	  (push (if (frag-object-p frag)
		  (let ((obj (frag-object frag)))
		    (if (mark-p obj)
			(list :mark (mark-sticky-left obj)
			      (mark-name obj) (mark-info obj))
			obj))
		  (list (frag-string frag) (frag-font frag)
			(frag-fcolor frag) (frag-bcolor frag)))
		(car text)))))
    text))


(defun GET-OBJECTS (gob)
  (let ((obj-list NIL))
    (do ((object (g-value gob :first-object) (g-value object :mf-next-object)))
        ((null object))
      (push object obj-list))
    obj-list))


(defun NOTICE-RESIZE-OBJECT (object)
  (let ((my-line (g-value object :multifont-line))
        (frag (g-value object :multifont-frag)))
    (setf (frag-width frag) (g-value object :width))
    (setf (frag-ascent frag) (g-value object :height))
    (calculate-size-of-line (g-value object :multifont-object) my-line)
    (invalidate-line my-line)))



;;; The following are conversion functions, useful for turning text lists into
;;; other things.  Note there is no STRING-TO-TEXT function since by
;;; definition a string is a valid form of text.

(defun font-to-list (my-font)
  (cond
    ((is-a-p my-font opal:font)
        (list :FONT (g-value my-font :family) (g-value my-font :face)
              (g-value my-font :size)))
    ((is-a-p my-font opal:font-from-file)
        (list :FONT-FROM-FILE (g-value my-font :font-path)
              (g-value my-font :font-name)))
    (t  (font-to-list opal:default-font))))

(defun list-to-font (my-font)
  (case (car my-font)
    (:FONT
        (GET-STANDARD-FONT (second my-font) (third my-font) (fourth my-font)))
    (:FONT-FROM-FILE
        (create-instance nil opal:font-from-file
          (:font-path (second my-font))
          (:font-name (third my-font))))))

(defun frag-to-list (frag)
  (if (or (is-a-p frag opal:view-object) (stringp frag))
    frag
    (let* ((specs-list (cdr frag))
	   (font (if (listp specs-list)
		     (first specs-list)
		     specs-list)))
      (cons (car frag) (font-to-list font)))))

(defun list-to-frag (frag)
  (if (or (stringp frag) (is-a-p frag opal:view-object))
    frag
    (let* ((specs-list (cdr frag))
	   (font (if (listp specs-list)
		     (first specs-list)
		     specs-list)))
      (cons (car frag) (list-to-font font)))))

(defun line-to-list (my-line)
  (if (and my-line (listp my-line))
    (cons (frag-to-list (car my-line)) (line-to-list (cdr my-line)))
    my-line))

(defun list-to-line (my-line)
  (if (and my-line (listp my-line))
    (cons (list-to-frag (car my-line)) (list-to-line (cdr my-line)))
    my-line))

(defun TEXT-TO-PURE-LIST (text)
  (check-text text)
  (if (or (stringp text) (null text))
    text
    (cons (line-to-list (car text)) (text-to-pure-list (cdr text)))))

(defun PURE-LIST-TO-TEXT (pure-list)
  (if (or (stringp pure-list) (null pure-list))
    pure-list
    (cons (list-to-line (car pure-list)) (pure-list-to-text (cdr pure-list)))))

(defun TEXT-TO-STRING (text)
  (check-text text)
  (if text
    (if (stringp text)
      text
      (let ((str ""))
	(declare (simple-string str))
        (dolist (my-line text)
          (if (stringp my-line)
            (setq str (concatenate 'string str my-line (string #\newline)))
            (progn
              (dolist (frag my-line)
                (setq str (concatenate 'string str
		    (cond
		      ((stringp frag) frag)
		      ((schema-p frag) (format NIL "~s" frag))
		      ((eq (car frag) :mark) nil)
		      (T (car frag))))))
              (setq str (concatenate 'string str (string #\newline))))))
        (setq str (subseq str 0 (1- (length str))))))
    ""))

(defun listify-frag (frag)
  (if (stringp frag)
    (list frag opal:default-font *default-color* *default-color*)
    frag))

(defun listify-line (my-line)
  (if (listp my-line)
    (if my-line
      (cons (listify-frag (car my-line)) (listify-line (cdr my-line)))
      NIL)
    (list (listify-frag my-line))))

(defun listify-text (text)
  (if (and text (listp text))
    (cons (listify-line (car text)) (listify-text (cdr text)))
    (when text
      (let ((pos (position #\newline text)))
        (if pos
          (cons (listify-line (subseq text 0 pos))
                (listify-text (subseq text (1+ pos))))
          (list (listify-line text)))))))

(defun concat-lists (text1 text2)
  (setf (cdr (last (car (last text1)))) (car text2))
  (setf (cdr (last text1)) (cdr text2))
  text1)

(defun CONCATENATE-TEXT (text1 text2)
  (check-text text1)
  (check-text text2)
  (setq text1 (listify-text text1))
  (setq text2 (listify-text text2))
  (cond
    ((null text1) text2)
    ((null text2) text1)
    (T (concat-lists text1 text2))))


(defun empty-frag-p (frag)
  (cond
    ((stringp frag) (string= "" frag))
    ((is-a-p frag opal:view-object) NIL)
    (T (string= "" (car frag)))))

(defun empty-line-p (line)
  (if (stringp line)
    (string= "" line)
    (if line
      (and (not (is-a-p line opal:view-object))
           (= (length line) 1) (empty-frag-p (first line)))
      T)))

;; Returns T if the text given contains no text (ie. its blank).  Returns NIL,
;; otherwise.
(defun EMPTY-TEXT-P (text)
  (check-text text)
  (if (stringp text)
    (string= "" text)
    (if text
      (and (= (length text) 1) (empty-line-p (first text)))
      T)))



;;;
;;; These functions are needed to reinitialize the font information
;;; when you change the display
;;;

(defun reset-all-sizes (gob)
  (do ((line (g-value gob :first-line) (g-value line :next-line)))
      ((null line))
    (do ((frag (g-value line :first-frag) (frag-next frag)))
        ((null frag))
      (if (frag-object-p frag)
        (let ((object (frag-object frag)))
	  (when (not (mark-p object))
	    (setf (frag-width frag) (g-value object :width))
	    (setf (frag-ascent frag) (g-value object :height))))
        (let ((my-font (frag-font frag)))
          (setf (frag-width frag)
                (opal:string-width my-font (frag-string frag)
				   :display (g-local-value gob :window)))
          (setf (frag-ascent frag) (g-value my-font :max-char-ascent))
          (setf (frag-descent frag) (g-value my-font :max-char-descent)))))
    (calculate-size-of-line gob line)))

(defun reset-multifont-sizes ()
  (opal::do-all-instances opal:multifont-text
    #'(lambda (obj)
	(reset-all-sizes obj))))

(push #'reset-multifont-sizes opal::*auxilliary-reconnect-routines*)
