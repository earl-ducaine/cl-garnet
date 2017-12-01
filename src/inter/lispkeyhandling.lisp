;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Multifont LISP text-editor
;;;  Matthew Goldberg
;;;  Feb 18, 1993
;;;

;;; $Id$
;;;
;;;
;;; ============================================================
;;; Change log:
;;;     9/20/93 Fernando D. Mato Mira - Variable name: position ---> text-position
;;;     8/05/93 Andrew Mickish - Added indent of kr:define-method
;;;     7/13/93 Matt Goldberg - Rewrote many functions, using opal:marks.
;;;     6/01/93 Matt Goldberg - Set interactor's :match-obj with the
;;;                set-parenthesis message and added function LISPIFY
;;;     5/26/93 Matt Goldberg - started
;;; ============================================================

(in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(indent turn-off-match lispify add-lisp-char delete-lisp-region)))


;;;; TABS

;;;; function WORD-TAB
;;;  input: TEXT-OBJ a text-object
;;;         FUNC-LINE, FUNC-CHAR the position of the beginning of the function
;;;          that the current word is an argument of.
;;;         ARG-NUM which argument is it (first, second, third)?
;;;  returns: the number of characters to the word to tab to

(defun word-tab (text-obj func-line func-char arg-num)
  (let* ((start (multiple-value-list
		    (opal:get-cursor-line-char-position text-obj)))
	 (start-line (first start)))
    (cond
      ((= 0 start-line) nil)
      ((= 0 arg-num) (1+ func-char))
      (t (opal:set-cursor-to-line-char-position text-obj
						func-line (1+ func-char))
         (do ((arg 0 (1+ arg))
              (location '(-1 -1)
                        (let ((new-loc (multiple-value-list
					   (opal:get-cursor-line-char-position
					    text-obj))))
                          (if (equal (first new-loc) (first location))
                              location
                              new-loc))))
	     ((= arg arg-num) (second location))
	   (skip text-obj)
           (opal:go-to-next-word text-obj)
	   (let ((char (opal:fetch-next-char text-obj)))
	     (if (equal char #\#)
		 (progn
		   (opal:go-to-next-char text-obj)
		   (let ((next-char (opal:fetch-next-char text-obj)))
		     (opal:go-to-prev-char text-obj)
		     (if (equal next-char #\|)
			 (progn
			   (skip text-obj)
			   (setf char (opal:fetch-next-char text-obj))
			   (if (and char (find char '(#\space #\newline)))
			       (opal:go-to-next-word text-obj))
			   (setf char (opal:fetch-next-char text-obj)))))))
	     (if (equal char #\;)
		 (progn
		   (skip text-obj)
		   (opal:go-to-next-word text-obj))))
	   )))))

;;;; function GET-WORD
;;;  input: A line, and a number (the character-position on the line)
;;;         TRIM: if specified T, will not include the first character of the
;;;               word... useful for "(defun" -> "defun"
;;;  returns: the word that the cursor is currently on or before on same line
;;;    ->         (in :strings format)

(defun get-word (line char-pos &key (trim nil))
  (multiple-value-bind (frag frag-pos)
      (opal::calculate-cursor-pos line char-pos)
    (let ((word ""))
      (when (not trim)
	(do* ((first-frag frag (opal::frag-prev first-frag))
	      (string (and frag (if (opal::frag-object-p frag)
				    "" (opal::frag-string frag)))
		      (and first-frag (if (opal::frag-object-p first-frag)
					  "" (opal::frag-string first-frag))))
	      (first-frag-pos
	       (position #\space (subseq string 0 frag-pos) :from-end t)
	       (position #\space string :from-end t)))
	     ((or first-frag-pos (null first-frag))
	      (when first-frag
		(setq word (concatenate
			    'string word
			    (reverse (if (eq first-frag frag)
					 (subseq string first-frag-pos frag-pos)
					 (subseq string first-frag-pos)))))))
	  (setq word (concatenate 'string word
				  (reverse (if (eq first-frag frag)
					       (subseq string 0 frag-pos)
					       string)))))
	(setq word (reverse word)))
      (do* ((last-frag frag (opal::frag-next last-frag))
	    (string (and frag (if (opal::frag-object-p frag)
				  "" (opal::frag-string frag)))
		    (and last-frag (if (opal::frag-object-p last-frag)
				       "" (opal::frag-string last-frag))))
	    (last-frag-pos
	     (position #\space string :start frag-pos)
	     (position #\space string)))
	   ((or last-frag-pos (null last-frag))
	    (when last-frag
	      (setq word (concatenate
			  'string word (if (eq last-frag frag)
					   (subseq string frag-pos last-frag-pos)
					   (subseq string 0 last-frag-pos))))))
	(setq word (concatenate 'string word (if (eq last-frag frag)
						 (subseq string frag-pos)
						 string))))
      (remove #\space word))))

;;; returns 't' if the current word is a character-code, 'nil' if not.
(defun char-code-p (frag frag-pos)
  (if (and (null (opal::frag-prev frag)) (< frag-pos 2))
      nil
      (progn
	(when (<= frag-pos 0)
	  (setq frag (do* ((my-frag (opal::frag-prev frag)
				    (opal::frag-prev my-frag))
			   (length (opal::frag-length my-frag)
				   (opal::frag-length my-frag)))
			  ((or (null my-frag) (> length 0))
			   (if my-frag
			       (setq frag-pos (+ length frag-pos))
			       (return-from char-code-p nil))
			   my-frag))))
	(let ((prev-char
	       (do* ((my-frag frag (opal::frag-prev frag))
		     (pos frag-pos (opal::frag-length frag)))
		    ((> pos 0) (schar (opal::frag-string frag) (1- pos)))))
	      char)
	  (if (member prev-char '(#\\ #\-))
	      (progn
		(cond
		  ((= frag-pos 1)
		   (do* ((my-frag (opal::frag-prev frag)
				  (opal::frag-prev my-frag))
			 (length (opal::frag-length my-frag)
				 (and my-frag (opal::frag-length my-frag))))
			((> length 0)
			 (setq char (schar (opal::frag-string my-frag)
					   (1- length))))))
		  ((= frag-pos 0)
		   (let ((two-frags-flag nil))
		     (do* ((my-frag (opal::frag-prev frag)
				    (opal::frag-prev my-frag))
			   (length (opal::frag-length my-frag)
				   (and my-frag (opal::frag-length my-frag))))
			  ((> length 1)
			   (setq char (schar (opal::frag-string my-frag)
					    (- length (if two-frags-flag 1 2)))))
		       (when (= length 1) (setq two-frags-flag t)))))
		  (t (setq char
			   (schar (opal::frag-string frag) (- frag-pos 2)))))
		(not (find char '(#\\ #\-))))
	      nil)))))

(defvar *legit-fonts* (list (opal:get-standard-font :fixed :roman :medium)
			    (opal:get-standard-font :fixed :bold :medium)))

(defvar *lisp-delim-chars* (list #\space #\newline #\tab #\( #\)))

(defvar *lisp-names* (list :open :close :arg
			   :open-comment :close-comment :quote))

(defmacro legit-font-p (font)
  `(member ,font *legit-fonts*))

(defmacro lisp-delim-char-p (char)
  `(or (null ,char) (member ,char *lisp-delim-chars*)))

(defmacro lisp-mark-p (name)
  `(member ,name *lisp-names*))

;;;; function LAST-PAREN
;;;   input: text-obj, the text object
;;;          IN-QUOTE-P: If specified T, the count will begin as if starting
;;;                      within a quotation, otherwise not.
;;;   This function will return the frag of the last unmatched open
;;;   parentheses before the initial cursor position, or nil if
;;;   there are none.
;;;   Returns: 2 values, the frag, and
;;;            the number of arguments to the function, or -1 if there
;;;            is none.

(defun last-paren (line frag &key (in-quote-p nil))
  (let ((arguments 0)
	(closes 0))
    (do* ((mark (if in-quote-p
		  (progn
		    (setq in-quote-p nil)
		    (opal::mark-prev
		     (opal::search-backwards-for-mark-from line
							   frag :name :quote)))
		  (opal::search-backwards-for-mark-from line frag))
		(and mark (opal::mark-prev mark)))
	  (name (and mark (opal::mark-name mark))
		(and mark (opal::mark-name mark)))
	  (frag (and mark (opal::frag-prev (opal::mark-frag mark)))
		(and mark (opal::frag-prev (opal::mark-frag mark))))
	  (pos (and frag (1- (opal::frag-length frag)))
	       (and frag (1- (opal::frag-length frag)))))
	 ((or (null mark) (and (eq name :open) (zerop closes)
			       (not (char-code-p frag pos))))
	  (if mark
	    (values mark (if (zerop arguments) 0 (1- arguments)))
	    (values nil -1)))
      (cond
	((char-code-p frag pos) nil)
	((eq name :arg) (when (zerop closes) (incf arguments)))
	((eq name :close)
	 (when (zerop closes) (incf arguments))
	 (incf closes))
	((eq name :open)
	 (decf closes))
	((eq name :quote)
	 (incf arguments)
	 (let ((prev-quote (opal::search-backwards-for-mark-from
			    (opal::mark-line mark)
			    (opal::frag-prev (opal::mark-frag mark))
			    :name :quote)))
	   (if prev-quote
	       (setq mark prev-quote)
	       (setq closes 0
		     arguments 0))))
	((eq name :open-comment)
	 (setq closes 0
	       arguments 0))
	((eq name :close-comment)
	 (let ((open-comment (opal::search-backwards-for-mark-from
			      (opal::mark-line mark)
			      (opal::frag-prev (opal::mark-frag mark))
			      :name :open-comment)))
	   (if open-comment
	       (setq mark open-comment)
	       (return-from last-paren (values nil -1)))))))))

;;;; Function Count-Chars-Before-Frag
;;;  input: A Fragment
;;;  returns: the number of chars from the beginning of the line to the
;;;           end of that fragment

(defun count-chars-before-frag (frag)
  (let ((chars 0))
    (do ((my-frag frag (opal::frag-prev my-frag)))
	((null my-frag) chars)
      (incf chars (opal::frag-length my-frag)))))

;;;; Function Get-Line-Number
;;;  input: A Line
;;;  returns: the number of that line, that is, 0 for first, 1 for second, etc.

(defun get-line-number (line)
  (let ((num 0))
    (do ((my-line (g-value line :prev-line) (g-value my-line :prev-line)))
	((null my-line) num)
      (incf num))))

;;;; Function FUNCTION-FIND
;;;  input: TEXT-OBJ, a text-object
;;;         START-LINE, the line to begin searching backwards from
;;;  returns:  the closest unclosed function name, i.e. (dotimes ...
;;;    in a list, first element: function-name (:strings format)
;;;              second element: number of arguments of that function
;;;               third element: line-position (number)
;;;              fourth element: character-position (number)

(defun function-find (text-obj start-line)
  (cond
    ((= start-line 0) (list nil 0 0 0))
    (t
     (opal:set-selection-to-line-char-position text-obj start-line 0)
     (opal:toggle-selection text-obj t)
     (let* ((text (opal:text-to-string (opal:copy-selected-text text-obj)))
	    (is-in-a-quote-p (oddp (- (length text)
				      (length (remove #\" text))))))
       (opal:toggle-selection text-obj nil)
       (let ((prev-line (g-value text-obj :cursor-line :prev-line))
	     func-list mark arguments)
	 (multiple-value-setq (mark arguments)
	   (last-paren prev-line (g-value prev-line :last-frag)
		       :in-quote-p is-in-a-quote-p))
	 (if (>= arguments 0)
	     (progn
	       (let* ((line (opal::mark-line mark))
		      (line-pos (get-line-number line))
		      (char-pos (count-chars-before-frag
				 (opal::mark-frag mark))))
		 (setf func-list (list (get-word line char-pos :trim t)
				       arguments line-pos (1- char-pos)))))
	     (setf func-list (list nil 0 0 0)))
	 func-list)))
    )
  )


;;;
;;;   The indentation table: how far to indent for each lisp function
;;;

(defvar *tab-table* (make-hash-table :test #'equal :size 3))

;;;; function GET-TAB-AMOUNT
;;;   input: name (the name of the function) a string
;;;          arg   then number of the argument to tab
;;;   output: the number of spaces to indent, or -1 if it should do a
;;;            "WORD-TAB"

(defun get-tab-amount (name arg)
  (let ((amount-list (gethash name *tab-table*)))
    (cond
      ((string-equal name "") 1)
      ((equal (elt name 0) #\() 1)
      ((not amount-list) -1)
      ((= (1- arg) (first amount-list)) 2)
      ((> (1- arg) (first amount-list)) -1)
      (t (second amount-list)))))

;;;; function INDENT
;;;  input: NAME, the name of the function
;;;         SPECIAL-ARGS one or two numbers.
;;;  output: will put the function into the *tab-table* hash table, giving
;;;           the function with name NAME special tab-properties.
;;;           The first number of SPECIAL-ARGS signifies how many arguments
;;;            have the special property.
;;;           The second number signifies how many spaces from the
;;;            function name these special args should be placed.
;;;           The number -1 means that this arg should act as a WORD-TAB.
;;;           The argument after the last special argument will be placed
;;;            one space in from the function-name.
;;;           All other args will act as WORD-TABS.

(defun indent (name &rest special-args)
  (setf (gethash name *tab-table*) (if special-args special-args '(0)))
  )

(indent "defun" 2 4)
(indent "create-instance" 2 4)
(indent "let" 1 4)
(indent "let*" 1 4)
(indent "flet" 1 4)
(indent "labels" 1 4)
(indent "when" 1 4)
(indent "unless" 1 4)
(indent "cond" 0)
(indent "case" 1 4)
(indent "ecase" 1 4)
(indent "ccase" 1 4)
(indent "typecase" 1 4)
(indent "etypecase" 1 4)
(indent "ctypecase" 1 4)
(indent "dotimes" 1 4)
(indent "dolist" 1 4)
(indent "do" 2 -1)
(indent "do*" 2 -1)
(indent "defvar" 1 4)
(indent "defparameter" 1 4)
(indent "lambda" 1 4)
(indent "defstruct" 1 4)
(indent "with-open-file" 1 4)
(indent "with-open-stream" 1 4)
(indent "progn" 0)
(indent "multiple-value-bind" 1 6)
(indent "multiple-value-setq" 1 4)
(indent "defclass" 2 4)
(indent "defmethod" 2 4)
(indent "with-accessors" 1 4)
(indent "define-method" 3 4)


;;;; function TAB
;;;  input: TEXT-OBJ, the text-object
;;;  output: will put the correct (LISP-wise) number of spaces between
;;;           the first word on the line and the beginning of the line.

(defun tab (interact text-obj event)
  (declare (ignore interact event))
  (let* ((start (multiple-value-list
		    (opal:get-cursor-line-char-position text-obj)))
         (start-char (second start))
         (start-line (first start))
         (info (function-find text-obj start-line))
         (special-tab (if (first info)
			  (get-tab-amount  (opal:text-to-string (first info))
					   (1+ (second info)))
			  0))
         (tab-amount (if (= -1 special-tab)
                         (word-tab text-obj (third info)
				   (fourth info) (second info))
                         (+ (fourth info) special-tab))))

    (opal:set-cursor-to-line-char-position text-obj start-line 0)
    (when (opal::delim-char-p (opal:fetch-next-char text-obj))
      (opal:go-to-next-word text-obj))
    (when (not (= start-line
		  (first (multiple-value-list
			     (opal:get-cursor-line-char-position text-obj)))))
      (opal:set-cursor-to-line-char-position text-obj start-line 0)
      (opal:go-to-end-of-line text-obj))
    (opal:set-selection-to-line-char-position
     text-obj start-line (second
			  (multiple-value-list
			      (opal:get-cursor-line-char-position text-obj))))
    (opal:go-to-beginning-of-line text-obj)
    (let ((num-of-deleted-chars (length (opal:text-to-string
					 (opal:delete-selection text-obj)))))
      (s-value text-obj :current-font
	       (opal:get-standard-font :fixed :roman :medium))
      (opal:insert-string text-obj
			  (make-string tab-amount :initial-element #\space))
      (opal:set-cursor-to-line-char-position
       text-obj start-line (+ (if (and (> start-char num-of-deleted-chars)
				       (>= num-of-deleted-chars 0))
				  (- start-char num-of-deleted-chars)
				  0)
			      tab-amount)))))


;;;; MATCHING PARENTHESIS

;;;; function LINE-STRING
;;;  input: a line
;;;  output: a string containing all the text on that line

(defun line-string (line)
  (opal::copy-text line (g-value line :first-frag) 0 line
		   (g-value line :last-frag)
		   (second (multiple-value-list
			       (opal::calculate-cursor-pos
				line (g-value line :length))))))

;;;; function MATCH-PARENS
;;;  input: text-obj, the text-object, with the cursor next to the close-paren
;;;          that should be matched.
;;;  output: Will bold-face the corresponding open paren, and set the
;;;          message-window to read the function name, if such a window exists.

(defun match-parens (interact text-obj)
  (let* ((start (multiple-value-list
		    (opal:get-cursor-line-char-position text-obj)))
         (start-char (second start))
         (start-line (first start))
	 (selection (multiple-value-list
			(opal:get-selection-line-char-position text-obj)))
	 (selection-p (g-value text-obj :selection-p)))
    (opal:go-to-prev-char text-obj)
    (if (char-code-p (g-value text-obj :cursor-frag)
		     (g-value text-obj :cursor-frag-pos))
	(opal:go-to-next-char text-obj)
	(multiple-value-bind (mark arguments)
	    (last-paren (g-value text-obj :cursor-line)
			(g-value text-obj :cursor-frag))
	  (if (>= arguments 0)
	    (progn
	      (opal:set-cursor-to-line-char-position
	       text-obj
	       (get-line-number (opal::mark-line mark))
	       (1- (count-chars-before-frag (opal::mark-frag mark))))
	      (let* ((begin (multiple-value-list
				(opal:get-cursor-line-char-position text-obj)))
		     (begin-char (second begin))
		     (begin-line (first begin)))
		(s-value text-obj :curr-paren begin)
		(opal:set-selection-to-line-char-position text-obj begin-line
							  (1+ begin-char))
		(opal:toggle-selection text-obj t)
		(opal:change-font-of-selection text-obj nil :bold t)
		(opal:toggle-selection text-obj nil)
		(if (g-value interact :match-obj)
		  (opal:set-text (g-value interact :match-obj)
				(line-string (g-value text-obj :cursor-line))))))
	    (progn
	      (inter:beep)
	      (s-value text-obj :curr-paren 0)
	      (if (g-value interact :match-obj)
		  (opal:set-text (g-value interact :match-obj) "No match."))))
	  (opal:set-cursor-to-line-char-position text-obj start-line start-char)
	  (when selection-p
	    (opal:set-selection-to-line-char-position
	     text-obj (first selection) (second selection))
	    (opal:toggle-selection text-obj t))))))

(defun TURN-OFF-MATCH (interact text-obj)
  (let ((curr-paren (g-value text-obj :curr-paren)))
    (if curr-paren
      (if (eq curr-paren 0)
	(progn
	  (if (g-value interact :match-obj)
	      (opal:set-text (g-value interact :match-obj) ""))
	  (s-value text-obj :curr-paren nil))
	(let* ((start (multiple-value-list
			  (opal:get-cursor-line-char-position text-obj)))
	       (start-char (second start))
	       (start-line (first start))
	       (selection (multiple-value-list
			      (opal:get-selection-line-char-position text-obj)))
	       (selection-p (g-value text-obj :selection-p)))
	  (opal:set-cursor-to-line-char-position
	   text-obj (first curr-paren) (second curr-paren))
	  (opal:set-selection-to-line-char-position
	   text-obj (first curr-paren) (1+ (second curr-paren)))
	  (opal:toggle-selection text-obj t)
	  (opal:change-font-of-selection text-obj nil :bold nil)
	  (if selection-p
	      (opal:set-selection-to-line-char-position
	       text-obj (first selection) (second selection))
	      (opal:toggle-selection text-obj nil))
	  (opal:set-cursor-to-line-char-position text-obj start-line start-char)
	  (s-value text-obj :curr-paren nil)
	  (if (g-value interact :match-obj)
	      (opal:set-text (g-value interact :match-obj) "")))))))

;;;; function CHECK-PARENS
;;;  If there is currently a parenthesis highlighted, will turn it off.
;;;  Also, if the cursor is next to a close paren, will call MATCH-PARENS
(defun check-parens (interact text-obj)
  (TURN-OFF-MATCH interact text-obj)
  (let ((curr-font (g-value text-obj :current-font))
	(last-char (opal:fetch-prev-char text-obj)))
    (if last-char
	(if (and last-char (equal last-char #\))
		 (equal curr-font
			(opal:get-standard-font :fixed :roman :medium)))
	    (match-parens interact text-obj)
	    (if (and last-char (equal last-char #\newline)
		     (equal curr-font
			    (opal:get-standard-font :fixed :italic :medium)))
		(s-value text-obj :current-font
			 (opal:get-standard-font :fixed :roman :medium))))
	(s-value text-obj :current-font
		 (opal:get-standard-font :fixed :roman :medium)))))


;;;; COMMENTS

;;;; function ITALICIZE
;;;  Input: text-obj, the text-object
;;;         FROM-LOCATION, a list of the form (line-no. char-no.)
;;;  Will make italics all text from "from-location" to end of line.

(defun italicize (text-obj from-location)
  (let* ((start (multiple-value-list
		    (opal:get-cursor-line-char-position text-obj)))
         (start-char (second start))
         (start-line (first start))
	 (from-line (first from-location))
	 (from-char (second from-location)))
    (opal:set-cursor-to-line-char-position text-obj from-line from-char)
    (opal:go-to-end-of-line text-obj)
    (do ((curr-char (opal:fetch-next-char text-obj)
		    (opal:fetch-next-char text-obj)))
        ((or (null curr-char) (equal curr-char #\newline)))
      (opal:go-to-next-line text-obj)
      (opal:go-to-end-of-line text-obj))
    (opal:set-selection-to-line-char-position text-obj from-line from-char)
    (opal:toggle-selection text-obj t)
    (opal:change-font-of-selection text-obj nil :italic t)
    (opal:toggle-selection text-obj nil)
    (opal:set-cursor-to-line-char-position text-obj from-line from-char)
    (let ((removed-mark nil))
      (do* ((frag (g-value text-obj :cursor-frag) (opal::frag-next frag))
	    (obj (and frag (opal::frag-object frag))
		 (and frag (opal::frag-object frag))))
	   ((null frag)
	    (when removed-mark
	      (opal::calculate-size-of-line text-obj
					    (g-value text-obj :cursor-line))))
	(when (and (opal::mark-p obj) (lisp-mark-p (opal::mark-name obj)))
	  (opal::remove-mark text-obj obj)
	  (setq removed-mark t))))
    (opal:set-cursor-to-line-char-position text-obj start-line start-char)))

;;;; function DEFAULT-UP-TO-NEXT-SEMI
;;;  Will make default-font all text from current location until the first
;;;   semi-colon or end of line.

(defun default-up-to-next-semi (text-obj)
  (let* ((start (multiple-value-list
		    (opal:get-cursor-line-char-position text-obj)))
         (start-char (second start))
         (start-line (first start)))
    (do ((prev-char (opal:fetch-prev-char text-obj) curr-char)
	 (curr-char (opal:fetch-next-char text-obj)
		    (opal:fetch-next-char text-obj)))
        ((or (null curr-char) (equal curr-char #\;)
	     (equal curr-char #\newline))
	 (if (eq prev-char #\()
	     (opal:insert-mark text-obj t :name :open)
	     (when (eq prev-char #\))
	       (opal:insert-mark text-obj t :name :close)))
         (opal:set-selection-to-line-char-position
	  text-obj start-line start-char)
         (opal:toggle-selection text-obj t)
         (opal:change-font-of-selection
	  text-obj (opal:get-standard-font :fixed :roman :medium))
         (opal:set-cursor-to-line-char-position text-obj
						start-line start-char))
      (cond
	((and (eq prev-char #\#) (eq curr-char #\|))
	 (opal:insert-mark text-obj nil :name :open-comment))
	((and (eq prev-char #\|) (eq curr-char #\#))
	 (opal:insert-mark text-obj t :name :close-comment))
	((lisp-delim-char-p prev-char)
	 (when (not (lisp-delim-char-p curr-char))
	   (opal:insert-mark text-obj nil :name :arg))
	 (if (eq prev-char #\()
	     (opal:insert-mark text-obj t :name :open)
	     (when (eq prev-char #\))
	       (opal:insert-mark text-obj t :name :close)))))
      (opal:go-to-next-char text-obj))))


;;;; SKIPS through LISP EXPRESSIONS (Control-Meta-F etc.)

;;;; function FIND-CLOSE-PAREN
;;;  Will search forward for the first unmatched close paren.

(defun find-close-paren (text-obj line frag)
  (let ((opens 0)
	(in-quote-p nil))
    (do* ((mark (if in-quote-p
		  (progn
		    (setq in-quote-p nil)
		    (opal::mark-prev
		     (opal::search-for-mark-from line frag :name :quote)))
		  (opal::search-for-mark-from line frag))
		(and mark (opal::mark-next mark)))
	  (name (and mark (opal::mark-name mark))
		(and mark (opal::mark-name mark)))
	  (frag (and mark (opal::frag-prev (opal::mark-frag mark)))
		(and mark (opal::frag-prev (opal::mark-frag mark))))
	  (pos (and frag (1- (opal::frag-length frag)))
	       (and frag (1- (opal::frag-length frag)))))
	 ((or (null mark) (and (eq name :close) (zerop opens)
			       (not (char-code-p frag pos))))
	  (if mark
	      (opal:set-cursor-to-line-char-position
	       text-obj
	       (get-line-number (opal::mark-line mark))
	       (count-chars-before-frag frag))
	      nil))
      (cond
	((char-code-p frag pos) nil)
	((eq name :open)
	 (incf opens))
	((eq name :close)
	 (decf opens))
	((eq name :quote)
	 (let ((next-quote (opal::search-for-mark-from
			    (opal::mark-line mark)
			    (opal::frag-next (opal::mark-frag mark))
			    :name :quote)))
	   (if next-quote
	       (setq mark next-quote)
	       (setq opens 0))))
	((eq name :close-comment)
	 (setq opens 0))
	((eq name :open-comment)
	 (let ((close-comment (opal::search-for-mark-from
			       (opal::mark-line mark)
			       (opal::frag-next (opal::mark-frag mark))
			       :name :close-comment)))
	   (if close-comment
	       (setq mark close-comment)
	       (return-from find-close-paren nil))))))))

;;;; function SKIP
;;;  Will move cursor, depending on current location.
;;;   If currently before an open paren, will move to corresponding close-paren.
;;;   If currently before a comment/quote, will move to end.
;;;   Else move to end of word.

(defun skip (text-obj)
  (opal:toggle-selection text-obj nil)
  (when (opal::delim-char-p (opal:fetch-next-char text-obj))
    (opal:go-to-next-word text-obj))
  (let ((char (opal:fetch-next-char text-obj)))
    (cond
      ((not char) nil)
      ((eq char #\()
       (find-close-paren text-obj (g-value text-obj :cursor-line)
			 (opal::frag-next
			  (opal::frag-next (g-value text-obj :cursor-frag)))))
      ((eq char #\#)
       (opal:go-to-next-char text-obj)
       (if (equal (opal:fetch-next-char text-obj) #\|)
	   (let ((mark (opal::search-for-mark-from
			(g-value text-obj :cursor-line)
			(g-value text-obj :cursor-frag)
			:name :close-comment)))
	     (when mark
	       (opal:set-cursor-to-line-char-position
		text-obj
		(get-line-number (opal::mark-line mark))
		(count-chars-before-frag (opal::mark-frag mark)))
	       (do ((char (opal:go-to-next-char text-obj)
			  (opal:go-to-next-char text-obj)))
		   ((or (null char) (lisp-delim-char-p char) (eq char #\"))
		    (opal:go-to-prev-char text-obj)))))))
      ((eq char #\")
       (opal:go-to-next-char text-obj)
       (let ((mark (opal::search-for-mark-from
		    (g-value text-obj :cursor-line)
		    (g-value text-obj :cursor-frag)
		    :name :quote)))
	 (when mark
	   (opal:set-cursor-to-line-char-position
	    text-obj
	    (get-line-number (opal::mark-line mark))
	    (count-chars-before-frag (opal::mark-frag mark))))))
      (T
       (do ((char (opal:go-to-next-char text-obj)
		  (opal:go-to-next-char text-obj)))
	   ((or (null char) (lisp-delim-char-p char) (eq char #\"))
	    (opal:go-to-prev-char text-obj)))
       (do ()
           ((not (equal (g-value text-obj :current-font)
			(opal:get-standard-font :fixed :italic :medium))))
         (opal:go-to-next-word text-obj))))))

;;;; function SKIP-BACK
;;;  Will move cursor, depending on current location.
;;;   If currently after a close paren, will move to corresponding open-paren.
;;;   If currently after a comment/quote, will move to beginning
;;;   Else move to beginning of word.

(defun skip-back (text-obj)
  (opal:toggle-selection text-obj nil)
  (when (opal::delim-char-p (opal:fetch-prev-char text-obj))
    (opal:go-to-prev-word text-obj))
  (let ((char (opal:fetch-prev-char text-obj)))
    (cond
      ((not char) nil)
      ((eq char #\))
       (let ((mark (last-paren (g-value text-obj :cursor-line)
			       (opal::frag-prev
				(opal::frag-prev
				 (g-value text-obj :cursor-frag))))))
	 (when mark
	   (opal:set-cursor-to-line-char-position
	    text-obj
	    (get-line-number (opal::mark-line mark))
	    (1- (count-chars-before-frag (opal::mark-frag mark)))))))
      ((eq char #\#)
       (opal:go-to-prev-char text-obj)
       (if (equal (opal:fetch-prev-char text-obj) #\|)
	   (let ((mark (opal::search-backwards-for-mark-from
			(g-value text-obj :cursor-line)
			(g-value text-obj :cursor-frag)
			:name :open-comment)))
	     (when mark
	       (opal:set-cursor-to-line-char-position
		text-obj
		(get-line-number (opal::mark-line mark))
		(count-chars-before-frag (opal::mark-frag mark)))
	       (do ((char (opal:go-to-prev-char text-obj)
			  (opal:go-to-prev-char text-obj)))
		   ((or (null char) (lisp-delim-char-p char) (eq char #\"))
		    (opal:go-to-next-char text-obj)))))))
      ((eq char #\")
       (opal:go-to-prev-char text-obj)
       (let ((mark (opal::search-backwards-for-mark-from
		    (g-value text-obj :cursor-line)
		    (g-value text-obj :cursor-frag)
		    :name :quote)))
	 (when mark
	   (opal:set-cursor-to-line-char-position
	    text-obj
	    (get-line-number (opal::mark-line mark))
	    (count-chars-before-frag (opal::mark-frag mark))))))
      (T
       (do ((char (opal:go-to-prev-char text-obj)
		  (opal:go-to-prev-char text-obj)))
	   ((or (null char) (lisp-delim-char-p char) (eq char #\"))
	    (opal:go-to-next-char text-obj)))
       (do ()
           ((not (equal (g-value text-obj :current-font)
			(opal:get-standard-font :fixed :italic :medium))))
         (opal:go-to-prev-word text-obj))))))

;;;;  LISPIFY
;;;  Changes a string into LISP format
;;;  Input: a string
;;;  Output: text, in "strings" format

(defun LISPIFY (text)
  (cond
    ((stringp text) (setq text (opal::add-space-to-line text)))
    ((atom text)    (setq text (list " ")))
    (T              (setq text (opal::add-space-to-line
				(opal:text-to-string text)))))
  (let ((line-pos -1)
	(paren-stack nil)
	(lispified-text nil)
	in-comment-p in-quote-p)
    (dolist (my-line text (reverse lispified-text))
      (incf line-pos)
      (let* ((paren-info (if paren-stack (first paren-stack) nil))
	     (special-tab (if paren-stack
			      (get-tab-amount (second paren-info)
					      (third paren-info))
			      0))
	     (tab-amount (if paren-stack
			     (if (= -1 special-tab)
				 (if (<= (third paren-info) 1)
				     (1+ (second (first paren-info)))
				     (second (fourth paren-info)))
				 (+ (second (first paren-info))
				    special-tab))
			     0))
	     (frag-string "")
	     (string (concatenate 'string
				  (make-string tab-amount
					       :initial-element #\space)
				  (let ((last-space
					 (position #\space (first my-line)
						   :test
						   #'(lambda (a b)
						       (declare (ignore a))
						       (not
							(opal::delim-char-p
							 b))))))
				    (if last-space
					(subseq (first my-line) last-space)
					""))))
	     (string-length (length string))
	     new-line char prev-char prev-prev-char in-argument-p)
	(dotimes (char-pos string-length)
	  (setq prev-prev-char prev-char
		prev-char char
		char (schar string char-pos))
	  (when (and (not in-argument-p) (not (lisp-delim-char-p char))
		     (not (eq char #\;)))
	    (push frag-string new-line)
	    (push (list :mark nil :arg) new-line)
	    (when (and paren-stack (not in-comment-p) (not in-quote-p)
		       (not (and (eq char #\#)
				 (eq (schar string (1+ char-pos)) #\|))))
	      (let ((paren-info (first paren-stack)))
		(when (and (> (incf (third paren-info)) 1)
			   (not (= line-pos (first (fourth paren-info)))))
		  (setf (fourth paren-info)
			(list line-pos char-pos)))))
	    (setq frag-string ""
		  in-argument-p t))
	  (cond
	    ((eq char #\space)
	     (setq frag-string (concatenate 'string frag-string
					    (string #\space))
		   in-argument-p nil))
	    ((and (or (eq prev-char #\-) (eq prev-char #\\ ))
		  (not (or (eq prev-prev-char #\-) (eq prev-prev-char #\\ ))))
	     (setq frag-string (concatenate 'string frag-string
					    (string char))))
	    ((eq char #\()
	     (when (and (not in-comment-p) (not in-quote-p))
	       (when paren-stack
		 (let ((paren-info (first paren-stack)))
		   (when (and (> (incf (third paren-info)) 1)
			      (not (= line-pos (first (fourth paren-info)))))
		     (setf (fourth paren-info)
			   (list line-pos char-pos)))))
	       (push (list (list line-pos char-pos)
			   (subseq string (1+ char-pos)
				   (position #\space string :start char-pos))
			   0 '(-1 0))
		     paren-stack))
	     (push (concatenate 'string frag-string (string char)) new-line)
	     (setq frag-string ""
		   in-argument-p nil)
	     (push (list :mark t :open) new-line))
	    ((eq char #\))
	     (when (and paren-stack (not in-comment-p) (not in-quote-p))
	       (pop paren-stack))
	     (push (concatenate 'string frag-string (string char)) new-line)
	     (setq frag-string ""
		   in-argument-p nil)
	     (push (list :mark t :close) new-line))
	    ((eq char #\")
	     (push (concatenate 'string frag-string (string char)) new-line)
	     (setq frag-string ""
		   in-quote-p (not in-quote-p))
	     (push (list :mark t :quote) new-line))
	    ((eq char #\;)
	     (push frag-string new-line)
	     (push (list (subseq string char-pos string-length)
			 (opal:get-standard-font :fixed :italic :medium))
		   new-line)
	     (setq char-pos string-length     ; XXX
		   frag-string ""))
	    ((and (eq char #\|) (eq prev-char #\#))
	     (push (concatenate 'string frag-string (string char)) new-line)
	     (setq frag-string ""
		   in-comment-p t)
	     (push (list :mark t :open-comment) new-line))
	    ((and (eq char #\#) (eq prev-char #\|))
	     (push frag-string new-line)
	     (push (list :mark t :close-comment) new-line)
	     (setq frag-string "#"
		   in-comment-p nil))
	    (T
	     (setq frag-string
		   (concatenate 'string frag-string (string char))))))
	(push (concatenate
	       'list (reverse (push frag-string new-line)) (rest my-line))
	      lispified-text)))))



;;;; EDITING FUNCTIONS

(defun ADD-LISP-CHAR (text-obj char &optional new-font new-fcolor new-bcolor)
  (let* ((prev-char (opal:fetch-prev-char text-obj))
	 (next-char (opal:fetch-next-char text-obj))
	 (added-lisp-arg
	  (and (not (lisp-delim-char-p char))
	       (legit-font-p (g-value text-obj :current-font))
	       (or (null new-font) (legit-font-p new-font))
	       (lisp-delim-char-p prev-char))))
    (cond
      ((and (eq prev-char #\|) (eq next-char #\#))
       (let ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag)))
	 (opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				      line frag :name :close-comment))
	 (opal::calculate-size-of-line text-obj line)))
      ((and (eq prev-char #\#) (eq next-char #\|))
       (let ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag)))
	 (opal::remove-mark text-obj (opal::search-for-mark-from
				      line frag :name :open-comment))
	 (opal::calculate-size-of-line text-obj line))))

    (opal:add-char text-obj char new-font new-fcolor new-bcolor)

    (when added-lisp-arg
      (when (and (not (lisp-delim-char-p next-char))
		 (not (eq next-char #\;)))
	(let* ((line (g-value text-obj :cursor-line))
	       (frag (g-value text-obj :cursor-frag))
	       (mark (opal::search-for-mark-from line frag :name :arg)))
	  (opal::remove-mark text-obj mark)
	  (opal::calculate-size-of-line text-obj line)))
      (opal:insert-mark text-obj t :name :arg))))


(defun DELETE-LISP-REGION (text-obj)
  (let* ((line1 (g-value text-obj :cursor-line))
	 (region (opal:delete-selection text-obj))
	 (string (opal:text-to-string region))
	 (string-length (length string)))
    (if (zerop string-length)
	nil
  (let* ((line2 (g-value text-obj :cursor-line))
	 (first-char (schar string 0))
	 (last-char (schar string (1- string-length)))
	 (next-char (opal:fetch-next-char text-obj))
	 (prev-char (opal:fetch-prev-char text-obj))
	 (legit-font (legit-font-p (g-value text-obj :current-font)))
	 (need-mark (and (lisp-delim-char-p prev-char)
			 (not (lisp-delim-char-p last-char))
			 (not (lisp-delim-char-p next-char))
			 (not (eq next-char #\;))
			 legit-font))
	 (lost-arg (and (not (lisp-delim-char-p next-char))
			(not (eq next-char #\;))
			(lisp-delim-char-p last-char)
			(not (lisp-delim-char-p prev-char))
			legit-font
			(not (eq last-char #\;)))))
    (setq region (lispify string))
    (when (and legit-font string (find #\; string) (eq line1 line2))
      (default-up-to-next-semi text-obj))
    (when (and legit-font (eq first-char #\#) (eq prev-char #\|))
      (let ((line (g-value text-obj :cursor-line)))
	(opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				     line (g-value text-obj :cursor-frag)
				     :name :close-comment))
	(opal::calculate-size-of-line text-obj line)))
    (when (and legit-font (eq last-char #\#) (eq next-char #\|))
      (let ((line (g-value text-obj :cursor-line)))
	(opal::remove-mark text-obj (opal::search-for-mark-from
				     line (g-value text-obj :cursor-frag)
				     :name :open-comment))
	(opal::calculate-size-of-line text-obj line)))
    (when (or
	   (and string (find #\newline string) (not legit-font))
	   (and last-char legit-font
		(not (find (if (> string-length 1)
			       (elt string (- string-length 2)) prev-char)
			   '(#\\ #\-)))
		(find last-char '(#\\ #\-)) (eq next-char #\;)))
      (italicize text-obj (multiple-value-list
			      (opal:get-cursor-line-char-position text-obj))))
    (when (and legit-font string)
      (if (eq prev-char #\|)
	  (when (eq next-char #\#)
	    (opal:insert-mark text-obj t :name :close-comment))
	  (when (and (eq prev-char #\#)
		     (eq next-char #\|))
	    (opal:insert-mark text-obj t :name :open-comment))))
    (when need-mark
      (opal:insert-mark text-obj nil :name :arg))
    (when lost-arg
      (let* ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag))
	     (mark (opal::search-for-mark-from line frag :name :arg)))
	(opal::remove-mark text-obj mark)
	(opal::calculate-size-of-line text-obj line)))))
    region))


;;;;  KEY-BINDING-FUNCTIONS

(defun semi-func (interact text-obj event)
  (declare (ignore interact event))
  (cond
   ((char-code-p (g-value text-obj :cursor-frag)
		 (g-value text-obj :cursor-frag-pos))
    (add-lisp-char text-obj #\;))
   ((not (equal (g-value text-obj :current-font)
		(opal:get-standard-font :fixed :roman :medium)))
    (add-lisp-char text-obj #\;))
   (t
    (add-lisp-char text-obj #\;
		   (opal:get-standard-font :fixed :italic :medium))
    (italicize text-obj (multiple-value-list
			    (opal:get-cursor-line-char-position text-obj))))))

(defun return-func (interact text-obj event)
  (let* ((next-char (opal:fetch-next-char text-obj))
	 (prev-char (opal:fetch-prev-char text-obj))
	 (need-mark (and (not (lisp-delim-char-p next-char))
			 (not (eq next-char #\;))
			 (not (lisp-delim-char-p prev-char))
			 (legit-font-p (g-value text-obj :current-font)))))
    (add-lisp-char text-obj #\newline)
    (if (not (equal (g-value text-obj :current-font)
		    (opal:get-standard-font :fixed :roman :medium)))
	(default-up-to-next-semi text-obj))
    (tab interact text-obj event)
    (s-value text-obj :current-font
	     (opal:get-standard-font :fixed :roman :medium))
    (when need-mark
      (opal:insert-mark text-obj nil :name :arg))))

(defun bslash-func (interact text-obj event)
  (declare (ignore interact))
  (add-lisp-char text-obj (event-char event))
  (when (legit-font-p (g-value text-obj :current-font))
    (let ((char (opal:fetch-next-char text-obj)))
      (when (eq char #\;)
	(opal:delete-char text-obj)
	(default-up-to-next-semi text-obj)
	(add-lisp-char text-obj #\;
		       (opal:get-standard-font :fixed :roman :medium))
	(opal:go-to-prev-char text-obj)
	(opal:toggle-selection text-obj nil))))
  )

(defun rubout-func  (interact text-obj event)
  (declare (ignore interact event))
  (when (g-value text-obj :selection-p)
    (when (not (zerop (length (DELETE-LISP-REGION text-obj))))
      (return-from rubout-func nil)))
  (let* ((next-char (opal:fetch-next-char text-obj))
    	 (char (opal:delete-prev-char text-obj))
	 (prev-char (opal:fetch-prev-char text-obj))
	 (legit-font (legit-font-p (g-value text-obj :current-font)))
	 (need-mark (and (lisp-delim-char-p prev-char)
			 (not (lisp-delim-char-p char))
			 (not (lisp-delim-char-p next-char))
			 (not (eq next-char #\;))
			 legit-font))
	 (lost-arg (and (not (lisp-delim-char-p next-char))
			(not (eq next-char #\;))
			(lisp-delim-char-p char)
			(not (lisp-delim-char-p prev-char))
			legit-font)))
    (cond
     ((and legit-font (eq char #\;))
      (default-up-to-next-semi text-obj))
     ((and legit-font (eq char #\#))
      (when (eq prev-char #\|)
	(let ((line (g-value text-obj :cursor-line)))
	  (opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				       line (g-value text-obj :cursor-frag)
				       :name :close-comment))
	  (opal::calculate-size-of-line text-obj line)))
      (when (eq next-char #\|)
	(let ((line (g-value text-obj :cursor-line)))
	  (opal::remove-mark text-obj (opal::search-for-mark-from
				       line (g-value text-obj :cursor-frag)
				       :name :open-comment))
	  (opal::calculate-size-of-line text-obj line))))
     ((or
      (and (eq char #\newline) (not legit-font))
      (and char legit-font
	   (not (find prev-char '(#\\ #\-)))
	   (find char '(#\\ #\-)) (eq next-char #\;)))

      (italicize text-obj (multiple-value-list
			      (opal:get-cursor-line-char-position text-obj)))))
    (when (and legit-font char)
      (if (eq prev-char #\|)
	  (when (eq next-char #\#)
	    (opal:insert-mark text-obj t :name :close-comment))
	  (when (and (eq prev-char #\#)
		     (eq next-char #\|))
	    (opal:insert-mark text-obj nil :name :open-comment))))
    (when need-mark
      (opal:insert-mark text-obj nil :name :arg))
    (when lost-arg
      (let* ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag))
	     (mark (opal::search-for-mark-from line frag :name :arg)))
	(opal::remove-mark text-obj mark)
	(opal::calculate-size-of-line text-obj line)))))

(defun cd-func (interact text-obj event)
  (declare (ignore interact event))
  (when (g-value text-obj :selection-p)
    (when (not (zerop (length (DELETE-LISP-REGION text-obj))))
      (return-from cd-func nil)))
  (let* ((prev-char (opal:fetch-prev-char text-obj))
	 (char (opal:delete-char text-obj))
	 (next-char (opal:fetch-next-char text-obj))
	 (legit-font (legit-font-p (g-value text-obj :current-font)))
	 (need-mark (and (lisp-delim-char-p prev-char)
			 (not (lisp-delim-char-p char))
			 (not (lisp-delim-char-p next-char))
			 (not (eq next-char #\;))
			 legit-font))
	 (lost-arg (and (not (lisp-delim-char-p next-char))
			(not (eq next-char #\;))
			(lisp-delim-char-p char)
			(not (lisp-delim-char-p prev-char))
			legit-font
			(not (eq char #\;)))))
    (cond
     ((and legit-font (eq char #\;))
      (default-up-to-next-semi text-obj))
     ((and legit-font (eq char #\#))
      (when (eq prev-char #\|)
	(let ((line (g-value text-obj :cursor-line)))
	  (opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				       line (g-value text-obj :cursor-frag)
				       :name :close-comment))
	  (opal::calculate-size-of-line text-obj line)))
      (when (eq next-char #\|)
	(let ((line (g-value text-obj :cursor-line)))
	  (opal::remove-mark text-obj (opal::search-for-mark-from
				       line (g-value text-obj :cursor-frag)
				       :name :open-comment))
	  (opal::calculate-size-of-line text-obj line))))
     ((or
       (and (eq char #\newline) (not legit-font))
       (and char legit-font
	    (not (find prev-char '(#\\ #\-)))
	    (find char '(#\\ #\-)) (eq next-char #\;)))
      (italicize text-obj (multiple-value-list
			      (opal:get-cursor-line-char-position text-obj)))))
    (when (and legit-font char)
      (if (eq prev-char #\|)
	  (when (eq next-char #\#)
	    (opal:insert-mark text-obj t :name :close-comment))
	  (when (and (eq prev-char #\#)
		     (eq next-char #\|))
	    (opal:insert-mark text-obj nil :name :open-comment))))
    (when need-mark
      (opal:insert-mark text-obj nil :name :arg))
    (when lost-arg
      (let* ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag))
	     (mark (opal::search-for-mark-from line frag :name :arg)))
	(opal::remove-mark text-obj mark)
	(opal::calculate-size-of-line text-obj line)))))

(defun md-func (interact text-obj event)
  (declare (ignore interact event))
  (when (g-value text-obj :selection-p)
    (when (not (zerop (length (DELETE-LISP-REGION text-obj))))
      (return-from md-func nil)))
  (let* ((prev-char (opal:fetch-prev-char text-obj))
	 (word (opal:delete-word text-obj))
	 (word-length (length word)))
    (if (zerop word-length)
	nil
  (let* ((first-char (schar word 0))
	 (last-char (schar word (1- word-length)))
	 (next-char (opal:fetch-next-char text-obj))
	 (legit-font (legit-font-p (g-value text-obj :current-font)))
	 (need-mark (and (lisp-delim-char-p prev-char)
			 (not (lisp-delim-char-p last-char))
			 (not (lisp-delim-char-p next-char))
			 (not (eq next-char #\;))
			 legit-font))
	 (lost-arg (and (not (lisp-delim-char-p next-char))
			(not (eq next-char #\;))
			(lisp-delim-char-p last-char)
			(not (lisp-delim-char-p prev-char))
			legit-font
			(not (eq last-char #\;)))))
    (when (and legit-font word (find #\; word))
      (default-up-to-next-semi text-obj))
    (when (and legit-font (eq first-char #\#) (eq prev-char #\|))
      (let ((line (g-value text-obj :cursor-line)))
	(opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				     line (g-value text-obj :cursor-frag)
				     :name :close-comment))
	(opal::calculate-size-of-line text-obj line)))
    (when (and legit-font (eq last-char #\#) (eq next-char #\|))
      (let ((line (g-value text-obj :cursor-line)))
	(opal::remove-mark text-obj (opal::search-for-mark-from
				     line (g-value text-obj :cursor-frag)
				     :name :open-comment))
	(opal::calculate-size-of-line text-obj line)))
    (when (or
	   (and word (find #\newline word) (not legit-font))
	   (and last-char legit-font
		(not (find (if (> word-length 1)
			       (elt word (- word-length 2)) prev-char)
			   '(#\\ #\-)))
		(find last-char '(#\\ #\-)) (eq next-char #\;)))
      (italicize text-obj (multiple-value-list
			      (opal:get-cursor-line-char-position text-obj))))
    (when (and legit-font word)
      (if (eq prev-char #\|)
	  (when (eq next-char #\#)
	    (opal:insert-mark text-obj t :name :close-comment))
	  (when (and (eq prev-char #\#)
		     (eq next-char #\|))
	    (opal:insert-mark text-obj t :name :open-comment))))
    (when need-mark
      (opal:insert-mark text-obj nil :name :arg))
    (when lost-arg
      (let* ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag))
	     (mark (opal::search-for-mark-from line frag :name :arg)))
	(opal::remove-mark text-obj mark)
	(opal::calculate-size-of-line text-obj line)))))))


(defun ck-func (interact text-obj event)
  (let ((char (opal:fetch-next-char text-obj))
	(cut-buffer (g-value interact :cut-buffer))
	(kill-mode (g-value interact :kill-mode))
	(a-window (event-window event))
	deleted-stuff)
    (s-value interact :kill-mode t)
    (cond
     ((and char (equal char #\newline))
      (cd-func interact text-obj event)
      (if kill-mode
	  (setq deleted-stuff (opal:concatenate-text cut-buffer
						     (string #\newline)))
	  (setq deleted-stuff (string #\newline))))
     (t
      (let ((prev-char (opal:fetch-prev-char text-obj))
	    (legit-font (legit-font-p (g-value text-obj :current-font))))
	(when (and legit-font (eq char #\#) (eq prev-char #\|))
	  (let ((line (g-value text-obj :cursor-line)))
	    (opal::remove-mark text-obj (opal::search-backwards-for-mark-from
					 line (g-value text-obj :cursor-frag)
					 :name :close-comment))
	    (opal::calculate-size-of-line text-obj line)))
	(when (and legit-font (eq char #\|) (eq prev-char #\#))
	  (let ((line (g-value text-obj :cursor-line)))
	    (opal::remove-mark text-obj (opal::search-for-mark-from
					 line (g-value text-obj :cursor-frag)
					 :name :open-comment))
	    (opal::calculate-size-of-line text-obj line)))
	(when (and (not (lisp-delim-char-p prev-char))
		   (not (lisp-delim-char-p char))
		   (not (eq char #\;)))
	  (opal:insert-mark text-obj nil :name :arg))
      (if (and char (not (legit-font-p (g-value text-obj :current-font)))
	       (not (equal char #\;))) (default-up-to-next-semi text-obj))
      (if kill-mode
	  (setq deleted-stuff (opal:concatenate-text
			       cut-buffer (opal:kill-rest-of-line text-obj)))
	  (setq deleted-stuff (opal:kill-rest-of-line text-obj))))))
    (s-value interact :cut-buffer deleted-stuff)
    (opal:set-x-cut-buffer a-window (opal:text-to-string deleted-stuff))))

(defun cy-func (interact text-obj event)
  (declare (ignore event))
  (let* ((prev-char (opal:fetch-prev-char text-obj))
	 (next-char (opal:fetch-next-char text-obj))
	 (legit-font (legit-font-p (g-value text-obj :current-font)))
	 (text-position (multiple-value-list
		       (opal:get-cursor-line-char-position text-obj))))
  (cond
   ((not legit-font)
    (inter::paste-selection interact)
    (italicize text-obj text-position))
   (t
    (cond
      ((and (eq prev-char #\|) (eq next-char #\#))
       (let ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag)))
	 (opal::remove-mark text-obj (opal::search-backwards-for-mark-from
				line frag :name :close-comment))
	 (opal::calculate-size-of-line text-obj line)))
      ((and (eq prev-char #\#) (eq next-char #\|))
       (let ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag)))
	 (opal::remove-mark text-obj (opal::search-for-mark-from line frag
						:name :open-comment))
	 (opal::calculate-size-of-line text-obj line))))
    (inter::paste-selection interact)
    (let ((new-position (multiple-value-list
			    (opal:get-cursor-line-char-position text-obj)))
	  (last-char (opal:fetch-prev-char text-obj))
	  (new-legit-font (legit-font-p (g-value text-obj :current-font)))
	  first-char)
      (opal:set-cursor-to-line-char-position text-obj (first text-position)
					     (second text-position))
      (setq first-char (opal:fetch-next-char text-obj))
      (when (and legit-font
		 (not (lisp-delim-char-p first-char))
		 (not (eq first-char #\;))
		 (not (lisp-delim-char-p prev-char))
		 (not (eq prev-char #\;)))
	(let ((line (g-value text-obj :cursor-line))
	      (frag (g-value text-obj :cursor-frag)))
	  (opal::remove-mark text-obj (opal::search-for-mark-from line frag
						      :name :arg))
	  (opal::calculate-size-of-line text-obj line)))
      (when (and legit-font (eq prev-char #\#) (eq first-char #\|))
	(opal:insert-mark text-obj nil :name :open-comment))
      (when (and legit-font (eq prev-char #\|) (eq first-char #\#))
	(opal:insert-mark text-obj t :name :close-comment))
      (opal:set-cursor-to-line-char-position text-obj (first new-position)
					     (second new-position))
     (when (and new-legit-font
		(lisp-delim-char-p prev-char)
		(not (lisp-delim-char-p last-char))
		(not (eq last-char #\;))
		(not (lisp-delim-char-p next-char))
		(not (eq next-char #\;)))
       (let ((line (g-value text-obj :cursor-line))
	     (frag (g-value text-obj :cursor-frag)))
	 (opal::remove-mark text-obj (opal::search-for-mark-from line frag
						     :name :arg))
	 (opal::calculate-size-of-line text-obj line)))
     (when (and new-legit-font (eq last-char #\#) (eq next-char #\|))
       (opal:insert-mark text-obj nil :name :open-comment))
     (when (and new-legit-font (eq last-char #\|) (eq next-char #\#))
       (opal:insert-mark text-obj t :name :close-comment))
     (when (not new-legit-font)
       (italicize text-obj (multiple-value-list
			       (opal:get-cursor-line-char-position text-obj))))
     )))))

(defun cmf-func (interact text-obj event)
  (declare (ignore interact event))
  (skip text-obj))

(defun cmb-func (interact text-obj event)
  (declare (ignore interact event))
  (skip-back text-obj))

(defun cmd-func (interact text-obj event)
  (declare (ignore interact event))
  (multiple-value-bind (line char)
      (opal:get-cursor-line-char-position text-obj)
    (skip text-obj)
    (opal:set-selection-to-line-char-position text-obj line char)
    (opal:toggle-selection text-obj t)
    (DELETE-LISP-REGION text-obj)))

(defun cmh-func (interact text-obj event)
  (declare (ignore interact event))
  (multiple-value-bind (line char)
      (opal:get-cursor-line-char-position text-obj)
    (skip-back text-obj)
    (opal:set-selection-to-line-char-position text-obj line char)
    (opal:toggle-selection text-obj t)
    (DELETE-LISP-REGION text-obj)))

(defun hash-func (interact text-obj event)
  (declare (ignore interact event))
  (let ((prev-char (opal:fetch-prev-char text-obj))
	(next-char (opal:fetch-next-char text-obj)))
    (when (eq prev-char #\|)
      (s-value text-obj :current-font
	       (opal:get-standard-font :fixed :roman :medium))
      (opal:insert-mark text-obj t :name :close-comment))
    (when (eq next-char #\|)
      (opal:insert-mark text-obj nil :name :open-comment)))
  (add-lisp-char text-obj #\#))

(defun bar-func (interact text-obj event)
  (declare (ignore interact event))
  (let ((prev-char (opal:fetch-prev-char text-obj))
	(next-char (opal:fetch-next-char text-obj)))
    (add-lisp-char text-obj #\|)
    (when (eq prev-char #\#)
      (opal:insert-mark text-obj t :name :open-comment))
    (when (eq next-char #\#)
      (opal:insert-mark text-obj t :name :close-comment))))

(defun quote-func (interact text-obj event)
  (declare (ignore interact event))
  (let ((need-mark (legit-font-p (g-value text-obj :current-font))))
    (add-lisp-char text-obj #\")
    (when need-mark
      (opal:insert-mark text-obj t :name :quote))))

(defun space-func (interact text-obj event)
  (declare (ignore interact event))
  (let ((next-char (opal:fetch-next-char text-obj))
	(prev-char (opal:fetch-prev-char text-obj)))
    (add-lisp-char text-obj #\space)
    (when (and (not (lisp-delim-char-p next-char))
	       (not (eq next-char #\;))
	       (not (lisp-delim-char-p prev-char))
	       (legit-font-p (g-value text-obj :current-font)))
      (opal:insert-mark text-obj nil :name :arg))))

(defun open-paren-func (interact text-obj event)
  (declare (ignore interact event))
  (let* ((need-mark (legit-font-p (g-value text-obj :current-font)))
	 (next-char (and need-mark (opal:fetch-next-char text-obj)))
	 (prev-char (and need-mark (opal:fetch-prev-char text-obj))))
    (add-lisp-char text-obj #\()
    (when need-mark
      (opal:insert-mark text-obj t :name :open)
      (when (and (not (lisp-delim-char-p next-char))
		 (not (eq next-char #\;))
		 (not (lisp-delim-char-p prev-char)))
	(opal:insert-mark text-obj nil :name :arg)))))

(defun close-paren-func (interact text-obj event)
  (declare (ignore interact event))
  (let* ((need-mark (legit-font-p (g-value text-obj :current-font)))
	 (next-char (and need-mark (opal:fetch-next-char text-obj)))
	 (prev-char (and need-mark (opal:fetch-prev-char text-obj))))
    (add-lisp-char text-obj #\))
    (when need-mark
      (opal:insert-mark text-obj t :name :close)
      (when (and (not (lisp-delim-char-p next-char))
		 (not (eq next-char #\;))
		 (not (lisp-delim-char-p prev-char)))
	(opal:insert-mark text-obj nil :name :arg)))))
