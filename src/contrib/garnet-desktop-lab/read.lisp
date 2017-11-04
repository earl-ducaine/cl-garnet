(defpackage :elisp
  (:use :common-lisp)
  (:export while
	   short-pi))

(in-package :elisp)

(ql:quickload :esrap)
(use-package :esrap)
(use-package :alexandria)

(defparameter special-characters '(#\" #\\ #\; #\?  #\( #\) #\. #\[ #\] #\#))
(defparameter escape-character '(#\\))
(defparameter generic-delimiters '(#\' #\` #\& #\% #\$ #\!))

(defun non-string-char-p (char)
  (and (not (eql #\" char))
       (graphic-char-p char)
       (not (eql #\! char))))

(defun not-doublequote (char)
  (and (not (eql #\" char))
       (graphic-char-p char)))

(defparameter *whitespace-characters*
  '(#\space #\tab #\Newline #\Return))

(defun is-not-special-char-p (char)
  (and (graphic-char-p char)
       (not (member char (union *whitespace-characters*
				special-characters)))))

;;(defrule alphanumeric (or (is-not-special-char-p character)))


(defrule alphanumeric (graphic-char-p character))

(defrule string-char (or (not-doublequote character) (and #\\ #\")))

(defrule non-string-char (or (non-string-char-p character) (and #\\ #\")))


(defrule comment (and #\! (* (graphic-char-p character)) #\Newline)
  (:destructure (ex c nl)
		(declare (ignore ex nl))
		 (list :comment (coerce (cons #\! c) 'string))))

(defrule whitespace-characters (+ (or #\space #\tab #\Newline #\Return))
  (:constant nil))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (list :string string)))

(defrule non-comment (or string non-string)
  (:destructure (string &rest str)
		 (cons string str)))


(defrule program-text (+ (or comment non-comment))
  (:destructure (string &rest str)
		 (cons string str)))

(defrule programe-text2 (or (? whitespace-characters)
			   (or magic list atom comment-line))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))


(defrule non-string (+ non-string-char)
  (:lambda (text)
    (list :non-string (coerce text 'string))))


;; comment
;;; (defrule program-text (or string  non-string))





(defun textual-preprocessor (text))


(defparameter current-call 'INTERACTORS::Find-Final-Feedback-Obj)

(swank/sbcl::defxref :who-calls INTERACTORS::Find-Final-Feedback-Obj)

(defparameter *ql-systems* (quicklisp-client:system-list))


;;; Operations that we'll need
;;; All packages     (list-all-packages)
;;; All the symbols in a package
;;; The root directory of the systmem that we're interested in
;;; A function to check whether the symbol is define in one of the files




(defun run-dir-ancestor-p ()
  (dir-ancestor-p
   #P"/home/rett/quicklisp/dists/quicklisp/software/cl-fad-0.7.4/"
   #P"/home/rett/quicklisp/dists/quicklisp/software/cl-fad-0.7.4/path.lisp"))


(defun dir-ancestor-p (ancestor path)  
  (let ((path (cl-fad:pathname-directory-pathname path)))
    (cond ((cl-fad:pathname-equal ancestor path)
	   t)
	  ((not (cl-fad:pathname-equal path #p"/"))
	   (dir-ancestor-p ancestor (cl-fad:pathname-parent-directory path)))
	  (t
	   nil))))



(ql:quickload :cl-fad)
(let ((ancestor  #P"/home/rett/quicklisp/dists/quicklisp/software/cl-fad-0.7.4/")
      (path #P"/home/rett/quicklisp/dists/quicklisp/software/cl-fad-0.7.4/path.lisp"))
  (cl-fad:pathname-equal ancestor (cl-fad:pathname-directory-pathname path)))

(cl-fad:pathname-directory-pathname path)



(defun run-get-all-functions-in-package ()
  (get-all-functions-in-package (car (list-all-packages))))


(defun get-all-functions-in-package (package)
  (let (symbols)
    (do-symbols (symbol package symbols)
      (when (fboundp symbol)
	(push symbol symbols)))))

(defun get-source-directory ()
    (asdf::system-source-directory (asdf:find-system 'cl-fad)))


(let (source-locations)
  (dolist (def  (run-get-all-functions-in-package) source-locations)
    (sb-introspect:find-definition-source (fdefinition def))
    (push (sb-introspect:find-definition-source (fdefinition def))
	  source-locations)))



(defun who-calls (symbol)
  (sb-introspect:who-calls symbol))

(defun run-who-calls ()
  (who-calls 'interactors::find-final-feedback-obj))

(quicklisp-client:system-list)


(quicklisp-client::name (first *ql-systems*))
(quicklisp-client::system-file-name (first *ql-systems*))
(quicklisp-client::release (first *ql-systems*))
(quicklisp-client::dist (first *ql-systems*))
(quicklisp-client::required-systems (first *ql-systems*))
(quicklisp-client::metadata-name (first *ql-systems*))



(ql:quickload 
 (make-symbol (string-upcase (quicklisp-client::name (first *ql-systems*)))))








(defclass system (preference-mixin)
  ((name
    :initarg :name
    :accessor name
    :reader short-description)
   (system-file-name
    :initarg :system-file-name
    :accessor system-file-name)
   (release
    :initarg :release
    :accessor release
    :reader preference-parent)
   (dist
    :initarg :dist
    :accessor dist)
   (required-systems
    :initarg :required-systems
    :accessor required-systems)
   (metadata-name
    :initarg :metadata-name
    :accessor metadata-name))
  (:default-initargs
   :metadata-name "systems"))


;; build who-calls database for all quicklisp items
INTERACTORS::Find-Final-Feedback-Obj





(defparameter unrch nil)

;;; Elisp object
(defclass elisp-object ()
  ())

(defclass elisp-t (elisp-object)
  ())

(defmethod readchar :around ((object elisp-t))
  (when unrch
    (let ((c unrch))
      (setf unrch nil)
      c)))

(defclass elisp-symbol (elisp-t)
  (name package plist value function))


;; (defclass elisp-buffer ()
;;     ((dotloc :initarg :dotloc :initform nil :accessor dotloc)
;;      (p1 :initarg :p1 :initform nil :accessor p1)
;;      (p2 :initarg :p2 :initform nil :accessor p2)
;;      (size1 :initarg :size1 :initform nil :accessor size1)
;;      (size2 :initarg :size2 :initform nil :accessor size2)
;;      (tail-clip :initarg :tail-clip :initform nil :accessor tail-clip))
;;   )


;;; Elisp buffer
(defclass elisp-buffer (elisp-object)
    ((text :initarg :text :initform nil :accessor text)))

(defmethod readchar ((buffer elisp-buffer))
  (unless (call-next-method)
    (format t "got a value")))

;;; Elisp marker
(defclass elisp-marker (elisp-object)
    ((text :initarg :text :initform nil :accessor text)))

(defmethod readchar ((marker elisp-marker))
  )

;;; Elisp stream

(defclass elisp-stream (elisp-object)
    ())

(defmethod readchar ((stream elisp-file-stream))
  )

;;; Elisp stream
(defclass elisp-string (elisp-object)
    ())

(defmethod readchar ((string elisp-string))
  )

(defclass elisp-function (elisp-object)
    ())

(defmethod readchar ((function elisp-function))
  (unless (call-next-method)
    (format t "got a value")))



;;; Elisp function interface
(defvar *current-read-source* nil)

;; Get a character from the tty
(defun read-char ()
  "Read a character from the command input (keyboard or macro).
   It is returned as a number."
  (readchar *current-read-source*))

;; export
(defun get-file-char ()
  "Don't use this yourself."
  (readchar *current-read-source*))
					;
;; export
(defun elisp-load (file-str missing-ok)
  "Execute a file of Lisp code named FILE.
   First tries FILE with .elc appended, then tries with .el,
   then tries FILE unmodified.  Searches directories in  load-path.
   If optional second arg MISSING-OK is non-nil,
   report no error if FILE doesn't exist.
   Print messages at start and end of loading unless
   optional third arg NOMESSAGE is non-nil.
   Return t if file exists."
  (let* ((count  (- specpdl-ptr specpdl))
	 (file-str (fsubstitute-in-file-name file-str))
	 (fd = (openp 'vload-path file-str ".elc" 0 0))
	 stream fd elispstream gcpro gcpro1)
    (unless (and file-str (> (length file-str) 0))
      (error "Empty file name"))
    (unless fd
      (setf fd (openp 'vload-path file-str ".el" 0 0)))
    (unless fd
      (setf fd (openp 'vload-path file-str "" 0 0)))
    (unless fd
      (if (null missing-ok)
	  (error "cannot open load file")
	  (return nil)))
    (let ((stream (fdopen fd "r")))
      (xsettype lispstream lisp-internal-stream)
      (xsetint (lispstream int) stream)
      (unless nomessage
	(message "loading %s..." file-str))
      (gcpro1 str)
      (record-unwind-protect closefile lispstream)
      (readevalloop qget-file-char stream feval 0)
      (unbind-to count)
      (unless nomessage
	(message "loading %s...done" file-str)))))

;; exec-only nonzero means don't open the files, just look for one
;; that is executable; returns 1 on success, having stored a string
;; into storeptr
(defun openp (path file-str suffix exec-only)
  (let (file-stream file-name want-size file st storeptr absolute file return-val
		    (file-name-size 100)
		    (buf (make-array 100))
		    (absolute  0))
    (when (member (aref file-str 0) '(#\~ #\/))
      (setf absolute t))
    (do ((path path (cdr path)))
	((null path))
      (setf file (expand-file-name str (car path)))
      (setf want-size  (+ (strlen suffix)  (+ (length file-size 1))))
      (setf file-name-size (+ 100 want-size))
      (when (< file-name-size want-size)
	(setf file-name (make-array file-name-size)))
      (setf file-name (concatenate 'string file suffix))
      (if exec-only
	  (when (and (file-executable-p file-name)
		     (directory-p file-name))
	    (when  return-val
	      (setf return-val file-name))
	    (return)
	    (progn
	      (setf file-stream
		    (open file-name :direction :io :if-does-not-exist :error))
	      (return)))))
    (values file-stream file-name)))

(defun file-executable-p (file-name)
  (let ((file-permissions (osicat:file-permissions file-name)))
    (or (:user-exec file-permissions)
	(:group-exec file-permissions)
	(:other-exec file-permissions))))

(defun closefile (elisp-stream)
  (close elisp-stream))

(defvar read-pure)

(defparameter
    *shell-el*
  "/home/rett/dev/garnet/cl-garnet/src/contrib/garnet-desktop-lab/elisp/shell.el")

(defun read0 (stream)
  )

(defun read-list (stream &optional flag)
  (let (elt val tail)
    (iter (for elt next (or (read-char stream nil)
			     (finish)))
	  (read1 stream))))

(defparameter special-characters '(#\" #\\ #\; #\?  #\( #\) #\. #\[ #\] #\#))
(defparameter escape-character '(#\\))
(defparameter generic-delimiters '(#\' #\` #\& #\% #\$ #\!))


(defun special-character-p (c)
  (member c special-characters))

(defun escape-character-p (c)
  (char= c escape-character))

(defun generic-delimiter-p (c)
  (member c generic-delimiters))

(defun white-space-p (c)
  (or (char= c #\Space)
      (not (graphic-char-p c))))


(defun str (&rest args)
  (apply 'concatenate (cons 'string args)))

;; delimiters:  '(#\' #\` #\& #\% #\$ #\!)
;; #\Newline
;; alphanumericp: a-z, A-Z, 0-9,
;; White space:  #\Space and nongraphical-char-p
;; punctuation: #\. #\: #\, #\* #\+ #\- #\/ #\| #\~
;;              #\^ #\< #\= #\> #\@ #\{ #\}

(defun get-string-elisp-integer (token)
  (let ((token-length (length token))
	(all-digits t))
    (when (> token-length 0)
      (dotimes (i token-length)
	(unless (digit-char-p (aref token i))
	  (setf all-digits nil)
	  (return)))
      (when all-digits
	(parse-integer token)))))

(defparameter *elisp-symbols*
  (make-hash-table :test #'equal))

(defun elisp-intern (symbol-string)
  (let ((name (string-upcase symbol-string)))
    (or (gethash name *elisp-symbols*)
	(let ((symbol (make-instance 'elisp-symbol)))
	  (with-slots   (name value function) symbol
	    (setf name (string-upcase symbol-string)
		  value nil
		  function nil)
	    (setf (gethash name *elisp-symbols*) symbol)
	    symbol)))))


;; Note, we assume that we have the standard cast of characters.  In
;; particular that our stream is either an ascii filestream or an
;; ordinary CL character stream.
(defun read1 (stream)
  (let ((dispatch-char (peek-char nil stream))
	(token ""))
    (case dispatch-char
      (#\( (read-list stream))
      (t
       (iter (for char next (or (read-char stream nil)
				(finish)))
	     (when (or (special-character-p char)
		       (white-space-p char)
		       (generic-delimiter-p char))
	       (finish))
	     (setf token (str token (string char))))))
    (or (get-string-elisp-integer token)
	(elisp-intern token))))

(defparameter read1-tests
  `((43 "43 (defu")
    (,(elisp-intern "defun") "defun shell-mode ()")
    (,(elisp-intern "4i3") "4i3 (defu")
    ( 43 "43")
    (42 ,(str "42" (string #\Newline) " "))))

(defun run-read1-tests ()
  (dolist (test read1-tests)
    (apply #'run-read1-test test)))

(defun run-read1-test (expected-value stream-string)
  (case
  (assert
   (with-input-from-string (stream stream-string)
     (apply comparator  (list expected-value (read1 stream))))
   (stream-string expected-value)
   "Unable to parse the correct value: ~s  -- from: ~s~%" expected-value stream-string))

  ;; (with-input-from-string (stream "42")
  ;;   (read1 stream)))







(defun readevalloop (readcharfun stream evalfun printflag)
  (let ((c val xunrch
	   (count (- specpdl-ptr specpdl))
	   (specbind qstandard-input readcharfun)
	   (unrch -1)


	   (with-open-file (stream *shell-el*)
	       (iter (for char next (or (read-char stream nil)
					(finish)))
		     ;; eat up comments
		     (when (char= char #\;)
		       (iter (for char next (let ((comment-char (read-char stream nil)))
					      (when (or (null comment-char)
							(char= comment-char #\Newline))
						(finish))))))
		     ;; eat up white space
		     (when (member char '(#\Space #\Tab #\Return #\Linefeed))
		       (next-iteration))
		     (unread-char char stream)
		     (let ((val (read1 stream)))
		       (elisp-eval val))))










	   (iterate
     (setf instream stream)
     (setf c (readchar))
     (when (= c #\;)
       (do ((c  (readchar) (readchar)))
	   ((and (/= c #\Return)
		 (/= c :eof)))))
     (if (member c #\Space #\Tab #\Return #\Linefeed)
	 (next-iteration)
	 (finish)))
     (
    (if (!null (vpurify-flag) && c == '(') (
      record-unwind-protect (unreadpure qnil)
      val = read-list (-1 readcharfun)
      unbind-to (count + 1)
    ) else (
      unread (c)
      val = read0 (readcharfun)
    )
    xunrch = unrch
    unrch = -1
    val = evalfun (val)
    if (printflag) (
      vvalues = fcons (val vvalues)
      if (eq (vstandard-output qt)) (
	fprin1 (val qnil)
      ) else (
	fprint (val qnil)
      )
    )
    (setf unrch xunrch))



	(let ((c #\Space)i)
	  (iterate (for i next t)
	   (if (member c '(#\Space #\Tab #\Return #\Linefeed))
	       (next-iteration)
	       (finish))
	   (setf c )))



	(let ((c #\Space)i)
	  (iterate
	   (for i next t)
	   (if (member c #\Space #\Tab #\Return #\Linefeed)
	       (next-iteration)
	       (finish))
	   (setf c )))




	(initially (setq i 0))
	(for i next (if (> i 10) (terminate) (incf i)))






















;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(defrule whitespace-characters (+ (or #\space #\tab #\newline #\Linefeed))
  (:constant nil))


(defparameter *whitespace-characters* '(#\space #\tab #\newline #\Linefeed))

(defun is-not-special-char-p (char)
  (and (graphic-char-p char)
       (not (member char (union *whitespace-characters*
				special-characters)))))

(defrule alphanumeric (or (is-not-special-char-p character)))

(defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with
;;; possibly leading whitespace.



(defun non-newline-p (char)
  (not (newline-p char)))

(defun newline-p (char)
  (and (member char '(#\Newline #\Linefeed #\Return))
       t))

(defrule non-newline (non-newline-p character))

(defrule newline (newline-p character))

(defrule comment-line  (and (? whitespace-characters)
			    (+ #\;)
			    (* non-newline)
			    #\Newline))

			  ;; (and
			  ;;      (* #\;)
			  ;;      (+ non-newline)
			  ;;      #\Newline)))
			    (? #\Linefeed)))


(defrule sexp (and (? whitespace-characters) (or magic list atom comment-line))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))

(defrule magic "foobar"
  (:constant :magic)
  (:when (eq * :use-magic)))

(defrule list (and #\( sexp (* sexp) (? whitespace-characters) #\)
		   (? whitespace-characters))
  (:destructure (p1 car cdr w1 p2 w2)
    (declare (ignore p1 p2 w1 w2))
    (cons car cdr)))

(defrule atom (or string integer symbol))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))

;;;; Try these

(parse 'sexp "FOO123")

(parse 'sexp "123")

(parse 'sexp "\"foo\"")

(parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(parse 'sexp "foobar")

(let ((* :use-magic))
  (parse 'sexp "foobar"))

(describe-grammar 'sexp)

(trace-rule 'sexp :recursive t)

(parse 'sexp "(foo bar 1 quux)")

(untrace-rule 'sexp :recursive t)

(defparameter *orig* (rule-expression (find-rule 'sexp)))

(change-rule 'sexp '(and (? whitespace) (or list symbol)))

(parse 'sexp "(foo bar quux)")

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

(change-rule 'sexp *orig*)

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)



(parse 'sexp "(defvar last)")

(defparameter *parse-text* "(defvar last-input-start nil \"In a shell-mode buffer, marker for start of last unit of input.\")")

(parse 'sexp *parse-text*)

(defun top-level-filter (file-name)
  (with-open-file (stream file-name)
    (iter (for char next (or (read-char stream nil)
			     (finish)))
	  ;; eat up comments
	  (when (char= char #\;)
	    (iter (for char next (let ((comment-char (read-char stream nil)))
				   (when (or (null comment-char)
					     (char= comment-char #\Newline))
				     (finish)))))
	    (next-iteration))
	  ;; eat up white space
	  (when (member char '(#\Space #\Tab #\Return #\Linefeed))
	    (next-iteration))
	  (unread-char char stream)
	  (finish))
    (let ((el-file-contents
	   (alexandria:read-stream-content-into-string stream)))
      (when (and el-file-contents
		 (> (length el-file-contents) 0))
;;	el-file-contents))))
	(parse 'sexp el-file-contents)))))


	  (let ((val (read1 stream)))
	    val))))
	    (elisp-eval val)))))
(parse 'sexp *parse-text*)


(defun run-top-level-filter ()
  (top-level-filter (str "/home/rett/dev/garnet/cl-garnet/src/contrib/"
			 "garnet-desktop-lab/elisp/shell-partial.el")))




(defrule programe-text (and (? whitespace-characters) (or magic list atom comment-line))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))
