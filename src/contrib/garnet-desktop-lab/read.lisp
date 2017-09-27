(defpackage :elisp
  (:use :common-lisp)
  (:export while
	   short-pi))

(in-package :elisp)

(defparameter unrch nil)

;;; Elisp object
(defclass elisp-object ()
  ())

(defclass elisp-t (elisp-object)
  ())

(defmethod readchar :around ((object elisp-tt))
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
  (let ((symbol (make-instance 'elisp-symbol)))
    (with-slots   (name value function) symbol
      (setf name (string-upcase symbol-string)
	    value nil
	    function nil)
      (setf (gethash name *elisp-symbols*) symbol)
      symbol)))
  

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
  `((42 "43 (defu")
    (nil "4i3 (defu")
    (,(elisp-intern "defun") "defun shell-mode ()")
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


