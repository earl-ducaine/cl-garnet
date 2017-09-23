
(in-package :gem)

(defparameter *standard-output-bak* *standard-output*)
(defparameter *error-output-bak* *error-output*)
(defparameter *trace-output-bak* *trace-output*)

(setf *trace-output* (make-string-output-stream))

(trace-gem :x)

(defvar *trace-output-stream*)
(defvar *last-char* nil)

(defun run-do-go ()
  (demo-3d:do-go))

(defun read-until-next-white-space (stream)
  (let (chars)
    (do ((char (read-char stream nil :eof)
	       (read-char stream nil :eof)))
	((or (member char '(#\Space #\Newline :eof))))
      (setf chars (format nil "~s~A" chars char))
      chars)))

(defun read-trace-output-stream ()
  (let ((last-char nil)
	(items nil)
	(trace-output-stream
	 (make-string-input-stream
	  (let ((strings (process-trace-output-stream-string
			  (get-output-stream-string *trace-output*))))
	    (apply #'concatenate (cons 'string strings))))))
    (flet ((read-from-trace-stream ()
	     (setf last-char
		   (read-char trace-output-stream nil :eof))
	     (unread-char last-char trace-output-stream)
	     (read trace-output-stream nil :eof)))
      (turn-on-trace-k-reader)
      (restart-case
	  (handler-bind ((error #'(lambda (c)
				    (invoke-restart 'my-restart c))))
	    (do ((item
		     (read-from-trace-stream) (read-from-trace-stream)))
	      ((eql item :eof))
	    (push item items))
	  items)
	(my-restart (&optional v)
	  (format t "~s~%" v)
	  (unread-char last-char trace-output-stream)
	  (push (read-until-next-white-space trace-output-stream)
		items)
	  (turn-off-trace-k-reader)
	  items)))))

(defun start-call-p (trace-string)
  (let ((end-digits
	 (dotimes (i (length trace-string) i)
	   (unless (digit-char-p (aref trace-string i))
	     (return i)))))
    (and (> end-digits 0)
	 (> (length trace-string) end-digits)
	 (aref trace-string end-digits)
	 (parse-integer (subseq trace-string 0 end-digits)))))

;; We assume that after trimming the lines look something like
;;
;;   "0: (X-DRAW-RECTANGLE #k<INTERACTOR-WINDOW-7158> 358 53 18 18 :COPY"
;;
;; or
;;
;;   "#k<OPAL:DEFAULT-LINE-STYLE> #k<OPAL:WHITE-FILL>)"
(defun process-trace-output-stream-string
    (&optional (trace-output-stream-string *trace-output-stream-string*))
;;  (turn-on-trace-k-reader)
;;  (mapcar #'read-from-string
	  (let (calls full-line)
	    (dolist (line
		      (mapcar
		       (lambda (string) (string-trim '(#\Space) string))
		       (cl-ppcre:split #\Newline trace-output-stream-string))
		     calls)
	      (multiple-value-bind (call-level position) (start-call-p line)
		(cond
		  (call-level
		   (let ((rest-line (subseq line (1+ position))))
		     (when full-line
		       (push full-line calls))
		     (setf full-line rest-line)))
		  (t
		   (unless (> (length line) 2)
		     (format t "very short trace line: ~s~%Previous full call ~s~%" line full-line))
		   (setf full-line (concatenate 'string full-line " " line))))))))
  ;;(turn-off-trace-k-reader))

;; (defun read-trace ()
;;   (apply #'concatenate (cons 'string TRACE-STRINGS)))


(defun k-reader-alt (stream subchar arg)
  "Modify the readtable so #k<NAME> is read as the symbol 'k<NAME>"
  (declare (ignore subchar arg))
  (let ((next-char (read-char stream)))
    (if (char= next-char #\<)
	;; This is a KR #k<...> object name
	(let ((string ""))
	  (do ((c (read-char stream) (read-char stream)))
	      ((char= c #\>))
	    (setf string (format nil "~A~C" string c)))
	  (read-from-string string))
	;; This is something else
	(cerror
	 "Ignore the token"
	 "  Illegal character ~S after reader macro #k (expecting \"<\")"
	 next-char))))

(defun g-reader-alt (stream subchar arg)
  "Modify the readtable so #g<NAME> is read as the symbol 'k<NAME>"
  (declare (ignore subchar arg))
  (let ((next-char (read-char stream)))
    (if (char= next-char #\<)
	;; This is a KR #k<...> object name
	(let ((string ""))
	  (do ((c (read-char stream) (read-char stream)))
	      ((char= c #\>))
	    (setf string (format nil "~A~C" string c)))
	  (read-from-string string))
	;; This is something else
	(cerror
	 "Ignore the token"
	 "  Illegal character ~S after reader macro #k (expecting \"<\")"
	 next-char))))

(defparameter *k-reader-stack* '())
(defparameter *g-reader-stack* '())

(defun turn-on-trace-k-reader ()
  (push (get-dispatch-macro-character #\# #\k) *k-reader-stack*)
  (set-dispatch-macro-character #\# #\k (function gem::k-reader-alt))
  (push (get-dispatch-macro-character #\# #\g) *g-reader-stack*)
  (set-dispatch-macro-character #\# #\g (function gem::k-reader-alt)))

(defun turn-off-trace-k-reader ()
  (when *k-reader-stack*
    (set-dispatch-macro-character #\# #\g (pop *g-reader-stack*))))


;;;(set-dispatch-macro-character #\# #\k (function kr::k-reader))



;;;(defun turn-on-
;;;; (read-from-string  "(X-CREATE-PIXMAP #k<INTERACTOR-WINDOW-7158> 16 16 1 NIL NIL NIL)")


;; (read-from-string readable-trace-stream)
;; (defparameter trace-stream (make-string-input-stream readable-trace-stream))
;;; (read trace-stream)
