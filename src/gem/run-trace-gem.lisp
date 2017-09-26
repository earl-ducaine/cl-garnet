
(in-package :gem)

(defparameter *standard-output-bak* *standard-output*)
(defparameter *error-output-bak* *error-output*)
(defparameter *trace-output-bak* *trace-output*)
(defparameter *gem-trace-output*)

(defvar *trace-x* t)


(eval-when (:execute :compile-toplevel :load-toplevel)
  (when *trace-x*
    (push
     (lambda ()
        (setf *gem-trace-output* (make-string-output-stream)
       (setf *trace-output* (make-string-output-stream))
       (trace-gem :x))
     *gem-initialization-hooks*)))

;; Various stages of trace log processing
;;
;; *trace-output*        raw log stream because it's a stream once it's
;;                       procecced it's empty.
;;
;; *trace-output-string*  Trace output separated into trimmed lines
;;
;;

(defvar *raw-trace-output-string*)
(defvar *trace-output-string*)
(defvar *last-char* nil)

(defun run-do-go ()
  ;; reset stream
  (demo-3d:do-go))

(defun read-until-next-white-space (stream)
  (let (chars)
    (do ((char (read-char stream nil :eof)
	       (read-char stream nil :eof)))
	((or (member char '(#\Space #\Newline :eof))))
      (setf chars (format nil "~s~A" chars char)))
    chars))

(defun read-trace-output-stream ()
  (let ((raw-trace-output-string (get-output-stream-string *trace-output*)))
    (setf *raw-trace-output-string* raw-trace-output-string)
    (let ((items nil)
	  (trace-output-stream
	   (make-string-input-stream
	    (let ((strings (process-trace-output-stream-string
			    raw-trace-output-string)))
	      (setf *trace-output-string* strings)
	      (apply #'concatenate (cons 'string strings))))))
      (flet ((read-from-trace-stream ()
	       ;; (setf last-char
	       ;; 	     (read-char trace-output-stream nil :eof))
	       ;; (unread-char last-char trace-output-stream)
	       (restart-case
		   (handler-bind ((error #'(lambda (c)
					     (invoke-restart 'my-restart c))))
		     (read trace-output-stream nil :eof))
		 (my-restart (&optional v)
		   (format t "~s~%" v)
		   ;; (unread-char last-char trace-output-stream)
		   (let ((as-string
			  (read-until-next-white-space trace-output-stream)))
		     (format t "invalid token: ~s~%" as-string)
		     (break)
		     as-string)))))
	(turn-on-trace-k-reader)
	(do ((item
	      (read-from-trace-stream)
	      (read-from-trace-stream)))
	    ((eql item :eof))
	  (push item items))
	(reverse items)))))

(defun start-call-p (trace-string)
  (let ((end-digits
	 (dotimes (i (length trace-string) i)
	   (unless (digit-char-p (aref trace-string i))
	     (return i)))))
    (and (> end-digits 0)
	 (> (length trace-string) end-digits)
	 (char= (aref trace-string end-digits) #\:)
	 (parse-integer (subseq trace-string 0 end-digits)))))

(defun process-string (string)
  (let ((string (string-trim '(#\Space) string)))
    (flet ((get-index ()
	     (or (search "#<XLIB:" string)
		 (search "#<xlib:" string))))
      (do ((index (get-index) (get-index)))
	  ((null index))
	(format t "First index ~S" index)
	(setf string (concatenate 'string
				  (subseq string 0 index)
				  "#G<XLIB-"
				  (subseq string (+ 7 index)))))
      string)))

;; We assume that after trimming the lines look something like
;;
;;   "0: (X-DRAW-RECTANGLE #k<INTERACTOR-WINDOW-7158> 358 53 18 18 :COPY"
;;
;; or
;;
;;   "#k<OPAL:DEFAULT-LINE-STYLE> #k<OPAL:WHITE-FILL>)"
(defun process-trace-output-stream-string
    (raw-trace-output-string)
  (let (calls full-line)
    (reverse
     (dolist
	 (line
	   (mapcar
	    (lambda (string)
	      (process-string string))
	    (cl-ppcre:split #\Newline raw-trace-output-string))
	  (progn
	    (push full-line calls)
	    calls))
       (multiple-value-bind (call-level position) (start-call-p line)
	 (cond
	   (call-level
	    (let ((rest-line (subseq line (1+ position))))
	      (when full-line
		(push full-line calls))
	      (setf full-line rest-line)))
	   (t
	    (unless (> (length line) 2)
	      (format
	       t
	       "very short trace line: ~s~%Previous full call ~s~%"
	       line
	       full-line))
	    (setf full-line (concatenate 'string full-line " " line)))))))))

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

(defun validate-call-list (call-list)
  ;; first argument must be function all other objects must be atoms,
  ;; objects returned before that we know about or other calls.
  (let* ((call-object (create-instance nil nil))
	 (call-funcion-symbol (car call-list)))
    (flet ((push-argument-on-call-object (object arg)
	     (let ((arguments (gv call-object :arguments)))
	       (push arg arguments)
	       (s-value call-object :arguments arguments))
	     object))
    (unless (fboundp call-funcion-symbol)
      (error "First element in call list not function: ~s~%"
	     call-funcion-symbol))
    (s-value call-object :function (symbol-function call-funcion-symbol))
    (dolist (arg (cdr call-list))
      (cond
	((atom arg)
	 (push-argument-on-call-object call-object arg))
	 ((listp arg)
	  (push-argument-on-call-object call-object (validate-call-list arg)))
	 (t
	  (error "Unexpected argument found in call-list: ~s~%" arg))))
    call-object)))



;; takes a list structure and returns a call structure, i.e. a kr
;; object.
;; (defun get-call (call-list)
;;   (unless (validate-call-list call-list)
;;     (error "call-list appears to be invalid: ~s~%" call-list))


;; ;; create a list of calls and coresponding returns.
;; (defun run-test (trace)
;;   (do ((
