run-program


(defun youtube-dl (url)
  (let ((shell-string
	 (shell-command-to-string
	  (concat "cd "  *default-music-directory* "; youtube-dl -k " url))
	 ))
     (let ((file-string-list (split-string
 			(if (stringp shell-string) shell-string "")
 			"\n")))
       (clean-file-string-list
        (cl-reduce (lambda (file-spec output-line)
		     ;; Note, string-match returns either nil or some
		     ;; number [0-NUMBER_OF_MATCHES_MINUS_ONE].  Since
		     ;; elisp interprets both 0 and nil as false, we
		     ;; need to explicity check for null to see if a
		     ;; match was found.
		     (cond
		      ((not (null (string-match
				   ".download. Destination: \\(.*\\)\\.\\(\..*\\)"
				   output-line)))
		       (cons (list (match-string 1 output-line)
				   (match-string 2 output-line) 'partial)
			     file-spec))
		      ((not (null (string-match
				   ".ffmpeg. Merging formats into \\(.*\\)\\.\\(\..*\\)"
				   output-line)))
		       (cons (list (match-string 1 output-line)
				   (match-string 2 output-line) 'main)
			     file-spec))
		      (t file-spec)))
		   file-string-list
		   :initial-value)))))


(defun convert-to-flac (source-file)
 (shell-command-to-string
   (join (list "ffmpeg" "-i" (concat source-file ".mkv")
	       "-acodec" (concat (file-name-sans-extension source-file) ".flac")))))

(defun interactive-normalize-file-name ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((filename (buffer-string)))
      (erase-buffer)
      (insert (normalize-file-name filename)))))

(defun join (the-list delim)
  (mapconcat 'identity the-list  delim))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed,
ASCII 10)."
  (replace-regexp-in-string
   "\\`[\t\n]*"
   ""
   (replace-regexp-in-string
    "[ \t\n]*\\'"
    ""
    string)))

(defvar *default-music-directory* "~/music")
(defvar *last-file-downloaded* nil)

(defun rename-download-file (from-file-name to-file-name)
  (let ((from-path (concat *default-music-directory* "/" from-file-name))
	(to-path (concat *default-music-directory* "/" to-file-name)))
    (rename-file from-path to-path)))

(defun delete-download-file (file-name)
    (delete-file (concat *default-music-directory* "/" file-name)))

(defun clean-file-string-list (file-string-list)
  (cl-mapcar
   (lambda (file-string-spec)
     (list (replace-regexp-in-string "[\"]" "" (car file-string-spec))
	   (replace-regexp-in-string "[\"]" "" (cadr file-string-spec))
	   (caddr file-string-spec)))
   file-string-list))


(defun process-convert-file (rename-to-base)
  (let* ((rename-to (concat rename-to-base ".flac"))
	(convert-output (shell-command-to-string
			 (concat "cd "  *default-music-directory* "; ffmpeg -i "
				 "\"" *last-file-downloaded* "\" "
				 "-acodec flac pop/" rename-to))))
    (delete-download-file *last-file-downloaded*)
    convert-output))

(defun normalize-file-name (filename)
  (replace-regexp-in-string
   "-+"
   "-"
   (replace-regexp-in-string "[ _!\"'()&,~.]" "-" (downcase filename))))

(defun interactive-normalize-file-name ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((filename (buffer-string)))
      (erase-buffer)
      (insert (normalize-file-name filename)))))

;;; (process-youtube-dl "https://www.youtube.com/watch?v=HJNrKHv50X8")

(defun process-youtube-dl (override-url)
  (let* ((url (or override-url (buffer-string)))
	 (original-file-names (youtube-dl url))
	 (file-spec
	  (dolist (file-spec original-file-names m4a)
	    (cond
	     ((eq (caddr file-spec) 'main)
	      (setq m4a file-spec))
	     ((eq (caddr file-spec) 'partial)
	      (delete-download-file (concat (car file-spec) "." (cadr file-spec)))))))
	 (replacement-text
	  (replace-regexp-in-string
	   "-+"
	   "-"
	   (replace-regexp-in-string "[ _!\"'()&,~.]" "-" (downcase (car file-spec))))))
    (let* ((rename-from (concat (car file-spec) "." (cadr file-spec)))
	   (rename-to (concat replacement-text "." (cadr file-spec))))
      (rename-download-file rename-from rename-to)
      (setf *last-file-downloaded* rename-to)
      replacement-text)))

(defvar *process-get-file-in-progress* nil)

(defun process-get-file ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((buffer-string (buffer-string)))
      (erase-buffer)
      (insert
       (cond (*process-get-file-in-progress*
	      (setq *process-get-file-in-progress* nil)
	      (process-convert-file buffer-string))
	     (t
	      (setq *process-get-file-in-progress* t)
	      (process-youtube-dl buffer-string)))))))
