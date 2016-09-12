
(in-package #:clx-freetype2-renderer)

(export '(*font-cache*
	  cache-fonts
	  get-font-styles
	  get-font-families))

(defvar *font-dirs* #+(or unix netbsd openbsd freebsd) (list "/usr/share/fonts/"
                                     (namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
        "List of directories, which contain Freetype2 fonts.")

;; family ->
;;   style -> filename
;;   style -> filename
(defparameter *font-cache* (make-hash-table :test 'equal))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +font-cache-filename+ 
    #.(merge-pathnames "ft2-font-cache.sexp"
                       (merge-pathnames ".fonts/" (user-homedir-pathname)))))

(eval-when (:load-toplevel :execute)
  (defparameter *font-cache*
    (if (fad:file-exists-p +font-cache-filename+)
        (cl-store:restore +font-cache-filename+)
        (make-hash-table :test 'equal))
    "Hashmap for caching font families, styles and files."))


(defun cache-font-file (pathname)
  "Caches font file."
  (declare (special *font-cache*))
  (handler-case 
      (ft2:with-open-face (font pathname)
	(multiple-value-bind (hash-table exists-p)
	    (gethash (ft2::ft-face-family-name font) *font-cache*
		     (make-hash-table :test 'equal))
	  (setf (gethash (ft2::ft-face-style-name font) hash-table) pathname)
	  (unless exists-p
	    (setf (gethash (ft2::ft-face-family-name font) *font-cache*)
		  hash-table))))
    (condition () (return-from cache-font-file))))

(defun ft2-pathname-test (pathname)
  (let ((suffix (pathname-type pathname)))
    (or (string-equal "ttf" suffix) (string-equal "otf" suffix))))

(defun cache-fonts (&optional (host ""))
  "Caches fonts from @refvar{*font-dirs*} directories."
  (declare (special *font-cache*))
  (clrhash *font-cache*)
  (append (xlib:font-path (xlib:open-display host)) *font-dirs*)  
  (dolist (font-dir *font-dirs*)
    (fad:walk-directory font-dir #'cache-font-file :if-does-not-exist :ignore
                        :test #'ft2-pathname-test))
  (ensure-directories-exist +font-cache-filename+)
  (cl-store:store *font-cache* +font-cache-filename+))

(defun get-font-families ()
  "Returns cached font families."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (key value)
               (declare (ignorable value))
               (push key result)) *font-cache*)
    (nreverse result)))

(defun get-font-styles (font-family)
  "Returns font styles for current @var{font-family}. For e.g. regular, italic, bold, etc."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (style pathname)
                            (declare (ignorable pathname))
                            (push style result)) value)
                 (return-from get-font-styles 
                   (nreverse result)))) *font-cache*)
    (nreverse result)))

