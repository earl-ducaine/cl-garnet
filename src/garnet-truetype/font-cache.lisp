;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CLX-TRUETYPE; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  The code in this file was written by Mikhail Filonenko and       ;;
;;  modified for integration with Garnet. It is used in accordance   ;;
;;  with the terms of the MIT license. See the file                  ;;
;;  MIT-LICENSE.clx-truetype in this directory.                      ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; The code in this file sets up a cache of font paths. The persistent
;;  cache is kept in the file "~/.fonts/font-cache.sexp".
;;

(in-package #:clx-truetype)

(defvar *font-dirs*
  #+(or unix netbsd openbsd freebsd) 
  (list "/usr/share/fonts/"
	(namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
  #+darwin 
  (list "/Library/Fonts/")
  #+windows
  ;; XXXX This depends on ASDF. Do we want that?
  (list (namestring
	 (merge-pathnames "fonts/" 
			  (pathname (concatenate 'string (asdf:getenv "WINDIR") "/")))))
  "List of directories, which contain TrueType fonts.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +font-cache-filename+ 
    #.(merge-pathnames "font-cache.sexp"
                       (merge-pathnames ".fonts/" (user-homedir-pathname)))))

;; family ->
;;   subfamily -> filename
;;   subfamily -> filename
(eval-when (:load-toplevel :execute)
  (defparameter *font-cache*
    (if (fad:file-exists-p +font-cache-filename+)
        (cl-store:restore +font-cache-filename+)
        (make-hash-table :test 'equal))
    "Hashmap for caching font families, subfamilies and files."))

;;(pushnew (xlib:font-path *display*) *font-dirs*)
(defun cache-font-file (pathname)
  "Caches font file."
  (handler-case 
    (zpb-ttf:with-font-loader (font pathname)
      (multiple-value-bind (hash-table exists-p)
          (gethash (zpb-ttf:family-name font) *font-cache*
                   (make-hash-table :test 'equal))
        (setf (gethash (zpb-ttf:subfamily-name font) hash-table)
              pathname)
        (unless exists-p
          (setf (gethash (zpb-ttf:family-name font) *font-cache*)
                hash-table))))
    (condition () (return-from cache-font-file))))

(defun ttf-pathname-test (pathname)
  (string-equal "ttf" (pathname-type pathname)))


(defun cache-fonts ()
  "Caches fonts from @refvar{*font-dirs*} directories."
  (clrhash *font-cache*)
  (dolist (font-dir *font-dirs*)
    (fad:walk-directory font-dir #'cache-font-file :if-does-not-exist :ignore
                        :test #'ttf-pathname-test))
  (ensure-directories-exist +font-cache-filename+)
  (cl-store:store *font-cache* +font-cache-filename+))

(defun get-font-families ()
  "Returns cached font families."
  (let ((result (list)))
    (maphash (lambda (key value)
               (declare (ignorable value))
               (push key result)) *font-cache*)
    (nreverse result)))

(defun get-font-subfamilies (font-family)
  "Returns font subfamilies for current @var{font-family}. For e.g. regular, italic, bold, etc."
  (let ((result (list)))
    (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (subfamily pathname)
                            (declare (ignorable pathname))
                            (push subfamily result)) value)
                 (return-from get-font-subfamilies 
                   (nreverse result)))) *font-cache*)
    (nreverse result)))


 
