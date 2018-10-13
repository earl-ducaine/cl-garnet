;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER  -*-

(in-package :common-lisp-user)

;; Not likely to be anywhere in the world where this would be useful.
(defparameter garnet-version-number "3.3.post")
(pushnew :garnet *features*)
(pushnew :garnet-v3 *features*)
(pushnew :garnet-v3.3.post *features*)
(pushnew :garnet-test *features*)
;; The :garnet-debug feature allows many different kinds of run-time
;; checking, and also loads some extra test code. After you have
;; debugged your code and want it to run faster, remove :GARNET-DEBUG
;; from the *features* list and RECOMPILE all of Garnet and your code.
;; The result will be smaller and somewhat faster.
;;
;; To remove :garnet-debug from the *features* list, either defvar
;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or
;; simply edit the following defvar to set Garnet-Garnet-Debug to nil.
;;
;; TODO (ed): I have a pathological hatred of using *features*.  I find it makes
;; for hideously ugly code.  So, at some point this should be changed
;; to a runtime special variable that dynamically controls this.  That
;; will forfit code size, but will still allow for optimizing
;; production code.

(defvar garnet-garnet-debug t)
(if garnet-garnet-debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))

;; The following variable affects compiler policy. Setting it to T
;; uses the settings in *garnet-compile-debug-settings*. Setting it to
;; NIL uses the ones in *garnet-compile-production-settings*. By
;; default we simply mirror Garnet-Garnet-Debug.
;; (defvar garnet-compile-debug-mode garnet-garnet-debug
;;   "Setting this variable to T sets the policy for the entire system
;; to make it more debuggable.")

;; (defvar garnet-compile-debug-settings
;;   '(optimize (speed 2)
;;     (safety 3)
;;     (debug 3)
;;     (space 2))
;;   "Use these settings for globally debugging the system or for debugging
;; a specific module. They emphasize debuggability at the cost of some speed.

;; With SBCL:

;; - These settings are type-safe.

;; - They prevent functions declared inline from being expanded inline.
;;   Note that as part of this version I have tried to make most
;;   non-syntactic macros into inline functions.

;; - They allow all possible debugging features.")

;; (defvar garnet-compile-production-settings
;;   '(optimize (speed 3)
;;     (safety 0)
;;     (space 1)
;;      (debug 1)
;;     (compilation-speed 0))
;;   "production compiler policy settings. emphasize speed, de-emphasize debugging.")

;; (defvar default-garnet-proclaim
;;   (if garnet-compile-debug-mode
;;       garnet-compile-debug-settings
;;       garnet-compile-production-settings)
;;   "Set compiler optimization settings.

;; 1. If you want everything debugged, set Garnet-Compile-Debug-Mode to t.

;; 2. If you want to debug specific modules, set Garnet-Compile-Debug-Mode
;;    to nil. Then set the variable in the modules you want debugged to enable
;;    debugging that module.

;; 3. Otherwise (for 'production' builds) just set Garnet-Compile-Debug-Mode
;;    to nil and leave everything else alone.")

(defun append-directory (directory sub-directory)
  "This is a little utility for accessing the subdirectory of a
   directory. It assumes that 'sub-directory' is directly under
   'directory'."
  (let ((dir (pathname-directory directory))
        (subdir (if (listp sub-directory)
                    sub-directory
                    (list sub-directory))))
    (make-pathname :directory (append dir subdir))))

(defun get-garnet-binary-pathname ()
  (let ((directory-name "src"))
    (append-directory org.xoanonos.asdf-app-config:*base-directory* directory-name)))

(defvar garnet-src-pathname (append-directory  org.xoanonos.asdf-app-config:*base-directory* "src"))
(defvar garnet-lib-pathname (append-directory org.xoanonos.asdf-app-config:*base-directory* "lib"))
(defvar garnet-binary-pathname (get-garnet-binary-pathname))

;; (defvar Garnet-Opal-Src
;;   (append-directory Garnet-Src-Pathname "opal"))
;; (defvar Garnet-Opal-Pathname
;;   (append-directory Garnet-Binary-Pathname "opal"))
(defvar Garnet-Truetype-Src
  (append-directory Garnet-Src-Pathname "truetype"))
(defvar Garnet-Truetype-Pathname
  (append-directory Garnet-Binary-Pathname "truetype"))
(defvar Garnet-Inter-Src
  (append-directory Garnet-Src-Pathname "inter"))
(defvar Garnet-Inter-Pathname
  (append-directory Garnet-Binary-Pathname "inter"))
(defvar Garnet-Gesture-Src
  (append-directory Garnet-Src-Pathname "gesture"))
(defvar Garnet-Gesture-Pathname
  (append-directory Garnet-Binary-Pathname "gesture"))
(defvar Garnet-Aggregadgets-Src
  (append-directory Garnet-Src-Pathname "aggregadgets"))
(defvar Garnet-Aggregadgets-Pathname
  (append-directory Garnet-Binary-Pathname "aggregadgets"))
(defvar Garnet-PS-Src
  (append-directory Garnet-Src-Pathname "ps"))
;; (defvar Garnet-PS-Pathname
;;   (append-directory Garnet-Binary-Pathname "ps"))
(defvar Garnet-Gadgets-Src
  (append-directory Garnet-Src-Pathname "gadgets"))
(defvar Garnet-Gadgets-Pathname
  (append-directory Garnet-Binary-Pathname "gadgets"))
(defvar Garnet-Debug-Src
  (append-directory Garnet-Src-Pathname "debug"))
(defvar Garnet-Debug-Pathname
  (append-directory Garnet-Binary-Pathname "debug"))
(defvar Garnet-Demos-Src
  (append-directory Garnet-Src-Pathname "demos"))
(defvar Garnet-Demos-Pathname
  (append-directory Garnet-Binary-Pathname "demos"))
(defvar Garnet-Gilt-Src
  (append-directory Garnet-Src-Pathname "gilt"))
(defvar Garnet-Gilt-Pathname
  (append-directory Garnet-Binary-Pathname "gilt"))
(defvar Garnet-C32-Src
  (append-directory Garnet-Src-Pathname "c32"))
(defvar Garnet-C32-Pathname
  (append-directory Garnet-Binary-Pathname "c32"))
(defvar Garnet-Lapidary-Src
  (append-directory Garnet-Src-Pathname "lapidary"))
(defvar Garnet-Lapidary-Pathname
  (append-directory Garnet-Binary-Pathname "lapidary"))
(defvar Garnet-Contrib-Src
  (append-directory Garnet-Src-Pathname "contrib"))
(defvar Garnet-Contrib-Pathname
  (append-directory Garnet-Binary-Pathname "contrib"))
(defvar Garnet-Protected-Eval-Src
  (append-directory Garnet-Src-Pathname "protected-eval"))
(defvar Garnet-Protected-Eval-Pathname
  (append-directory Garnet-Binary-Pathname "protected-eval"))

(defvar Garnet-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "bitmaps"))
(defvar Garnet-Pixmap-Pathname
  (append-directory Garnet-Lib-Pathname "pixmaps"))
(defvar Garnet-Gilt-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "gilt"))
(defvar Garnet-C32-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "c32"))
(defvar Garnet-DataFile-Pathname
  (append-directory Garnet-Lib-Pathname "data"))
(defvar Garnet-Gesture-Data-Pathname
  (append-directory Garnet-Lib-Pathname "gesture"))

;;; Names of loader files.

(defparameter Garnet-Inter-Loader (merge-pathnames "inter-loader" Garnet-Inter-PathName))

(defparameter Garnet-Gesture-Loader (merge-pathnames "gesture-loader" Garnet-Gesture-PathName))

(defparameter Garnet-Aggregadgets-Loader (merge-pathnames "aggregadgets-loader" Garnet-Aggregadgets-PathName))
(defparameter Garnet-Aggregraphs-Loader (merge-pathnames "aggregraphs-loader" Garnet-Aggregadgets-PathName))
(defparameter Garnet-Gadgets-Loader (merge-pathnames "gadgets-loader" Garnet-Gadgets-PathName))
(defparameter Garnet-Debug-Loader (merge-pathnames "debug-loader" Garnet-Debug-PathName))
(defparameter Garnet-Demos-Loader (merge-pathnames "demos-loader" Garnet-Demos-PathName))
(defparameter Garnet-Gilt-Loader (merge-pathnames "gilt-loader" Garnet-Gilt-PathName))
(defparameter Garnet-C32-Loader (merge-pathnames "c32-loader" Garnet-C32-PathName))
(defparameter Garnet-Lapidary-Loader (merge-pathnames "lapidary-loader" Garnet-Lapidary-PathName))
(defparameter garnet-protected-eval-Loader (merge-pathnames "protected-eval-loader" Garnet-Protected-Eval-PathName))

;; Packages to load and the locations of those packages.
(defparameter garnet-load-alist
;;; Target directories (binarys)
  `(("gg"                 . ,Garnet-Gadgets-PathName)
    ("gadgets"            . ,Garnet-Gadgets-PathName)
;;    ("opal"               . ,Garnet-Opal-Pathname)
    ("truetype"           . ,Garnet-Truetype-PathName)
    ("inter"              . ,Garnet-Inter-PathName)
    ("gesture"            . ,Garnet-Gesture-PathName)
    ("gestures"           . ,Garnet-Gesture-PathName)
;;    ("ps"                 . ,Garnet-PS-PathName)
    ("aggregadgets"       . ,Garnet-Aggregadgets-PathName)
    ("debug"              . ,Garnet-Debug-PathName)
    ("demos"              . ,Garnet-Demos-PathName)
    ("demo"               . ,Garnet-Demos-PathName)
    ("gilt"               . ,Garnet-Gilt-PathName)
    ("c32"                . ,Garnet-C32-PathName)
    ("lapidary"           . ,Garnet-Lapidary-PathName)
    ("contrib"            . ,Garnet-Contrib-PathName)
    ("protected-eval"     . ,Garnet-Protected-Eval-PathName)
;;; Source directories.
;;    ("opal-src"           . ,Garnet-Opal-Src)
    ("inter-src"          . ,Garnet-Inter-Src)
    ("gesture-src"        . ,Garnet-Gesture-Src)
    ("gestures-src"       . ,Garnet-Gesture-Src)
    ("ps-src"             . ,Garnet-PS-Src)
    ("aggregadgets-src"   . ,Garnet-Aggregadgets-Src)
    ("gadgets-src"        . ,Garnet-Gadgets-Src)
    ("gg-src"             . ,Garnet-Gadgets-Src)
    ("debug-src"          . ,Garnet-Debug-Src)
    ("demos-src"          . ,Garnet-Demos-Src)
    ("demo-src"           . ,Garnet-Demos-Src)
    ("gilt-src"           . ,Garnet-Gilt-Src)
    ("c32-src"            . ,Garnet-C32-Src)
    ("lapidary-src"       . ,Garnet-Lapidary-Src)
    ("contrib-src"        . ,Garnet-Contrib-Src)
    ("protected-eval-src" . ,Garnet-Protected-eval-Src)))

;;; The actual loader code.
(defun Add-Garnet-Load-Prefix (prefix pathname)
  (push (cons prefix pathname) Garnet-Load-Alist))

(defun Garnet-Load (filename)
  "Load a file. If the file is prefixed with a Garnet module name, get
the file from the proper directory in the Garnet source tree.
Otherwise just load the filename as given."
  (let ((pos (position #\: filename)))
    (if pos
        (let* ((module (subseq filename 0 pos))
               (name (subseq filename (1+ pos)))
               (module-src-directory
                (or (cdr (assoc module Garnet-Load-Alist :test #'string=))
                    (error "Module ~S is not a Garnet module" module)))
               (src-pathname (make-pathname :name name
                                            ;; For Windows.
                                            :device (pathname-device module-src-directory)
                                            :directory (pathname-directory
                                                        module-src-directory))))
          (force-output *error-output*)
          (format T "~&Loading ~s~%" src-pathname)
          (force-output)
          (load src-pathname))
        ;; else no module name found; load regular.
        (progn
          (format T "No module name given: Loading ~s~%" filename)
          (load filename)))))


;;; Garnet-Compile.
;; This function will compile your garnet files while keeping the
;; sources and binaries separated.  If you want to just compile one
;; file from Garnet, like the gadget file gauge.lisp, then you could
;; use this function to compile the source file and automatically
;; save the binary file in the proper directory in the binary tree.
;;
;; Example:
;;    (garnet-compile "gadgets:gauge")
;;    Takes the source file from Garnet-Gadgets-Src, compiles it, and
;;    saves the binary file in Garnet-Gadgets-Pathname (the binary
;;    gadgets directory).
;;
(defvar *compiler-extension*
  (pathname-type (compile-file-pathname "foo.lisp")))

;;; RGA  This will lose on Windows XXX
(defun garnet-mkdir-if-needed (dirname)
  "Creates the directory if it does not exist."
  (ensure-directories-exist dirname :verbose t))

(defun garnet-compile (filename)
  "Compile a single Garnet file, finding the source in the Garnet
   source tree and installing the result in the corresponding
   directory in the binary tree.
   Example:
   (garnet-compile \"gadgets:gauge\") akes the source file from
   Garnet-Gadgets-Src, compiles it, and aves the binary file in
   Garnet-Gadgets-Pathname (the binary adgets directory)."
  (let* ((pos (position #\: filename))
         (module (if pos
                     (subseq filename 0 pos)
                     ;; else no colon, abort
                     (error
                      "The filename ~A is not prefixed by a garnet module name. Aborting compile"
                      filename)))
         ;; We want to extract just the name part, without the .lisp if present.
         (filepath (subseq filename (1+ pos)))
         (name (pathname-name filepath))
         (type (pathname-type name))
         (module-src (concatenate 'string module "-src"))
         (module-src-directory
          (or (cdr (assoc module-src Garnet-Load-Alist
                          :test #'string=))
              (cdr (assoc module Garnet-Load-Alist
                          :test #'string=))
              (error "Module named ~S not found in Garnet-Load-Alist"
                     module)))
         (module-bin-directory
          (or (cdr (assoc module Garnet-Load-Alist
                          :test #'string=))
              (error "Module named ~S not found in Garnet-Load-Alist"
                     module)))
         (src-pathname (make-pathname :name name
                                      ;; If no user supplied type, add default.
                                      :type (or type "lisp")
                                      :device (pathname-device module-src-directory)
                                      :directory (pathname-directory module-src-directory)))
         (bin-pathname (progn
			  (format t "Never make it here (hopefully).")
			  (make-pathname :name name
					 :type *compiler-extension*
					 :device (pathname-device module-bin-directory)
					 :directory (pathname-directory module-bin-directory)))))
	 (force-output *error-output*)
	 (format T "~&Compiling ~s~%" src-pathname)
	 (format T "for output to ~s~%" bin-pathname)
	 (force-output)
	 ;; sds: make sure that bin/foo directory is already there
	 (garnet-mkdir-if-needed bin-pathname)
	 (let ((*compile-verbose* Garnet-Garnet-Debug)
	       (*compile-print* Garnet-Garnet-Debug))
	   (compile-file src-pathname))))
