;;;-*- Mode: COMMON-LISP; Package: COMMON-LISP-USER -*-

(cl:in-package "COMMON-LISP-USER")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :GARNET *features*)
    (cerror "continue" "Garnet must be loaded before loading or compiling Multi-Garnet"))
  #-(and)
  (unless (or (equal "2.2" user::garnet-version-number)
	      )
    (format t "~&Warning: Garnet version ~S is loaded~%" user::garnet-version-number)
    (format t "~&    Multi-Garnet v2.2 has been tested in Garnet v2.2~%")
    (format t "~&    Multi-Garnet will probably not work with this version of Garnet.~%")
    (cerror "compile Multi-Garnet v2.2"
            "Incompatible version of Garnet loaded: continue at your own risk")
    )
  )

(defparameter *multi-garnet-path* (merge-pathnames "multi-garnet/" your-garnet-pathname))

(compile-file (merge-pathnames "package" *multi-garnet-path*))
(load (merge-pathnames "package" *multi-garnet-path*))

(in-package "MULTI-GARNET")
;; make sure that multi-garnet is disabled when compiling or loading it
(eval-when (:load-toplevel :execute)
  (when (fboundp 'disable-multi-garnet)
    (disable-multi-garnet))
  )

(compile-file (merge-pathnames "sky-blue" common-lisp-user::*multi-garnet-path*))
(load (merge-pathnames "sky-blue" common-lisp-user::*multi-garnet-path*))
(compile-file (merge-pathnames "multi-garnet" common-lisp-user::*multi-garnet-path*))
(load (merge-pathnames "multi-garnet" common-lisp-user::*multi-garnet-path*))

;; enable multi-garnet after all files are loaded
(eval-when (:load-toplevel :execute) (enable-multi-garnet))

