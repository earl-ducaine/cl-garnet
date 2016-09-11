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


(in-package #:clx-truetype)

(defun drawable-screen (drawable)
  (typecase drawable
    (xlib:drawable
     (dolist (screen (xlib:display-roots (xlib:drawable-display drawable)))
       (when (xlib:drawable-equal (xlib:screen-root screen) (xlib:drawable-root drawable))
         (return screen))))
    (xlib:screen drawable)
    (t nil)))
