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


(defpackage #:clx-truetype
  (:nicknames #:xft)
  (:use #:cl #:kr)
  (:export
   :font
   :font-family
   :font-subfamily
   :font-size
   :font-underline
   :font-strikethrough
   :font-overline
   :font-background
   :font-foreground
   :font-overwrite-gcontext
   :cache-font-file
   :*font-dirs*
   :drawable-screen
   :font-ascent
   :font-descent
   :text-bounding-box
   :xmin
   :ymin
   :xmax
   :ymax
   :screen-default-dpi
   :screen-dpi
   :draw-text
   :draw-text-line
   :get-font-families
   :get-font-subfamilies
   :text-height
   :text-width
   :text-line-bounding-box
   :text-line-width
   :text-line-height
   :font-line-gap
   :baseline-to-baseline
   :font-antialias
   :font-lines-height
   :cache-fonts
   :font-equal)
  (:documentation "Package contains API for TrueType text rendering using CLX, XRender. 
Glyphs information is obtained by ZPB-TTF. Font rasterization is made by CL-VECTORS."))
