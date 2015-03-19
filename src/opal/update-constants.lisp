;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Opal:Update-Constants
;;
;;  This contains constants used to reference the Update-Slots-Values arrays.
;;  If you make any changes to an :update-slots slot, you must also make the
;;  corresponding changes in this file, lest havoc result...
;;
;;  You do not have to supply constants for :visible, :line-style,
;;  :filling-style, or :draw-function.  


;;; Changes:
;;  06-Oct-93 amickish  Removed x-substr and text-extents variables
;;  28-Jul-93 amickish  Merged cursor-multi-text constants with multi-text
;;  30-Jun-93 amickish  New constants for text
;;  23-Mar-90 ecp  New slot :fill-background-p for text objects.


(in-package "OPAL")


;;; LINE
(declaim (fixnum +line-x1+ +line-x2+ +line-y1+
		 +line-y2+ +line-lstyle+ +line-fstyle+
		 +line-draw-function+))
(defconstant +line-x1+		  2)
(defconstant +line-x2+		  3)
(defconstant +line-y1+		  4)
(defconstant +line-y2+		  5)
(defconstant +line-lstyle+        6)
(defconstant +line-fstyle+	  7)
(defconstant +line-draw-function+ 8)


;;; RECTANGLE
(declaim (fixnum +rect-top+ +rect-left+ +rect-width+
		 +rect-height+ +rect-lstyle+ +rect-fstyle+
		 +rect-draw-function+))
(defconstant +rect-top+           2)
(defconstant +rect-left+          3)
(defconstant +rect-width+         4)
(defconstant +rect-height+        5)
(defconstant +rect-lstyle+	  6)
(defconstant +rect-fstyle+	  7)
(defconstant +rect-draw-function+ 8)


;;; ROUNDTANGLE
(declaim (fixnum +roundt-top+ +roundt-left+ +roundt-width+
		 +roundt-height+ +roundt-radius+ +roundt-draw-radius+
		 +roundt-lstyle+ +roundt-fstyle+ +roundt-draw-function+))
(defconstant +roundt-top+           2)
(defconstant +roundt-left+          3)
(defconstant +roundt-width+         4)
(defconstant +roundt-height+        5)
(defconstant +roundt-radius+        6)
(defconstant +roundt-draw-radius+   7)
(defconstant +roundt-lstyle+	    8)
(defconstant +roundt-fstyle+	    9)
(defconstant +roundt-draw-function+ 10)


;;; MULTIPOINT
(declaim (fixnum +multi-point-list+ +multi-lstyle+ 
		 +multi-fstyle+ +multi-draw-function+))
(defconstant +multi-point-list+      2)
(defconstant +multi-lstyle+	     3)
(defconstant +multi-fstyle+	     4)
(defconstant +multi-draw-function+   5)


;;; POLYLINE
(declaim (fixnum +polyline-point-list+ +polyline-lstyle+
		 +polyline-fstyle+ +polyline-draw-function+))
(defconstant +polyline-point-list+    2)
(defconstant +polyline-lstyle+	      3)
(defconstant +polyline-fstyle+	      4)
(defconstant +polyline-draw-function+ 5)


;;; TEXT
(declaim (fixnum +text-top+ +text-left+ +text-width+
		 +text-height+ +text-string+ +text-xfont+
		 +text-actual-heightp+ +text-fill-background-p+ +text-lstyle+
		 +text-draw-function+ +text-cursor-offset+ +text-justification+
		 +text-cut-strings+ +text-line-number+))
(defconstant +text-top+                    2)
(defconstant +text-left+                   3)
(defconstant +text-width+                  4)
(defconstant +text-height+                 5)
(defconstant +text-string+                 6)
(defconstant +text-xfont+                  7)
(defconstant +text-actual-heightp+         8)
(defconstant +text-fill-background-p+      9)
(defconstant +text-lstyle+                10)
(defconstant +text-draw-function+         11)
(defconstant +text-cursor-offset+         12)
(defconstant +text-justification+	  13)
(defconstant +text-cut-strings+		  14)
(defconstant +text-line-number+		  15)


;;; MULTIFONT TEXT
(declaim (fixnum +mf-text-lstyle+ +mf-text-draw-function+
		 +mf-text-fill-background-p+ +mf-text-force-update+))
(defconstant +mf-text-lstyle+ 6)
(defconstant +mf-text-draw-function+ 7)
(defconstant +mf-text-fill-background-p+ 8)
(defconstant +mf-text-force-update+ 9)


;;; K-FRAMED-TEXT
(declaim (fixnum +kft-string+ +kft-font+ +kft-xfont+
		 +kft-text-extents+ +kft-text-width+ +kft-text-height+))
(defconstant +kft-string+        9)
(defconstant +kft-font+         10)
(defconstant +kft-xfont+        11)
(defconstant +kft-text-extents+ 12)
(defconstant +kft-text-width+   13)
(defconstant +kft-text-height+  14)


;;; BITMAPS
(declaim (fixnum +bm-image+ +bm-top+ +bm-left+
		 +bm-lstyle+ +bm-fstyle+ +bm-draw-function+))
(defconstant +bm-image+		2)
(defconstant +bm-top+		3)
(defconstant +bm-left+		4)
(defconstant +bm-lstyle+	5) 
(defconstant +bm-fstyle+	6) 
(defconstant +bm-draw-function+ 7)


;;; ARC
(declaim (fixnum +arc-left+ +arc-top+ +arc-width+
		 +arc-height+ +arc-angle1+ +arc-angle2+
		 +arc-lstyle+ +arc-fstyle+ +arc-draw-function+))
(defconstant +arc-left+		 2)
(defconstant +arc-top+		 3)
(defconstant +arc-width+	 4)
(defconstant +arc-height+	 5)
(defconstant +arc-angle1+	 6)
(defconstant +arc-angle2+	 7)
(defconstant +arc-lstyle+	 8)
(defconstant +arc-fstyle+	 9)
(defconstant +arc-draw-function+ 10)


;;; CIRCLE
(declaim (fixnum +circle-left+ +circle-top+ +circle-width+
		 +circle-height+ +circle-angle1+ +circle-angle2+
		 +circle-lstyle+ +circle-fstyle+ +circle-draw-function+))
(defconstant +circle-left+		2)
(defconstant +circle-top+		3)
(defconstant +circle-width+		4)
(defconstant +circle-height+		5)
(defconstant +circle-angle1+		6)
(defconstant +circle-angle2+		7)
(defconstant +circle-lstyle+		8)
(defconstant +circle-fstyle+		9)
(defconstant +circle-draw-function+	10)

