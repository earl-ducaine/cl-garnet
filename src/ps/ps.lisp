;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$
;;;


;;; Make-PS-File
;;;
;;; The function Make-PS-File generates postscript files from Garnet
;;; windows. The resulting files may be sent directly to a postscript
;;; printer or included in larger Scribe and LaTex documents.
;;;
;;; Designed and implemented by Andrew Mickish
;;;


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Make-PS-File)))


;;;  POSTSCRIPT NOTES
;;;    When passing parameters, a value of -1 corresponds to NIL in Lisp.
;;;

; Clip future drawing to this rectangular area
;
(defparameter *clip-fn*
  "
/ClipDict 2 dict def
/ClipToRectangle { % left top width height => -
    ClipDict begin
	/height exch def  /width exch def
        newpath
	    moveto			% Go to the left,top corner
	    width 0 rlineto		% top side
	    0 height neg rlineto	% right side
	    width neg 0 rlineto		% bottom side
	    closepath			% left side
	gsave 1 setgray fill grestore
	clip newpath
    end
} def")

(defparameter *fillshape-fn*
  "
/FillShape { % [halftone] | [r g b] | null => -
    gsave
        dup null eq { pop } {
	  aload length 3 eq { setrgbcolor fill } {
	      FillPattern
	  } ifelse
        } ifelse
    grestore
    newpath
} def")


(defparameter *strokeshape-fn*
  "
/StrokeShape { % line-color cap join dash thickness => -
    % If no line is desired, pass -1 for line-thickness
    dup 0 ge 5 index null ne and {
	% if line-thickness >= 0, then draw the outline
	gsave
	    setlinewidth 0 setdash setlinejoin setlinecap
	    aload length 3 eq { setrgbcolor stroke } {
		strokepath FillPattern
	    } ifelse
	grestore
    } {
	pop pop pop pop pop
    } ifelse
    newpath
} def")

(defparameter *line-fn*
  "
/AdjustLineDict 20 dict def
/AdjustLine { % x1 y1 x2 y2 width => x1 y1 x2 y2
  AdjustLineDict begin
    /width exch cvi def
    /hw width 2 div def
    /oddwidth? width 1 and 1 eq def
    /y2 exch def /x2 exch def /y1 exch def /x1 exch def
    /dx x2 x1 sub def /dy y2 y1 sub def
    dy 0 eq {
      dx 0 eq {
	% point
	oddwidth? {
	  /x1 x1 .5 add def /y1 y1 .5 sub def
	  /x2 x2 .5 add def /y2 y2 .5 sub def
	} if
      } {
	% horizontal
	oddwidth? {
	  /y1 y1 .5 sub def
	  /y2 y2 .5 sub def
	} if
      } ifelse
    } {
      dx 0 eq {
	% vertical
	oddwidth? {
	  /x1 x1 .5 add def
	  /x2 x2 .5 add def
	} if
      } {
	% diagonal
	oddwidth? {
	  /x1 x1 .5 add def /y1 y1 .5 sub def
	  /x2 x2 .5 add def /y2 y2 .5 sub def
	} if
      } ifelse
    } ifelse
    x1 y1 x2 y2
  end
} def

/DrawLineDict 20 dict def
/DrawLine { % line-color cap join dash thickness x2 y2 x1 y1 => -
    gsave DrawLineDict begin
	4 index AdjustLine
	moveto lineto
	StrokeShape
    end grestore
} def")

(defparameter *roundtangle-fn*
  "
/RoundtanglePath { % left top width height radius => -
    /r exch def  /h exch def  /w exch def  /t exch def  /l exch def
    /right l w add def  /bottom t h sub def
    l r add t moveto                         % origin
    right t right t r sub r arcto            % top side
    right bottom right r sub bottom r arcto  % right side
    l bottom l bottom r add r arcto          % bottom side
    l t l r add t r arcto		     % left side
    16 {pop} repeat  % each arcto accumulates 4 stack parameters
} def

/RoundtangleDict 27 dict def
/DrawRoundtangle { % left top width height radius line-color
		   % cap join dash thickness fill-color => -
    gsave RoundtangleDict begin
	/fill-color exch def  /thickness exch def
	/dash-pattern exch def  /line-join exch def  /line-cap exch def
	/line-color exch def  /radius exch def
	/height exch def  /width exch def  /top exch def  /left exch def
        
	% Draw filling
	newpath
	left thickness add top thickness sub
	width thickness 2 mul sub height thickness 2 mul sub
        radius thickness sub dup 0 lt { pop 0 } if RoundtanglePath
	fill-color FillShape

	left thickness 2 div add top thickness 2 div sub
	width thickness sub height thickness sub
        radius thickness 2 div sub dup 0 lt { pop 0 } if RoundtanglePath

	% Draw border
	line-color line-cap line-join dash-pattern thickness
	StrokeShape
    end grestore
} def
")

(defparameter *rectangle-fn*
  "
/RectanglePath { % left top width height => -
    /h exch def  /w exch def
    moveto			% Go to the left,top corner
    w 0 rlineto			% top side
    0 h neg rlineto		% right side
    w neg 0 rlineto		% bottom side
    closepath			% left side
} def

/RectangleDict 21 dict def
/DrawRectangle { % left top width height line-color
		 % cap join dash thickness fill-color => -
    RectangleDict begin
	/fill-color exch def  /thickness exch def /dash-pattern exch def
	/line-join exch def  /line-cap exch def /line-color exch def
	/height exch def /width exch def  /top exch def  /left exch def
        /thickness-for-fill thickness 0 lt {0} {thickness} ifelse def
	% Draw filling
	newpath
	left thickness-for-fill add top thickness-for-fill sub
        width thickness-for-fill 2 mul sub height thickness-for-fill 2 mul sub
	RectanglePath
	fill-color FillShape
	% Draw border
	newpath
	left thickness 2 div add  top thickness 2 div sub
	width thickness sub  height thickness sub
	RectanglePath
	line-color line-cap line-join dash-pattern thickness StrokeShape
    end
} def")


; This function for drawing ellipses (circles, arcs, and ovals) is based
; on the example "Elliptical Arcs" in the blue postscript Tutorial and
; Cookbook.
;
(defparameter *ellipse-fn*
  "
/EllipseDict 23 dict def
EllipseDict /mtrx matrix put
/DrawEllipse { % x y xrad yrad startangle endangle line-color
	       % cap join dash thickness fill-color => -
    EllipseDict begin
	/fill-color exch def /thickness exch def
	/dash-pattern exch def  /line-join exch def  /line-cap exch def
	/line-color exch def  /endangle exch def /startangle exch def
	/yrad exch def  /xrad exch def /y exch def /x exch def
	/savematrix mtrx currentmatrix def
	% Draw the filling
	gsave
	    newpath
	    x y translate
	    xrad thickness sub yrad thickness sub scale
	    0 0 1 startangle endangle arc
	    savematrix setmatrix
	    fill-color FillShape
	    newpath
	    x y translate
	    xrad thickness 2 div sub yrad thickness 2 div sub scale
	    0 0 1 startangle endangle arc
	    savematrix setmatrix
	    line-color
	    0 % line-cap
	    line-join dash-pattern thickness
	    StrokeShape
	grestore
    end
} def")

(defparameter *arc-fn*
  "
/ArcDict 23 dict def
ArcDict /mtrx matrix put
/DrawArc { % x y xrad yrad startangle endangle line-color
	   % cap join dash thickness fill-color => -
    ArcDict begin
	/fill-color exch def /thickness exch def
	/dash-pattern exch def  /line-join exch def  /line-cap exch def
	/line-color exch def  /endangle exch def /startangle exch def
	/yrad exch def  /xrad exch def /y exch def /x exch def
	/savematrix mtrx currentmatrix def
	% Draw the filling
	gsave
	    newpath
	    x y translate
	    xrad thickness sub yrad thickness sub scale
	    0 0 moveto
	    0 0 1 startangle endangle arc closepath
	    savematrix setmatrix
	    fill-color FillShape
	    newpath
	    x y translate
	    xrad thickness 2 div sub yrad thickness 2 div sub scale
	    0 0 1 startangle endangle arc
	    savematrix setmatrix
	    line-color
	    0 % line-cap
	    line-join dash-pattern thickness
	    StrokeShape
	grestore
    end
} def")

(defparameter *polyline-fn*
  "
/PolylineDict 20 dict def
/DrawPolyline { % x1 y1 {{x y ...} ...} line-color
		% join cap dash thickness fill-color => -
    gsave PolylineDict begin
	/fill-color exch def  /thickness exch def  /dash-pattern exch def
	/line-join exch def  /line-cap exch def  /line-color exch def
	% Don't draw the path of the fill if the filling-style is null.
	fill-color null eq not {
		3 copy
		newpath
		3 1 roll moveto
		{ aload length 2 idiv { lineto } repeat } forall
		fill-color FillShape
		} if

	newpath
	.5 -.5 translate

	% Stroke after every sub-array to avoid a limitcheck error
	2 index 2 index moveto
	{ aload length 2 sub 2 idiv { lineto } repeat
	2 copy lineto
	line-color line-cap line-join dash-pattern thickness StrokeShape
	moveto
	} forall

	currentpoint
	3 -1 roll sub abs .01 lt
	3 1 roll sub abs .01 lt and {
	  0 0 rlineto closepath
	} if

	line-color line-cap line-join dash-pattern
	thickness
%	dup -1 ne { .5 add } if % fudge outline width thicker
	StrokeShape

    end grestore
} def")



(defparameter *text-fn*
  "
/TextDict 40 dict def
/DrawText { % left top base-y opal-width height fill-p
	    % fore-color back-color string size font-name => -
    TextDict begin
	gsave
	    findfont exch scalefont setfont
	    /s exch def
	    /back-color exch def /fore-color exch def /fill-p exch def
	    /height exch def  /opal-width exch def
	    /base-y exch def  /top exch def
	    /left exch 1 sub def % XXX: I don't know why!

	    % Calculate distance to add between each character, based on the 
	    % width expected by Opal, the width expected by postscript, and 
	    % the number of characters to distribute the change over.
	    /x-dist opal-width s stringwidth pop sub s length div def

	    % Draw background of text if appropriate
	    fill-p {
		gsave
		    newpath
		    left top opal-width height RectanglePath
		    back-color FillShape
		grestore
	    } if

	    % Draw text in the appropriate color
	    newpath
	    s length 0 ne {
		left base-y moveto
		fore-color aload length 3 eq {
		    setrgbcolor
		    x-dist 0 s ashow
		} { % Halftone pattern:
		    %   fgR fgG fgB bgR bgG bgB top pattern
		    %   fgR fgG fgB false top pattern
		    /pattern exch def /top exch def /opaque? exch def
		    opaque? not { .5 .5 .5 } if
		    /bgB exch def /bgG exch def /bgR exch def
		    /fgB exch def /fgG exch def /fgR exch def

		    fgR bgR add 2 div
		    fgG bgG add 2 div
		    fgB bgB add 2 div
		    setrgbcolor

		    opaque? {
			x-dist 0 s ashow
		    } {
			mark
			/ch 1 string def
			/space { /space { x-dist 0 rmoveto } def } def
%			/jt statusdict begin jobtimeout end def
			gsave { % try really hard to do it right
			    s {
				% don't take more than 2 seconds a character
%				statusdict begin 2 setjobtimeout end
				space
				ch 0 3 -1 roll put
				ch true charpath
				currentpoint /yy exch def /xx exch def
				fgR fgG fgB false top pattern FillPattern
				newpath xx yy moveto
			    } forall
			} stopped grestore { % fall back in case we fail
			    x-dist 0 s ashow
			} if
%			statusdict begin jt setjobtimeout end
			cleartomark
		    } ifelse
		} ifelse
	    } if
	grestore
    end
} def")


(defparameter *bitmap-fn*
  "
/BitmapDict 20 dict def
/DrawBitmap { % left top width height pattern transparent-p color => -
    BitmapDict begin
	/color exch def /transparent-p exch def
	/pattern exch def  /height exch def  /width exch def
	gsave
	    translate
	    width height scale
	    color length 3 eq {
	        color aload pop setrgbcolor
	    } {
	        .5 setgray % fudge pattern fills
	    } ifelse
	    transparent-p {
		% The case where the background is not drawn
		width height false
		[ width 0 0 height 0 height ]
		{pattern} imagemask
	    } {
		% The case where the background is drawn
%               {0 eq {currentgray} {1} ifelse} settransfer
		width height 1
		[ width 0 0 height 0 height ]
		{pattern} image
	    } ifelse
	grestore
    end
} def")


(defparameter *def-image-fn*
  "
/DefImage { % image-name font-name w h string => -
  20 dict begin
    /ImageString exch def /ImageHeight exch def /ImageWidth exch def
    /FontName exch def
    /ImageName exch def
    /FontType 3 def
    /FontMatrix [1 0 0 1 0 0] def
    /FontBBox [0 0 1 1] def
    /Encoding [
      /image 255 { /.notdef } repeat
    ] def
    /BuildChar { % font char => -
      pop begin
        1 0
	0 0 1 1 setcachedevice
	gsave
	  ImageWidth ImageHeight false
	  [ ImageWidth 0 0 ImageHeight 0 0 ]
          {ImageString} imagemask
	grestore
      end
    } def
    ImageName currentdict definefont
    FontName exch
    ImageName ImageString
  end
  def def
} def
")


(defparameter *pixmap-fn*
  "
/PixmapDict 20 dict def
/DrawPixmap { % left top width height pixwidth pixheight pixstr=> -
    PixmapDict begin
        /pixstr exch def
	/pixheight exch def /pixwidth exch def
	/height exch def  /width exch def
	gsave
	    translate
	    width height scale
	    pixwidth pixheight 8 [pixwidth 0 0 pixheight neg 0 pixheight]
	    pixstr
	    false 3 colorimage
	grestore
    end
} def")




(defparameter *arbitrary-pattern-fn*
  "
/FillPattern { % fgR fgG fgB [bgR bgG bgB] opaque? top font => -
  50 dict begin gsave
    /font exch def
    /top exch def
    /opaque? exch def
    /w font /ImageWidth get def
    /h font /ImageHeight get def
    font [w 0 0 h 0 0] makefont setfont
    pathbbox
    /y1 exch def /x1 exch def
    /y0 exch def /x0 exch def
    clip
    opaque? {
      gsave
	setrgbcolor
	fill
      grestore
    } if
    newpath
    setrgbcolor
    /y0 y0 top sub h div floor h mul top add cvi def
    /x0 x0 w div floor w mul cvi def
    x0 y0 translate
    /W x1 x0 sub w div ceiling cvi 1 add def
    /H y1 y0 sub h div ceiling cvi def
    /s W string def
    H {
      0 0 moveto
      s show
      0 h translate
    } repeat
  grestore end
  newpath
} def
")

(defparameter *motif-window-frame-fn*
  "
/DrawMotifWindowFrame { % label left top w h 
			% leftmargin topmargin rightmargin bottommargin => -
  30 dict begin

  { % setup
    /bottommargin exch def /rightmargin exch def
    /topmargin exch def /leftmargin exch def
    /h exch def /w exch def /y exch h sub def /x exch def
    /label exch def

    /bottom y bottommargin sub def
    /top y h add topmargin add def
    /left x leftmargin sub def
    /right x w add rightmargin add def

    /light .9 def
    /medium .7 def
    /dark .4 def

    1 setlinewidth
    0 setlinecap
  } exec

  { % background
    left bottom moveto
    left top lineto
    right top lineto
    right bottom lineto
    closepath
    medium setgray fill
  } exec

  { % outer upper left edges
    left bottom moveto
    right bottom lineto
    right top lineto
    right 2 sub top 2 sub lineto
    right 2 sub bottom 2 add lineto
    left 2 add bottom 2 add lineto
    closepath
    dark setgray fill
  } exec

  { % outer lower right edges
    left bottom moveto
    left top lineto
    right top lineto
    right 2 sub top 2 sub lineto
    left 2 add top 2 sub lineto
    left 2 add bottom 2 add lineto
    closepath
    light setgray fill
  } exec

  { % inner lower right edges
    left 9 add bottom 9 add moveto
    right 9 sub bottom 9 add lineto
    right 9 sub top 25 sub lineto
    right 11 sub top 27 sub lineto
    right 11 sub bottom 11 add lineto
    left 11 add bottom 11 add lineto
    closepath
    light setgray fill
  } exec

  { % inner upper left edges
    left 9 add bottom 9 add moveto
    left 9 add top 25 sub lineto
    right 9 sub top 25 sub lineto
    right 11 sub top 27 sub lineto
    left 11 add top 27 sub lineto
    left 11 add bottom 11 add lineto
    closepath
    dark setgray fill
  } exec

  { % top rut above label
    left 9 add top 9.5 sub moveto
    right 9.01 sub top 9.5 sub lineto
    dark setgray stroke
    left 10 add top 10.5 sub moveto
    right 9.01 sub top 10.5 sub lineto
    light setgray stroke
  } exec

  { % left edge of left button
    left 9.5 add top 9 sub moveto
    left 9.5 add top 25.99 sub lineto
    dark setgray stroke
    left 10.5 add top 10 sub moveto
    left 10.5 add top 25.99 sub lineto
    light setgray stroke
  } exec

  { % right edge of big right button
    right 10.5 sub top 11 sub moveto
    right 10.5 sub top 25.99 sub lineto
    dark setgray stroke
    right 9.5 sub top 11 sub moveto
    right 9.5 sub top 25.99 sub lineto
    light setgray stroke
  } exec

  { % right edge of left button
    left 26.5 add top 11 sub moveto
    left 26.5 add top 25.99 sub lineto
    dark setgray stroke
    left 27.5 add top 11 sub moveto
    left 27.5 add top 25.99 sub lineto
    light setgray stroke
  } exec

  { % edge between small and big right buttons
    right 27.5 sub top 11 sub moveto
    right 27.5 sub top 25.99 sub lineto
    dark setgray stroke
    right 26.5 sub top 11 sub moveto
    right 26.5 sub top 25.99 sub lineto
    light setgray stroke
  } exec

  { % left edge of small right button
    right 44.5 sub top 11 sub moveto
    right 44.5 sub top 25.99 sub lineto
    dark setgray stroke
    right 43.5 sub top 11 sub moveto
    right 43.5 sub top 25.99 sub lineto
    light setgray stroke
  } exec

  { % top left resize corner
    left 26.5 add top 1 sub moveto
    left 26.5 add top 9.99 sub lineto
    dark setgray stroke
    left 27.5 add top 1 sub moveto
    left 27.5 add top 9.99 sub lineto
    light setgray stroke

    left 1 add top 26.5 sub moveto
    left 9.99 add top 26.5 sub lineto
    dark setgray stroke
    left 1 add top 27.5 sub moveto
    left 9.99 add top 27.5 sub lineto
    light setgray stroke
  } exec

  { % top right resize corner
    right 27.5 sub top 1 sub moveto
    right 27.5 sub top 9.99 sub lineto
    dark setgray stroke
    right 26.5 sub top 1 sub moveto
    right 26.5 sub top 9.99 sub lineto
    light setgray stroke

    right 1.01 sub top 26.5 sub moveto
    right 9 sub top 26.5 sub lineto
    dark setgray stroke
    right 1.01 sub top 27.5 sub moveto
    right 9 sub top 27.5 sub lineto
    light setgray stroke
  } exec

  { % bottom left resize corner
    left 26.5 add bottom 1.01 add moveto
    left 26.5 add bottom 9 add lineto
    dark setgray stroke
    left 27.5 add bottom 1.01 add moveto
    left 27.5 add bottom 9 add lineto
    light setgray stroke

    left 1 add bottom 27.5 add moveto
    left 9.99 add bottom 27.5 add lineto
    dark setgray stroke
    left 1 add bottom 26.5 add moveto
    left 9.99 add bottom 26.5 add lineto
    light setgray stroke
  } exec

  { % bottom right resize corner
    right 27.5 sub bottom 1.01 add moveto
    right 27.5 sub bottom 9 add lineto
    dark setgray stroke
    right 26.5 sub bottom 1.01 add moveto
    right 26.5 sub bottom 9 add lineto
    light setgray stroke

    right 1.01 sub bottom 27.5 add moveto
    right 9 sub bottom 27.5 add lineto
    dark setgray stroke
    right 1.01 sub bottom 26.5 add moveto
    right 9 sub bottom 26.5 add lineto
    light setgray stroke
  } exec

  { % upper left button
    left 14.5 add top 19.99 sub moveto
    left 14.5 add top 16.5 sub lineto
    left 22.99 add top 16.5 sub lineto
    light setgray stroke
    left 15 add top 19.5 sub moveto
    left 22.5 add top 19.5 sub lineto
    left 22.5 add top 17 sub lineto
    dark setgray stroke
  } exec

  { % upper right big button
    right 14.01 sub top 14.5 sub moveto
    right 22.5 sub top 14.5 sub lineto
    right 22.5 sub top 22.99 sub lineto
    light setgray stroke
    right 14.5 sub top 15 sub moveto
    right 14.5 sub top 22.5 sub lineto
    right 22 sub top 22.5 sub lineto
    dark setgray stroke
  } exec

  { % upper right small button
    right 34.01 sub top 16.5 sub moveto
    right 37.5 sub top 16.5 sub lineto
    right 37.5 sub top 19.99 sub lineto
    light setgray stroke
    right 34.5 sub top 17 sub moveto
    right 34.5 sub top 19.5 sub lineto
    right 37 sub top 19.5 sub lineto
    dark setgray stroke
  } exec

  { % label
    /Helvetica-Bold findfont 10 scalefont setfont
    label stringwidth pop
    left 28 add right 44 sub add 2 div
    1 index 2 div sub
    dup 2 sub top 24 sub
    moveto
    1 index 4 add 0 rlineto
    0 12 rlineto
    exch 4 add neg 0 rlineto
    closepath
    1 setgray fill
    top 21 sub moveto
    0 setgray
    label show
  } exec

  end
} def")


(defparameter *generic-window-frame-fn*
  "
/DrawGenericWindowFrame { % label left top w h
			  % leftmargin topmargin rightmargin bottommargin => -
  30 dict begin

  { % setup
    /bottommargin exch def /rightmargin exch def
    /topmargin exch def /leftmargin exch def
    /h exch def /w exch def /y exch h sub def /x exch def
    /label exch def
    /pointsize
      topmargin 1 sub
      dup 1 lt { pop 1 } if
      dup 12 gt { 12 sub 3 div 12 add } if
    def

    /bottom y def /top y h add def
    /left x def /right  x w add def
    1 setlinewidth
    0 setlinecap
  } exec

  { % background
    newpath
    left leftmargin sub bottom bottommargin sub moveto
    left leftmargin sub top topmargin add lineto
    right rightmargin add top topmargin add lineto
    right rightmargin add bottom bottommargin sub lineto
    closepath
    0 setgray fill

    left bottom moveto
    left top lineto
    right top lineto
    right bottom lineto
    closepath
    1 setgray fill
  } exec

  topmargin 8 ge {
    /Helvetica-Bold findfont pointsize scalefont setfont
    label stringwidth pop
    left right add 2 div
    exch 2 div sub
    top topmargin 2 div add pointsize 2 div sub 2 add moveto
    1 setgray
    label show
  } if

  end
} def")



(defparameter *required-ps-fns* NIL)
(defparameter *font-list* NIL)
(defparameter *image-list* NIL)
(defparameter *image-cnt* 0)
(defparameter *piximage-list* NIL)
(defparameter *piximage-cnt* 0)
(defparameter *file-uses-color-p* NIL)
(defvar *temp-win* NIL)
(defvar *color-p* T)
(defvar *clip-left* 0)
(defvar *clip-right* 0)
(defvar *clip-top* 0)
(defvar *clip-bottom* 0)
(defvar *page-width*)
(defvar *page-height*)
(defvar *print-area-width*)
(defvar *print-area-height*)
(defvar *llx* 0) (defvar *lly* 0)
(defvar *urx* 0) (defvar *ury* 0)
(defvar *1-inch* 72)
(defvar *2-inches* 144)
(defvar *motif-window-border-left* 11)
(defvar *motif-window-border-right* 11)
(defvar *motif-window-border-top* 27)
(defvar *motif-window-border-bottom* 11)
(defvar *window-frame-font* "Helvetica-Bold")
(defvar *thickest-left-border* 0)
(defvar *thickest-top-border* 0)

;;;
;;;  Utiltity Functions
;;;

(defun convert-y (y)
  (- y))

(defun convert-angle (x)
  (round (* 180 (/ x (coerce PI 'short-float)))))

(defun ps-number (x)
  ;; If x is float, format with two decimal places.  Else, format as integer.
  (if (or (integerp x) (= x (round x)))
      (prin1-to-string (round x))
      (format NIL "~,2F" x)))

(defun ps-rotate (angle)
  (format t "~A rotate~%" (ps-number angle)))
(defun scale (scale-x scale-y)
  (format T "~A ~A scale~%" (ps-number scale-x) (ps-number scale-y)))
(defun translate (x y)
  (format T "~A ~A translate~%" (ps-number x) (ps-number y)))
(defun gsave ()
  (format T "gsave~%"))
(defun grestore ()
  (format T "grestore~%~%"))

;;;
;;;  Functions for printing comments and the prolog
;;;

(defun bbox-comment ()
  (format t "%%BoundingBox: ~S ~S ~S ~S~%" *llx* *lly* *urx* *ury*))

(defun header-comments (title creator for comment)
  (format t "%!PS-Adobe-2.0 EPSF-2.0~%")
  (format t "%%Title: ~A~%" title)
  (format t "%%Creator: ~A~%" creator)
  (format t "%%CreationDate: ~A~%" (inter::time-to-string))
  (format t "%%For: ~A~%" for)
  (format t (if *file-uses-color-p*
		"%%This file uses real color~%"
		"%%This file does not use real color~%"))
  (format t "%%DocumentFonts: (atend)~%")
  (bbox-comment)
  (format t "%%Pages: 1~%")
  (when (and comment (not (equal comment "")))
    (unless (listp comment)
      (setf comment (list comment)))
    (dolist (line comment)
      (format t "%%~A~%" line)))
  (format t "%%EndComments~%~%"))

(defun prolog ()
  (dolist (fn *required-ps-fns*)
    (write-string fn) (terpri))
  (terpri)
  (dolist (pair *image-list*)
    (print-image-info (car pair) (cdr pair)))
  (dolist (pair *piximage-list*)
    (print-piximage-info (car pair) (cdr pair)))
  (terpri)
  (format t "%%EndProlog~%")
  (format t "%%Page: 1 1~%~%"))

(defun trailer-comments (debug)
  (terpri)
  (when debug
    (let* ((width (- *urx* *llx*))
	   (height (- *ury* *lly*))
	   (top (+ *lly* height)))
      (format T "~S ~S ~S ~S [1 0 0] 0 0 [] 1 null DrawRectangle~%~%"
	      *llx* top width height)))
  (format T "showpage~%")
  (format T "%%Trailer~%")
  (format T "%%DocumentFonts: ")
  (if *font-list*
      (do ((fonts *font-list* (cdr fonts)))
	  ((null fonts))
	(write-string (car fonts)) (write-char #\ )))
  (terpri)
  (bbox-comment))



;;;
;;; Functions for setting the *required-ps-fns* list
;;;

(defun make-image-name ()
  (concatenate 'string "image-" (prin1-to-string (incf *image-cnt*))))
(defun make-piximage-name ()
  (concatenate 'string "piximage-" (prin1-to-string (incf *piximage-cnt*))))

;   Only those ps functions that are needed by the objects in the window
; will be printed out to the file.  So this function is iterated over every
; component in a top-level aggregate in a first pass to register which
; functions will be needed for this window.
;   If there are arbitrary fill patterns in any of the objects, then define
; them at the top of the file and dereference them when they are needed in
; a function call.
;
(defun Register-Fns-In-Win (win subwindows-p)
  (check-color (g-value win :background-color))
  (when (and (g-value win :aggregate)
	     (g-value win :aggregate :visible))
    (opal:do-all-components (g-value win :aggregate)
      #'(lambda (comp)
	  (when (g-value comp :visible)
	    (kr-send comp :ps-register-fn comp)
	    (when (arbitrary-pattern-p (g-value comp :filling-style))
	      (pushnew *arbitrary-pattern-fn* *required-ps-fns*)
	      (pushnew *def-image-fn* *required-ps-fns*)
	      (let ((image (g-value comp :filling-style :stipple :image)))
		(unless (assoc image *image-list*)
		  (push (cons image (make-image-name)) *image-list*))))
	    (when (arbitrary-pattern-p (g-value comp :line-style))
	      (pushnew *arbitrary-pattern-fn* *required-ps-fns*)
	      (pushnew *def-image-fn* *required-ps-fns*)
	      (let ((image (g-value comp :line-style :stipple :image)))
		(unless (assoc image *image-list*)
		  (push (cons image (make-image-name)) *image-list*))))))))
  (when subwindows-p
    (let ((wins (g-value win :child)))
      (dolist (sub-win wins)
	(Register-Fns-In-Win sub-win subwindows-p)))))

;; Check-FS-Color and Check-LS-Color are used to determine whether the
;; PS file will require a real color when it is printed.  A "real color"
;; is a color where the red, green, and blue values are not all equal
;; (i.e., a non-gray color).  A comment is printed at the top of the file
;; announcing whether the file uses a real color.
;;
(defun check-color (color)
  (if (and color
	   (not (= (g-value color :red)
		   (g-value color :green)
		   (g-value color :blue))))
      (setf *file-uses-color-p* T)
      NIL))

(defun check-style (obj style)
  (and *color-p*
       (or *file-uses-color-p*
	   (let ((st (g-value obj style)))
	     (if st
		 (or (check-color (g-value st :foreground-color))
		     (check-color (g-value st :background-color))))))))

(defun check-fs-color (obj)
  (check-style obj :filling-style))

(defun check-ls-color (obj)
  (check-style obj :line-style))

(defun check-fs-and-ls-color (obj)
  (or *file-uses-color-p*
      (check-style obj :filling-style)
      (check-style obj :line-style)))


;   These methods are executed on a "first pass" through the aggregate tree
; in order to find out which postscript functions will be used by the objects.
; 

(define-method :ps-register-fn OPAL:TEXT (obj)
  (pushnew *text-fn* *required-ps-fns*)
  (pushnew *line-fn* *required-ps-fns*)
  (check-ls-color obj))

(define-method :ps-register-fn OPAL:LINE (obj)
  (pushnew *line-fn* *required-ps-fns*)
  (check-ls-color obj))

(define-method :ps-register-fn OPAL:ARC (obj)
  (pushnew *arc-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:CIRCLE (obj)
  (pushnew *ellipse-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:OVAL (obj)
  (pushnew *ellipse-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:ROUNDTANGLE (obj)
  (pushnew *roundtangle-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:RECTANGLE (obj)
  (pushnew *rectangle-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:POLYLINE (obj)
  (pushnew *polyline-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))

(define-method :ps-register-fn OPAL:BITMAP (obj)
  (pushnew *bitmap-fn* *required-ps-fns*)
  (pushnew *def-image-fn* *required-ps-fns*)
  (let ((image (g-value obj :image)))
    (when image
      (unless (assoc image *image-list*)
	(push (cons image (make-image-name)) *image-list*))))
  (check-fs-color obj))

(define-method :ps-register-fn OPAL:PIXMAP (obj)
  (let ((image (g-value obj :image)))
    (if *color-p*
	(unless (assoc image *piximage-list*)
	  (pushnew *pixmap-fn* *required-ps-fns*)
	  (push (cons image (make-piximage-name)) *piximage-list*)
	  (setf *file-uses-color-p* t))
	(call-prototype-method obj))))
  


(define-method :ps-register-fn OPAL:VIRTUAL-AGGREGATE (obj)
  (let ((dummy-item (g-value obj :dummy-item)))
    (when dummy-item
      (kr-send dummy-item :ps-register-fn dummy-item)
      (check-ls-color dummy-item))))



;; All computations for position, dimension, and scaling are performed
;; before anything is written out to the file.  Then, everything is
;; written out to the file at once, at the bottom of Make-PS-File
;;

;; LBG says:
;; Calling MAKE-PS-FILE on a window which contains GG:MOTIF-SCROLLING-WINDOW-WITH-BARS and
;; OPAL:MULTIFONT-TEXT neglects to add the postscript definitions found in opal::*line-fn*,
;; opal::*text-fn*, and opal::*polyline-fn* to the beginning of the ps file. Therefore, we need to
;; add these to:

;;    (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*
;;				  *line-fn* *text-fn* *polyline-fn*))

(defun Make-PS-File (windows file-name
		     &key left top scale-x scale-y landscape-p
		          (paper-size :letter)
		          (position-x :center) (position-y :center)
		          (left-margin *1-inch*) (right-margin *1-inch*)
			  (top-margin *1-inch*) (bottom-margin *1-inch*)
		          (borders-p :motif)
			  (subwindows-p T) (clip-p T) (color-p T)
			  (background-color opal:white)
			  (title (if (schema-p windows)
				     (g-value windows :title)
				     "Untitled"))
			  (creator (concatenate 'string
				     "Make-PS-File -- Garnet Version "
				     common-lisp-user::Garnet-Version-Number))
			  (for "")
			  (comment "")
			  ;; For debugging the bbox computations
			  ;(debug NIL)
			  )
"Generate an Encapsulated PostScript file from Garnet windows.

Requires a Garnet WINDOW (or list of windows), and a PostScript
output FILE-NAME.  Optional arguments that affect the position
and appearance of the picture are:

POSITION-X - :LEFT, :CENTER, or :RIGHT. Default :CENTER.  

POSITION-Y - :TOP, :CENTER, or :BOTTOM.  Default :CENTER. 

LEFT-MARGIN, RIGHT-MARGIN, TOP-MARGIN, BOTTOM-MARGIN - Distance in
points, default 72.

LEFT, TOP - Distance in points, or default NIL to use POSITION-X
and POSITION-Y. 

SCALE-X, SCALE-Y - Scale factor for image.  Default is NIL, which means
the image will be automatically scaled to fit on the page.

LANDSCAPE-P - T or NIL, to rotate 90 degrees or portrait.  Default
is NIL.

PAPER-SIZE - :LETTER, :A4, or (WIDTH HEIGHT) in points specifies
page size. Default :LETTER.

SUBWINDOWS-P - T or NIL to include subwindows or not.  Default T.

BORDERS-P - T, NIL, :GENERIC, or :MOTIF, frames to print around windows.
Default :GENERIC.

CLIP-P - T, NIL, or (LEFT TOP WIDTH HEIGHT) in screen coordinates,
controls clipping.  Default T.

COLOR-P - T or NIL controls use of color.  Default T.

BACKGROUND-COLOR - Opal color, background fill.  Default is opal:white.

TITLE, CREATOR, FOR, COMMENT - Strings for header comments. 
"
  (let (region-left region-right region-top region-bottom
	region-width region-height)

    (if (not (listp windows))
	(setf windows (list windows)))

    ;; We call *temp-win* a "window", but it is really just a KR object
    ;; that has the same slots as a window.
    (setf *temp-win* (create-schema NIL
		       (:visible t)
		       (:background-color background-color)
		       (:border-width 0)
		       (:omit-title-bar-p t)
		       (:child windows)))
    (cond
      ((and clip-p (listp clip-p))
       (setf region-left (first clip-p))
       (setf region-top (second clip-p))
       (setf region-width (third clip-p))
       (setf region-height (fourth clip-p))
       (setf clip-p T)) ; Since the temp window has the same dimensions
			; as the clip region, just clip to the window
      (t
       (setf region-left gem:*screen-width*)
       (setf region-right 0)
       (setf region-top gem:*screen-height*)
       (setf region-bottom 0)

       ;; Figure maximum window size.
       (dolist (win (g-value *temp-win* :child))

	 (let* ((agg (g-value win :aggregate))
	        (wl (+ (g-value win :left)
		       (if clip-p 0 (if agg (g-value agg :left) 0))))
		(wt (+ (g-value win :top)
		       (if clip-p 0 (if agg (g-value agg :top) 0))))
		(wr (+ wl (if clip-p
			      (g-value win :width)
			      (if agg (g-value agg :width) 0))))
		(wb (+ wt (if clip-p
			      (g-value win :height)
			      (if agg (g-value agg :height) 0)))))

	   ;; Adjust for borders.
	   (multiple-value-bind
	    (border-left border-right border-top border-bottom)
	    (window-borders win borders-p)

	    (setf region-left
		  (min region-left wl))
	    (setf region-right
		  (max region-right
		       (+ wr border-left border-right)))
	    (setf region-top
		  (min region-top
		       (- wt border-top)))
	    (setf region-bottom
		  (max region-bottom
		       (+ wb border-bottom))))))

       (setf region-width (- region-right region-left))
       (setf region-height (- region-bottom region-top))))

    (s-value *temp-win* :left region-left)
    (s-value *temp-win* :top region-top)
    (s-value *temp-win* :width region-width)
    (s-value *temp-win* :height region-height)
    (s-value *temp-win* :border-width 0)
    (s-value *temp-win* :left-border-width 0)
    (s-value *temp-win* :right-border-width 0)
    (s-value *temp-win* :top-border-width 0)
    (s-value *temp-win* :bottom-border-width 0)
    (s-value *temp-win* :aggregate
	     (create-schema NIL
			    (:left 0) (:top 0)
			    (:width region-width)
			    (:height region-height)))

    (setf *clip-left* region-left)
    (setf *clip-right* region-right)
    (setf *clip-top* region-top)
    (setf *clip-bottom* region-bottom)
    (setf *color-p* color-p)
    (setf *font-list* NIL)
     ;; ** HERE IS THE CHANGE ** LBG 6/11/94
;;  (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*))
    (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*
				  *line-fn* *text-fn* *polyline-fn*))

    (setf *image-list* NIL)
    (setf *image-cnt* 0)
    (setf *piximage-list* NIL)
    (setf *piximage-cnt* 0)
    (when borders-p
      (pushnew *generic-window-frame-fn* *required-ps-fns*)
      (if (eq borders-p :motif)
	  (pushnew *motif-window-frame-fn* *required-ps-fns*)))
    (pushnew *rectangle-fn* *required-ps-fns*)

    (register-fns-in-win *temp-win* subwindows-p)

    ; Deal with the paper size
    (cond
      ((listp paper-size)
       (setf *page-width* (car paper-size)
	     *page-height* (cadr paper-size)))
      ((eq paper-size ':letter)
       (setf *page-width* 612
	     *page-height* 792))
      ((eq paper-size ':a4)
       (setf *page-width* 594
	     *page-height* 842))
      (t
       (error "unknown paper-size -- try :paper-size (width height)")))

    ; Rotate size 90 degrees if landscape.
    (if landscape-p
	(let ((temp *page-width*))
	  (setf *page-width* *page-height*)
	  (setf *page-height* temp)))

    ;; Adjust margins
    (setf *print-area-width* (- *page-width* left-margin right-margin))
    (setf *print-area-height* (- *page-height* top-margin bottom-margin))

    ;; Center window (or clipping region) on page by default
    ;; If not clipping, then look at the top-level aggregate instead of window
    (unless (and scale-x scale-y)
      (cond (scale-x
	     (setf scale-y (if (> region-height *print-area-height*)
			       (float (/ *print-area-height* region-height))
			       1)))
	    (scale-y
	     (setf scale-x (if (> region-width *print-area-width*)
			       (float (/ *print-area-width* region-width))
			       1)))
	    (t (setf scale-x (min 1 (float (/ *print-area-width*
					      region-width))
				  (float (/ *print-area-height*
					    region-height)))
		     scale-y scale-x))))

    ;; Adjust print area dimensions since it is getting scaled, too
    (setf *print-area-width* (ceiling (/ *print-area-width* scale-x)))
    (setf *print-area-height* (ceiling (/ *print-area-height* scale-y)))
    (unless left
      (setf left (case position-x
		   (:left (setf left 0))
		   (:center (setf left (round (- *print-area-width*
						 region-width) 2)))
		   (:right (setf left (- *print-area-width*
					 region-width))))))
    (unless top
      (setf top (case position-y
		  (:top (setf top 0))
		  (:center (setf top (round (- *print-area-height*
					       region-height) 2)))
		  (:bottom (setf top (- *print-area-height*
					region-height))))))

    ;; Compute compensation for windows with no title-bars
    (do* ((wins (g-value *temp-win* :child) (cdr wins))
	  (win (if wins (first wins)) (if wins (first wins)))
	  (this-left-thickness (if win (g-value win :left-border-width) 0)
			       (if win (g-value win :left-border-width) 0))
	  (this-top-thickness (if win (g-value win :top-border-width) 0)
			      (if win (g-value win :top-border-width) 0))
	  (left-thickness this-left-thickness
			  (max left-thickness this-left-thickness))
	  (top-thickness this-top-thickness
			 (max top-thickness this-top-thickness))
	  (left-different? NIL)
	  (top-different? NIL)
	  )
	 ((null wins) (progn
			(setf *thickest-left-border*
			      (if left-different? left-thickness 0))
			(setf *thickest-top-border*
			      (if top-different? top-thickness 0))))
      (unless (eq left-thickness this-left-thickness)
	(setf left-different? T))
      (unless (eq top-thickness this-top-thickness)
	(setf top-different? T)))

    ;; Compute boundingbox  llx = lower-left-x, urx = upper-right-x
    (cond
      (landscape-p
       (setf *llx* (round (+ left-margin (* top scale-y))))
       (setf *lly* (round (+ bottom-margin (* left scale-x))))
       (setf *urx* (round (+ *llx* (* region-height scale-y))))
       (setf *ury* (round (+ *lly* (* region-width scale-x)))))
      (t
       (setf *llx* (round (+ left-margin (* left scale-x))))
       (setf *lly* (round (+ bottom-margin (* (- *print-area-height*
						 (+ top region-height))
					      scale-y))))
       (setf *urx* (round (+ *llx* (* region-width scale-x))))
       (setf *ury* (round (+ *lly* (* region-height scale-y))))))

    ;; Now write everything to the file
    (Write-PS-To-File *temp-win* file-name
		      left top
		      scale-x scale-y landscape-p
		      left-margin bottom-margin
		      borders-p subwindows-p clip-p
		      title creator for comment NIL)
    ;; Reset color variable
    (setf *file-uses-color-p* NIL)

    ;; Clean up temporary window if one was allocated
    (when *temp-win*
      (s-value *temp-win* :child NIL)
      (opal:destroy (g-value *temp-win* :aggregate))
      (opal:destroy *temp-win*)
      (setf *temp-win* NIL)
    )
    T))

;; This function handles all of the file output.
;;
(defun Write-PS-To-File (win file-name left top
			 scale-x scale-y landscape-p
			 left-margin bottom-margin
			 borders-p subwindows-p clip-p
			 title creator for comment debug)
  (with-open-file (*standard-output* file-name :direction :output
				     :if-exists :supersede)
    (header-comments title creator for comment)
    (prolog)
    (gsave)
    (when landscape-p
      (translate *page-height* 0)
      (ps-rotate 90))
    (translate left-margin bottom-margin)
    (scale scale-x scale-y)
    (translate left (- *print-area-height* top)) ; XXX

    (ps-window win borders-p subwindows-p clip-p)
    (grestore) 
   (trailer-comments debug)))


(defun ps-window (win borders-p subwindows-p clip-p)
  (when (g-value win :visible)
    (let* ((agg (g-value win :aggregate))
	   (natural-p (or clip-p (null agg)))
	   (left (+ (g-value win :left)
		    (if natural-p 0 (g-value agg :left))))
	   (top (+ (g-value win :top)
		   (if natural-p 0 (g-value agg :top))))
	   (width (if natural-p
		      (g-value win :width)
		      (g-value agg :width)))
	   (height (if natural-p
		       (g-value win :height)
		       (g-value agg :height)))
	   (top-level-p (member win (g-value *temp-win* :child)))
	   (old-clip-left *clip-left*)
	   (old-clip-right *clip-right*)
	   (old-clip-top *clip-top*)
	   (old-clip-bottom *clip-bottom*))
      (multiple-value-bind
       (border-left border-right border-top border-bottom)
       (window-borders win borders-p)

	; XXX: TODO: if window bbox (with borders) doesn't overlap
	; clip bbox, then don't draw

       (format t "~%%~%% Begin new window~%%~%")

       (gsave)

       (if top-level-p
	   (translate (- (+ left border-left)
			 (g-value *temp-win* :left)
			 (if (and (g-value win :omit-title-bar-p)
				  (eq borders-p T))
			     *thickest-left-border*
			     0))
		      (- (g-value *temp-win* :top)
			 (- top (if (g-value win :omit-title-bar-p)
				    *thickest-top-border*
				    0))))
	   (unless (eq win *temp-win*)
	     (translate (+ left border-left)
			(- border-top top)))
	   )

       (when (and borders-p
		  (not (eq win *temp-win*)))
	 (add-font-to-list *window-frame-font*)
	   (cond
	    ((or (eq borders-p t)
		 (g-value win :omit-title-bar-p)
		 (not top-level-p))
	     (format T "() 0 0 ~S ~S ~S ~S ~S ~S DrawGenericWindowFrame~%"
		     width height
		     border-left border-top border-right border-bottom))

	    ((eq borders-p :generic)
	     (format T "(~A) 0 0 ~S ~S ~S ~S ~S ~S DrawGenericWindowFrame~%"
		     (g-value win :title)
		     width height
		     border-left border-top border-right border-bottom))

	    ((eq borders-p :motif)
	     (format T "(~A) 0 0 ~S ~S ~S ~S ~S ~S DrawMotifWindowFrame~%"
		     (g-value win :title)
		     width height
		     border-left border-top border-right border-bottom))))

       ;; Clip everything in this window (including subwindows) into the window
       (format T "0 0 ~S ~S ClipToRectangle~%" width height)
       (setf *clip-top* 0)
       (setf *clip-bottom* height)
       (setf *clip-left* 0)
       (setf *clip-right* width)

       ;; Print the meat of the window
       (let ((top-agg (g-value win :aggregate)))
	 (gsave)
	 (if (not clip-p)
	     (translate (- (g-value top-agg :left))
			(g-value top-agg :top)))
	 ;; maybe pass the window background color to the frame proc 
	 (print-window-background win)
	 (when top-agg
	   (kr-send top-agg :ps-object top-agg))
	 (grestore))

       ;; Print subwindows
       (when (or subwindows-p
		 (eq win *temp-win*))
	 (dolist (child (g-value win :child))
	   (ps-window child borders-p subwindows-p clip-p)))

       (setf *clip-top* old-clip-top)
       (setf *clip-bottom* old-clip-bottom)
       (setf *clip-left* old-clip-left)
       (setf *clip-right* old-clip-right)

       (grestore)))))

;;;
;;; Utility functions
;;;

(defun window-borders (win borders-p)
  (cond

   ((eq borders-p nil)
    (values 0 0 0 0))

   ((or (eq borders-p t)
	(g-value win :omit-title-bar-p)
	(not (member win (g-value *temp-win* :child))))
    (let ((border (g-value win :border-width)))
      (values border border border border)))

   ((eq borders-p :generic)
    (values (g-value win :left-border-width)
	    (g-value win :right-border-width)
	    (g-value win :top-border-width)
	    (g-value win :bottom-border-width)))

   ((eq borders-p :motif)
    (values *motif-window-border-left*
	    *motif-window-border-right*
	    *motif-window-border-top*
	    *motif-window-border-bottom*))

   (t
    (error "unknown borders-p value"))))

;    A special function to consider the background-color of the window
;
(defun print-window-background (win)
  (let ((background-color (g-value win :background-color)))
    (if background-color
	(let ((red (g-value background-color :red))
	      (green (g-value background-color :green))
	      (blue (g-value background-color :blue)))
	  (if *color-p*
	      (format t "0 0 ~S ~S [0 0 0] 0 0 [] -1 [~A ~A ~A] DrawRectangle~%"
		      ;; XXX This size will be wrong if clip-p is NIL
		      (g-value win :width) (g-value win :height)
		      ;;; XXX: Optimize with BLACK and WHITE color tokens
		      (ps-number red)
		      (ps-number green)
		      (ps-number blue))
	      (format t "0 0 ~S ~S [0 0 0] 0 0 [] -1 [~A dup dup] DrawRectangle~%"
		      (g-value win :width) (g-value win :height)
		      (ps-number (float (/ (+ red green blue) 3)))))))))
	      
    
;    A filling-style is called 'arbitrary' if it is not one of the pre-defined
; opal halftones like opal:gray-fill, and has BLACK and WHITE as the foreground
; and background colors.  Opal:diamond-fill is considered an arbitrary filling-
; style.
;
(defun arbitrary-pattern-p (style)
  (when style
    (let ((stipple (g-value style :stipple)))
      (cond
	((not stipple)
	 nil)
	((not (g-value stipple :percent))
	 t)
	((and (eq (g-value style :foreground-color) opal:black)
	      (eq (g-value style :background-color) opal:white))
	 nil)
	(t *color-p*)))))

;    This function is called to put an arbitrary filling style on the stack,
; which will be used to fill the next shape drawn.  The filling style has
; been defined with its image-name at the top of the postscript file, and
; here it is just referenced.
;
(defun handle-arbitrary-pattern (style)
  (let* ((image (g-value style :stipple :image))
	 (image-name (cdr (assoc image *image-list*))))
    ;; The following arguments are pushed on the stack to be passed
    ;; to FillPattern by FillShape. 

    (format T "[ ~A ~A ~A "
	    ;;; XXX: Optimize with BLACK and WHITE color tokens
	    (ps-number (g-value style :foreground-color :red))
	    (ps-number (g-value style :foreground-color :green))
	    (ps-number (g-value style :foreground-color :blue)))
    (if (or (g-value style :stipple :percent)
	    (eq (g-value style :fill-style) :opaque-stippled)
	    (eq (g-value style :line-style) :dash)
	    (eq (g-value style :line-style) :double-dash))

	(format T "~A ~A ~A true "
	        ;;; XXX: Optimize with BLACK and WHITE color tokens
		(ps-number (g-value style :background-color :red))
		(ps-number (g-value style :background-color :green))
		(ps-number (g-value style :background-color :blue)))
	(format T "false "))
    (format T "0 ~A-font ] " ; should 0 line up with grid?
	    image-name)))


(defun print-bit-array (image-name a width height flip-p)
  (let* ((power 0) (digit 0)
         (bwidth (ceiling width 8))  ; Min bytes needed for one row
         (max-col (- width 1))  ; dotimes is 0-based
         (max-row (- height 1)) ; dotimes is 0-based
         (digits-in-array-row (+ 1 (* 2 bwidth)))
         (cols-before-newline (max 1 (floor 78 digits-in-array-row)))
         (cbn-1 (- cols-before-newline 1)))
    (format T "/~A /~A-font ~S ~S <~%" image-name image-name width height)
    (dotimes (row height)
      (setf power 0) (setf digit 0)
      (dotimes (col (* bwidth 8))
	(setf power (case power (0 3) (t (1- power))))
	(setf digit
	      (+ digit (* (if (> col max-col)
			      0
			      (if flip-p 
				  (aref a (- max-row row) col)
				  (if (eq (aref a (- max-row row) col) gem::*black*)
				      0 1)))
			  (expt 2 power))))
	(when (eq 0 power)
	  ;(format T "~1X" (if flip-p (- #xf digit) digit))
	  (write (if flip-p (- #xf digit) digit) :base 16)
	  (setf digit 0)))
      (unless (eq row max-row)
	(if (eq cbn-1 (mod row cols-before-newline))
	    (terpri)
	    (write-char #\ ))))
    (format t ">~%DefImage~%~%")))
  


;    This function looks at the image in the filling-style of the object.
; When the filling-style was created, a two-dimensional array was stored with
; it (called A below).  This function goes through the array of 1's and 0's
; and generates corresponding hex numbers.
;    The hex numbers are generated with respect to a full-byte representation
; of the array.  That is, zeroes are filled in at the end of an array row if
; the width is not an integral number of bytes.  This is required by the
; postscript function that will be called with the data.
;
#+apple
(defun print-image-info (image image-name)
  (if (eq gem::*MAC-BUFFER* (class-of image))
      ;; IMAGE is a CLOS MAC-BUFFER object (an instance of ccl::Gworld)
      (let* ((size (slot-value image 'ccl::size))
             (width (ccl:point-h size))
             (height (ccl:point-v size))
             (pixarray (getf (slot-value image 'gem::plist) :pixarray))
             (flip-p NIL))
        (print-bit-array image-name pixarray width height flip-p))

      (let ((width 8)
            (height 8)
            (flip-p T))
        (format T "/~A /~A-font ~S ~S <~%" image-name image-name width height)
        (dotimes (row height)
          (let ((digit (ccl:rref image (#-ccl-3 :pattern.array #+ccl-3 :pattern.pat row) :storage :pointer)))
            (write (if flip-p (- #xff digit) digit) :base 16)
            (write-char #\ )))
        (format t ">~%DefImage~%~%"))))


#-apple
(defun print-image-info (image image-name)
  ;; Need to have z-type images to get information from
  (let* ((flip-p (unless (xlib:image-z-p image)
		   (setf image (xlib:copy-image image
						:result-type 'xlib:image-z))))
	 (width (xlib:image-width image))
	 (height (xlib:image-height image))
	 (a (xlib:image-z-pixarray image)))
    (print-bit-array image-name a width height flip-p)))


#+apple
(defun print-piximage-info (image image-name)
  ;; IMAGE is a CLOS MAC-BUFFER object (an instance of ccl::Gworld)
  (let* ((size (slot-value image 'ccl::size))
         (width (ccl:point-h size))
         (height (ccl:point-v size))
         (pixarray (getf (slot-value image 'gem::plist) :pixarray)))
    (format T "/~A <~%" image-name)
    (dotimes (row height)
      (dotimes (col width)
        (format T "~6,'0X" (aref pixarray row col)))
      (terpri))
    (format t ">~%def~%~%")))

#-apple
(defun print-piximage-info (image image-name)
  (let* ((width (xlib:image-width image))
	 (height (xlib:image-height image))
	 (a (xlib:image-z-pixarray image))
	 (max-col (- width 1))  ; dotimes is 0-based
	 (color-alist '())
	 )
    (format T "/~A <~%" image-name)
    (dotimes (row height)
      (dotimes (col width)
	(let* ((digit (if (> col max-col) 0 (aref a row col)))
	       (rgb (cdr (assoc digit color-alist))))
	  (cond
	    (rgb
	     (format T "~2,'0X" (first rgb))
	     (format T "~2,'0X" (second rgb))
	     (format T "~2,'0X" (third rgb)))
	    (t (let* ((xcolor (car (xlib:query-colors
				    gem::*default-x-colormap* (list digit))))
		      (red (inter:clip-and-map
			    (xlib:color-red xcolor) 0 1 0 255))
		      (green (inter:clip-and-map
			      (xlib:color-green xcolor) 0 1 0 255))
		      (blue (inter:clip-and-map
			     (xlib:color-blue xcolor) 0 1 0 255)))
		 (push (list digit red green blue) color-alist)
		 (format T "~2,'0X" red)
		 (format T "~2,'0X" green)
		 (format T "~2,'0X" blue))))))
      (terpri))
    (format t ">~%def~%~%")))

    
; Works for either filling-styles or line-styles
(defun print-color-info (style ground)
  (if style
      (let* ((color (g-value style ground))
	     (stipple (g-value style :stipple))
	     (gray (if stipple (g-value stipple :percent))))
	(if (arbitrary-pattern-p style)
	    (handle-arbitrary-pattern style)
	    (if gray
	        (format t "[~A dup dup] "
			(ps-number
			 (float (/ (- 100 gray) 100))))
		(let ((red (g-value color :red))
		      (green (g-value color :green))
		      (blue (g-value color :blue)))
		(if *color-p*
		    (format t "[~A ~A ~A] " (ps-number red)
			                    (ps-number green)
					    (ps-number blue))
		    ;(format t "[0 0 0] ")
		    (format t "[~A dup dup] " (float (/ (+ red green blue) 3)))
		    )))))
      ; Should we just print a gray scale? "[~A dup dup] "
      (format t "null ")))

; Parameters: line-color, line-cap, line-join, dash-pattern, line-thickness
; Line-thickness of -1 means don't draw a line
;;; LBG 4/9/94 A change which speeds up make-ps-file by factor of ~600.
(defun print-line-qualities (obj)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((line-style (g-value obj :line-style)))
    (if line-style
	(let* ((line-thickness (let ((lt (g-value line-style :line-thickness)))
				 (if (eq lt 0) 1 lt)))
	       (line-cap (case (g-value line-style :cap-style)
			   (:butt 0) (:round 1) (:projecting 2)
			   (otherwise 0)))
	       (line-join (case (g-value line-style :join-style)
			    (:miter 0) (:round 1) (:bevel 2)
			    (otherwise 0)))
	       (dash-pattern (let ((dp (g-value line-style :dash-pattern)))
			       (if dp
				   (substitute #\[ #\(
				     (substitute #\] #\)
				       (concatenate 'string
					 (prin1-to-string dp) " ")))
				   "[] "))))
	  (print-color-info line-style :foreground-color)
	  (format T "~S ~S " line-cap line-join)
	  ;; Here is the change LBG
	  ;;	  (format T   dash-pattern)
	  (format T "~a"  dash-pattern)
	  (format T "~S " line-thickness))
	;; Don't draw a line
	(format T "[0 0 0] 0 0 [] -1 "))))

(defun print-graphic-qualities (obj)
  (print-line-qualities obj)
  (let ((filling-style (g-value obj :filling-style)))
    ;; Print fill-halftone
    (print-color-info filling-style :foreground-color)))


;;;
;;; Aggregate
;;;

(define-method :ps-object OPAL:AGGREGATE (obj)
  (dovalues (comp obj :components)
    (if (and (g-value comp :visible)
	     (not (eq :no-op (g-value comp :draw-function)))
	     ; XXX: TODO: only draw object of it's in the clip box
	     ; (or (not clip-p) <object bbox overlaps :window bbox>)
	     )
	(kr-send comp :ps-object comp))))

;;;
;;; Text
;;;

;; Add-Font-To-List will put the font-name in a global list which is
;; printed out at the end of the postscript file.
;;
(defun add-font-to-list (font-name)
  (pushnew font-name *font-list* :test #'string=))

(defun remove-left (string)
  (let ((index (position #\( string)))
    (if index
	(let ((part1 (subseq string 0 index))
	      (part2 (subseq string (1+ index))))
	  ;; The double-\\ turn into single-\ when formatted
	  (concatenate 'string part1 "\\050" (remove-left part2)))
	string)))

(defun remove-right (string)
  (let ((index (position #\) string)))
    (if index
	(let ((part1 (subseq string 0 index))
	      (part2 (subseq string (1+ index))))
	  ;; The double-\\ turn into single-\ when formatted
	  (concatenate 'string part1 "\\051" (remove-right part2)))
	string)))

;; Since postscript uses parentheses as quote marks, we have to convert
;; parens in the string to \050 and \051.
;;
(defun convert-parentheses (string)
  (if (or (find #\( string) (find #\) string))
      (remove-left (remove-right string))
      string))

;; Double each occurrence of a backslash.  When we see a \ in the string,
;; we have to replace it with \\\\.  The \\\\ becomes \\ when it is formatted
;; in lisp, and that \\ becomes \ when it is printed in PostScript.
;;
(defun double-backslashes (string)
  (let ((index (position #\\ string)))
    (if index
	(let ((part1 (subseq string 0 index))
	      (part2 (subseq string (1+ index))))
	  (concatenate 'string part1 "\\\\" (double-backslashes part2)))
	string)))

(define-method :ps-object OPAL:TEXT (obj)
  (let ((line-style (g-value obj :line-style))
	(cut-strings (g-value obj :cut-strings)))
    (if (and line-style cut-strings)
	(let* ((font (g-value obj :font))
	       (font-size (g-value font :ps-font-size))
	       (font-name (g-value font :ps-font-name))
	       (justification (g-value obj :justification))
	       (left (g-value obj :left)) (top (convert-y (g-value obj :top)))
	       (height (opal:string-height font "X"))
	       (max-line-width (g-value obj :width))
               (win (g-value obj :window))
	       (ascent (gem:max-character-ascent win font)))
	  (add-font-to-list font-name)
	  (dotimes (i (length cut-strings))
	    (let* ((cut-string (nth i cut-strings))
		   (string-top (- top (* i height)))
		   (base-y (- string-top ascent))
		   (opal-width (cut-string-width cut-string))
		   (string (convert-parentheses
			    (cut-string-string cut-string))))
	      (unless (string= string "")
		(format t "~S ~S ~S ~S ~S "
			(+ left
			   (case justification
			     (:right (- max-line-width opal-width))
			     (:center (floor (- max-line-width opal-width) 2))
			     (t 0)))
			string-top base-y opal-width height)
		(if (g-value obj :fill-background-p)
		    (format t "true ")
		    (format t "false "))
		(print-color-info line-style :foreground-color)
		(print-color-info line-style :background-color)
		(format t "(~A) ~A ~A DrawText~%"
			string (prin1-to-string font-size) font-name))))
  ;; Print the cursor (as a line)
  (let ((cursor-index (g-value obj :cursor-index)))
    (if (and cursor-index
	     (g-value obj :line-style)
	     font)
	(let* ((line-number (g-value obj :line-number))
	       (line-height (+ ascent (gem:max-character-descent win font)))
	       (cursor-offset (g-value obj :cursor-offset)))

	  ;; Parameters: line-halftone, line-cap, line-join, dash-pattern,
	  ;;             line-thickness, x2, y2, x1, y1
	  (print-line-qualities obj)
	  (format T "~S ~S ~S ~S " (+ left cursor-offset)
		  (- top (* line-number line-height))
		  (+ left cursor-offset)
		  (- top (* (1+ line-number) line-height)))
	  (format T "DrawLine~%"))))
  ))))


;;;
;;;  Lines
;;;

(define-method :ps-object OPAL:LINE (obj)
  ;; Parameters: line-halftone, line-cap, line-join, dash-pattern,
  ;;             line-thickness, x2, y2, x1, y1
  (when (g-value obj :line-style)
    (print-line-qualities obj)
    (format T "~S ~S ~S ~S " (g-value obj :x2)
	                     (convert-y (g-value obj :y2))
			     (g-value obj :x1)
			     (convert-y (g-value obj :y1)))
    (format T "DrawLine~%")))

;;;
;;;  Circles, Arcs, Ovals
;;;

;; This method should really be split into two - one method for ARCs and
;; one method for CIRCLEs.
;;
(define-method :ps-object OPAL:ARC (obj)
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((left (g-value obj :left))
	   (opal-top (g-value obj :top))
	   (width (g-value obj :width))
	   (height (g-value obj :height))
	   (radius-x (float (/ width 2)))
	   (radius-y (float (/ height 2)))
	   (center-x (+ left radius-x))
	   (center-y (convert-y (+ opal-top radius-y)))
	   (angle1 (convert-angle (g-value obj :angle1)))
	   (angle2 (+ angle1 (convert-angle (g-value obj :angle2)))))
      ;; Parameters: center-x, center-y, radius-x, radius-y, angle1, angle2,
      ;;             line-thickness, line-halftone, fill-halftone
      (format T "~S ~S ~S ~S ~S ~S " center-x center-y radius-x radius-y
	                             angle1 angle2)
      (print-graphic-qualities obj)
      (format T "DrawArc~%"))))

;; Same as OPAL:ARC except angles are ignored
;;
(define-method :ps-object OPAL:OVAL (obj)
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((left (g-value obj :left))
	   (opal-top (g-value obj :top))
	   (width (g-value obj :width))
	   (height (g-value obj :height))
	   (radius-x (float (/ width 2)))
	   (radius-y (float (/ height 2)))
	   (center-x (+ left radius-x))
	   (center-y (convert-y (+ opal-top radius-y))))
      ;; Parameters: center-x, center-y, radius-x, radius-y, angle1, angle2,
      ;;             line-thickness, line-halftone, fill-halftone
      (format T "~S ~S ~S ~S 0 360 " center-x center-y radius-x radius-y)
      (print-graphic-qualities obj)
      (format T "DrawEllipse~%"))))

;; Same as OPAL:OVAL except diameter is the minimum of the width and height
;;
(define-method :ps-object OPAL:CIRCLE (obj)
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((left (g-value obj :left))
	   (opal-top (g-value obj :top))
	   (width (min (g-value obj :width) (g-value obj :height)))
	   (radius (float (/ width 2)))
	   (center-x (+ left radius))
	   (center-y (convert-y (+ opal-top radius))))
      ;; Parameters: center-x, center-y, radius-x, radius-y, angle1, angle2,
      ;;             line-thickness, line-halftone, fill-halftone
      (format T "~S ~S ~S ~S 0 360 " center-x center-y radius radius)
      (print-graphic-qualities obj)
      (format T "DrawEllipse~%"))))
      
	   

;;;
;;;  Roundtangles
;;;

(define-method :ps-object OPAL:ROUNDTANGLE (obj)
  ;; Parameters: left, top, width, height, radius, line-thickness,
  ;; line-halftone, line-cap, line-join, dash-pattern, fill-halftone
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((left (g-value obj :left))
	   (top (convert-y (g-value obj :top)))
	   (width (g-value obj :width))
	   (height (g-value obj :height))
	   (radius (g-value obj :draw-radius)))
      (when (and (plusp width) (plusp height) (plusp radius))
	(format T "~S ~S ~S ~S ~S " left top width height radius)
	(print-graphic-qualities obj)
	(format T "DrawRoundtangle~%")))))
  
;;;
;;;  Rectangles
;;;

(define-method :ps-object OPAL:RECTANGLE (obj)
  ;; Parameters: left, top, width, height,
  ;;             line-thickness, line-halftone, fill-halftone
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((left (g-value obj :left))
	   (top (convert-y (g-value obj :top)))
	   (width (g-value obj :width))
	   (height (g-value obj :height)))
      (format T "~S ~S ~S ~S " left top width height))
    (print-graphic-qualities obj)
    (format T "DrawRectangle~%")))

;;;
;;;  Polylines
;;;
#|
(define-method :ps-object OPAL:POLYLINE (obj)
  ;; Parameters: x1, y1, ..., xn, yn, n
  (when (and (or (g-value obj :line-style) (g-value obj :filling-style))
	     (g-value obj :point-list))
    (let ((point-list (g-value obj :point-list)))
      ;; Convert all the y-coordinates while printing
      (format T "~S ~S {~%  { " (car point-list) (convert-y (cadr point-list)))
      (do* ((counter 0 (1+ counter))
	    (points (cddr point-list) (cddr points)))
	   ((null points) (format T "}~%"))
	(format T "~S ~S " (first points) (convert-y (second points)))
	(when (>= counter 64)
	  (format T "}~%  {")
	  (setf counter 0)))
      (format T "} "))
    (print-graphic-qualities obj)
    (format T "DrawPolyline~%")))
|#

(defun write-points (points)
  (write-string "  { ")
  (mapc #'(lambda (n) (write n) (write-char #\ )) points)
  (write-char #\})
  (terpri))

(define-method :ps-object OPAL:POLYLINE (obj)
  ;; Parameters: x1, y1, ..., xn, yn, n
  (when (and (or (g-value obj :line-style) (g-value obj :filling-style))
	     (g-value obj :point-list))
    (let ((point-list (g-value obj :point-list))
	  (counter 0)
	  (points '()))
      ;; Convert all the y-coordinates while printing
      (format T "~S ~S {~%" (car point-list) (convert-y (cadr point-list)))
      (setf point-list (cddr point-list))
      (dotimes (i (truncate (length point-list) 2))
	(setf points (cons (car point-list)
			   (cons (convert-y (cadr point-list)) points)))
	(setf point-list (cddr point-list))
	(incf counter)
	(when (>= counter 64)
	  (write-points points)
	  (setf counter 0)
	  (setf points '())))
      (if (> counter 0)
	  (write-points points))
      (write-string "} "))
    (print-graphic-qualities obj)
    (format T "DrawPolyline~%")))

;;
;; Bitmaps
;;

(define-method :ps-object OPAL:BITMAP (obj)
  (let ((filling-style (g-value obj :filling-style)))
    (when filling-style
      (let* ((image (g-value obj :image))
	     (image-name (cdr (assoc image *image-list*)))
	     (transparent-p (eq :stippled
				(g-value filling-style :fill-style))))
	(format t "~S ~S ~S ~S ~A ~A "
		(g-value obj :left) (convert-y (g-value obj :top))
		(g-value obj :width) (g-value obj :height)
		image-name
		(if transparent-p "true" "false"))
	(print-color-info filling-style :foreground-color)
	(format t "DrawBitmap~%")))))


;;
;; Pixmaps
;;

(define-method :ps-object OPAL:PIXMAP (obj)
  (if *color-p*
      (let* ((image (g-value obj :image))
	     (image-name (cdr (assoc image *piximage-list*))))
	(format t "~S ~S ~S ~S ~S ~S ~A DrawPixmap~%"
		(g-value obj :left) (convert-y (+ (g-value obj :height)
						  (g-value obj :top)))
		(g-value obj :width) (g-value obj :height)
		(g-value obj :width) (g-value obj :height)
		image-name))
      (call-prototype-method obj)))
	


;;
;; Virtual Aggregate
;;

(define-method :ps-object OPAL:VIRTUAL-AGGREGATE (gob)
  (let* ((dummy (g-value gob :dummy-item))
	 (update-info (g-value dummy :update-info))
	 (item-array (g-value gob :item-array))
	 (array-size (g-value gob :array-length)))
    (if (numberp array-size)
	;; 1-dimensional
	(dotimes (n (g-value gob :next-available-rank))
	  (s-value dummy :rank n)
	  (s-value dummy :item-values (aref item-array n))
	  (opal::update-slots-values-changed dummy 0 update-info)
	  (when (and (aref item-array n) (g-value dummy :visible))
	    (kr-send dummy :ps-object dummy)))
	;; 2-dimensional
	(dotimes (n (first array-size))
	  (dotimes (m (second array-size))
	    (s-value dummy :rank1 m)
	    (s-value dummy :rank2 n)
	    (s-value dummy :item-values (aref item-array m n))
            (opal::update-slots-values-changed dummy 0 update-info)
	    (when (and (aref item-array m n) (g-value dummy :visible))
	      (kr-send dummy :ps-object dummy)))))))
