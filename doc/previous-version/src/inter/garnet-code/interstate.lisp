(setf garnetdraw::*DRAW-AGG*

(create-instance 'INTERSTATE OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 680))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 410))
  (:CONSTANT `(:COMPONENTS ))
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 673)
  (:WINDOW-HEIGHT 464)
  (:PACKAGE-NAME "USER")
  (:WINDOW-TITLE "INTERSTATE")
  (:EXPORT-P T)
  (:FUNCTION-FOR-OK NIL)
  (:parts `(
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 276))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 35))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Running-action")
      (:BOX (276 35 98 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 610))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 300))
      (:CONSTANT (:KNOWN-AS T ))
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (30 125 610 300 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 45))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 575))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 240))
      (:CONSTANT (:KNOWN-AS T ))
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (45 125 575 240 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 60))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 540))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 190))
      (:CONSTANT (:KNOWN-AS T ))
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (60 125 540 190 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 75))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 250))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 140))
      (:CONSTANT (:KNOWN-AS T ))
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (75 125 250 140 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 90))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 210))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 90))
      (:CONSTANT (:KNOWN-AS T ))
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (90 125 210 90 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE")
      (:GG-INTERIM-SELECTED NIL)
      (:FRAC-LEFT 90)
      (:FRAC-TOP 125)
      (:FRAC-WIDTH 210)
      (:FRAC-HEIGHT 90))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 345))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 125))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 235))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 90))
      (:CONSTANT (:KNOWN-AS T ))
      (:FRAC-HEIGHT 90)
      (:FRAC-WIDTH 235)
      (:FRAC-TOP 125)
      (:FRAC-LEFT 345)
      (:GG-INTERIM-SELECTED NIL)
      (:RADIUS 20)
      (:GROW-P T)
      (:BOX (345 125 235 90 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 15))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 100))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 50))
      (:CONSTANT (:KNOWN-AS T ))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GROW-P T)
      (:BOX (15 100 100 50 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 272))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 100))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 50))
      (:CONSTANT (:KNOWN-AS T ))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GROW-P T)
      (:BOX (272 100 100 50 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ROUNDTANGLE
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 560))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 100))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 100))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 50))
      (:CONSTANT (:KNOWN-AS T ))
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:GROW-P T)
      (:BOX (560 100 100 50 ))
      (:BEHAVIORS NIL)
      (:GILT-REF "TYPE-RECTANGLE"))
    (NIL ,OPAL:ARC
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 43))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 55))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 40))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 90))
      (:CONSTANT (:KNOWN-AS ))
      (:ANGLE2 3.141592653589793d0)
      (:ANGLE1 0)
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (43 55 40 90 )))
    (NIL ,OPAL:ARC
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 305))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 55))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 40))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 90))
      (:CONSTANT (:KNOWN-AS ))
      (:ANGLE2 3.141592653589793d0)
      (:ANGLE1 0)
      (:GILT-REF "TYPE-RECTANGLE")
      (:BOX (305 55 40 90 )))
    (NIL ,OPAL:TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 40))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 117))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Start")
      (:BOX (40 117 52 19 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 286))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 116))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Running")
      (:BOX (286 116 75 19 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 574))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 117))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Outside")
      (:BOX (574 117 74 19 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 155))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 80))
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (155 80 76 42 ))
      (:STRING "start-event
   over
start-where")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:JUSTIFICATION :left))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 151))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 128))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Start-action")
      (:BOX (151 128 3 3 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 159))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 197))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "stop-event")
      (:BOX (159 197 69 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 155))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 216))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Stop-action")
      (:BOX (155 216 83 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 155))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 247))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "abort-event")
      (:BOX (155 247 76 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 151))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 267))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Abort-action")
      (:BOX (151 267 84 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 287))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 297))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "abort-event")
      (:BOX (287 297 76 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 283))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 318))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Abort-action")
      (:BOX (283 318 84 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 283))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 368))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Abort-action")
      (:BOX (283 368 84 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 287))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 428))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Stop-action")
      (:BOX (287 428 84 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 197))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 347))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "stop-event if outside-control = Abort")
      (:BOX (197 347 76 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 199))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 407))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "stop-event if outside-control = Last")
      (:BOX (199 407 76 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 421))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 94))
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (421 94 90 28 ))
      (:STRING "  not over
running-where")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:JUSTIFICATION :left))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 421))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 185))
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (421 185 90 28 ))
      (:STRING "  back over
running-where")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :ITALIC)))
      (:JUSTIFICATION :left))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 418))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 128))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Outside-action")
      (:BOX (418 128 84 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 403))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 218))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Back-inside-action")
      (:BOX (403 218 126 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 20))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 35))
      (:CONSTANT (:KNOWN-AS T ))
      (:STRING "Stop-action")
      (:BOX (30 35 98 14 ))
      (:BEHAVIORS NIL)
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:GILT-REF "TYPE-TEXT"))
    (NIL ,OPAL:TEXT
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 14))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 20))
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (14 20 110 14 ))
      (:STRING "continuous = NIL")
      (:font ,(create-instance NIL opal:font (:face :italic))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (90 170 90 150 ))
      (:FRAC-X1 90)
      (:FRAC-Y1 169)
      (:FRAC-X2 90)
      (:FRAC-Y2 149)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (30 170 30 150 ))
      (:FRAC-X1 30)
      (:FRAC-Y1 171)
      (:FRAC-X2 30)
      (:FRAC-Y2 151)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (45 170 45 150 ))
      (:FRAC-X1 45)
      (:FRAC-Y1 170)
      (:FRAC-X2 45)
      (:FRAC-Y2 150)
      (:GG-INTERIM-SELECTED NIL)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (60 170 60 150 ))
      (:FRAC-X1 60)
      (:FRAC-Y1 170)
      (:FRAC-X2 60)
      (:FRAC-Y2 150)
      (:GG-INTERIM-SELECTED NIL)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (75 170 75 150 ))
      (:FRAC-X1 75)
      (:FRAC-Y1 170)
      (:FRAC-X2 75)
      (:FRAC-Y2 150)
      (:GG-INTERIM-SELECTED NIL)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (345 169 345 149 ))
      (:FRAC-X1 345)
      (:FRAC-Y1 169)
      (:FRAC-X2 345)
      (:FRAC-Y2 149)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:FRAC-Y2 125)
      (:FRAC-X2 272)
      (:FRAC-Y1 125)
      (:FRAC-X1 258)
      (:POINTS (258 125 272 125 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:FRAC-Y2 125)
      (:FRAC-X2 559)
      (:FRAC-Y1 125)
      (:FRAC-X1 545)
      (:POINTS (545 125 559 125 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:parts (
        (:LINE :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD2 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:FRAC-Y2 97)
      (:FRAC-X2 346)
      (:FRAC-Y1 92)
      (:FRAC-X1 346)
      (:POINTS (345 93 345 98 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:GG-INTERIM-SELECTED NIL)
      (:FRAC-Y2 95)
      (:FRAC-X2 83)
      (:FRAC-Y1 91)
      (:FRAC-X1 83)
      (:POINTS (83 93 83 98 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:BEHAVIORS NIL))))))))

)