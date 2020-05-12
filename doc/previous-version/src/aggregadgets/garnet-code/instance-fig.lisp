(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 600)(:top 10)(:width 357)(:height 238))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate
			  (:left 10) (:top 10)
			  (:width 200) (:height 200)))
(opal:update WIN)

(setf garnetdraw::*DRAW-AGG*
(create-instance 'INSTANCE OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GV GARNETDRAW::DRAW-WIN :WIDTH) 680))
  (:HEIGHT (o-formula (GV GARNETDRAW::DRAW-WIN :HEIGHT) 410))
  (:CONSTANT `(:COMPONENTS ))
  (:parts `(
    (NIL ,GARNETDRAW::MOVING-AGG
      (:CONSTANT (:KNOWN-AS :COMPONENTS ))
      (:OLD-LEFT 11)
      (:OLD-TOP 36)
      (:OLD-WIDTH 155)
      (:OLD-HEIGHT 111)
      (:parts (
        (NIL ,GARNETDRAW::MOVING-ROUNDTANGLE
          (:CONSTANT (:KNOWN-AS ))
          (:TEMP-BOX (11 36 155 111 ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (11 36 155 111 ))
          (:FRAC-LEFT 11)
          (:FRAC-TOP 36)
          (:FRAC-WIDTH 155)
          (:FRAC-HEIGHT 111))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (21 96 45 40 ))
          (:FRAC-LEFT 21)
          (:FRAC-TOP 96)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:TEMP-BOX (21 96 45 40 )))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 37))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 104))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "B")
          (:BOX (37 104 14 25 ))
          (:FRAC-LEFT 37)
          (:FRAC-TOP 104)
          (:FRAC-WIDTH 14)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 1)
          (:TEMP-BOX (37 104 14 25 )))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (111 96 45 40 ))
          (:FRAC-LEFT 111)
          (:FRAC-TOP 96)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:BEHAVIORS NIL)
          (:TEMP-BOX (111 96 45 40 )))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 127))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 104))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "C")
          (:BOX (127 104 14 25 ))
          (:FRAC-LEFT 127)
          (:FRAC-TOP 104)
          (:FRAC-WIDTH 14)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 1)
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:TEMP-BOX (127 104 14 25 )))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (66 46 45 40 ))
          (:FRAC-LEFT 66)
          (:FRAC-TOP 46)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:TEMP-BOX (66 46 45 40 ))
          (:BEHAVIORS NIL))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 82))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 54))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "A")
          (:BOX (82 54 14 25 ))
          (:FRAC-LEFT 82)
          (:FRAC-TOP 54)
          (:FRAC-WIDTH 14)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 1)
          (:TEMP-BOX (82 54 14 25 ))
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (NIL ,GARNETDRAW::MOVING-LINE
          (:CONSTANT (:KNOWN-AS ))
          (:TEMP-POINTS (56 100 19 -20 ))
          (:PARTS ((:LINE :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD1 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD2 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 4)))
          (:FILLING-STYLE NIL)
          (:POINTS (56 100 75 80 ))
          (:FRAC-X1 56)
          (:FRAC-Y1 100)
          (:FRAC-X2 75)
          (:FRAC-Y2 80))
        (NIL ,GARNETDRAW::MOVING-LINE
          (:CONSTANT (:KNOWN-AS ))
          (:TEMP-POINTS (120 100 -18 -21 ))
          (:PARTS ((:LINE :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD1 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD2 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 4)))
          (:FILLING-STYLE NIL)
          (:POINTS (120 100 102 79 ))
          (:FRAC-X1 120)
          (:FRAC-Y1 100)
          (:FRAC-X2 102)
          (:FRAC-Y2 79)))))
    (NIL ,GARNETDRAW::MOVING-AGG
      (:CONSTANT (:KNOWN-AS :COMPONENTS ))
      (:OLD-HEIGHT 111)
      (:OLD-WIDTH 155)
      (:OLD-TOP 117)
      (:OLD-LEFT 193)
      (:BOX (217 131 155 111 ))
      (:parts (
        (NIL ,GARNETDRAW::MOVING-ROUNDTANGLE
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (193 117 155 111 ))
          (:FRAC-LEFT 193)
          (:FRAC-TOP 117)
          (:FRAC-WIDTH 155)
          (:FRAC-HEIGHT 111)
          (:BEHAVIORS NIL)
          (:TEMP-BOX (193 117 155 111 )))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (203 177 45 40 ))
          (:FRAC-LEFT 203)
          (:FRAC-TOP 177)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:TEMP-BOX (203 177 45 40 ))
          (:BEHAVIORS NIL))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 217))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 185))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "B'")
          (:BOX (217 185 23 25 ))
          (:FRAC-LEFT 217)
          (:FRAC-TOP 185)
          (:FRAC-WIDTH 23)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 2)
          (:TEMP-BOX (217 185 23 25 ))
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (293 177 45 40 ))
          (:FRAC-LEFT 293)
          (:FRAC-TOP 177)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:BEHAVIORS NIL)
          (:TEMP-BOX (293 177 45 40 )))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 306))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 185))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "C'")
          (:BOX (306 185 23 25 ))
          (:FRAC-LEFT 306)
          (:FRAC-TOP 185)
          (:FRAC-WIDTH 23)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 2)
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:TEMP-BOX (306 185 23 25 )))
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FILLING-STYLE NIL)
          (:DRAW-FUNCTION :COPY)
          (:BOX (248 127 45 40 ))
          (:FRAC-LEFT 248)
          (:FRAC-TOP 127)
          (:FRAC-WIDTH 45)
          (:FRAC-HEIGHT 40)
          (:TEMP-BOX (248 127 45 40 ))
          (:BEHAVIORS NIL))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 262))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 135))
          (:CONSTANT (:KNOWN-AS ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))))
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:TEXT-P T)
          (:STRING "A'")
          (:BOX (262 135 24 25 ))
          (:FRAC-LEFT 262)
          (:FRAC-TOP 135)
          (:FRAC-WIDTH 24)
          (:FRAC-HEIGHT 25)
          (:CURSOR-INDEX NIL)
          (:SAVED-CURSOR-INDEX 2)
          (:TEMP-BOX (262 135 24 25 ))
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (NIL ,GARNETDRAW::MOVING-LINE
          (:CONSTANT (:KNOWN-AS ))
          (:PARTS ((:LINE :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD1 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD2 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 4)))
          (:FILLING-STYLE NIL)
          (:POINTS (238 181 257 161 ))
          (:FRAC-X1 238)
          (:FRAC-Y1 181)
          (:FRAC-X2 257)
          (:FRAC-Y2 161)
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:TEMP-POINTS (238 181 19 -20 )))
        (NIL ,GARNETDRAW::MOVING-LINE
          (:CONSTANT (:KNOWN-AS ))
          (:PARTS ((:LINE :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD1 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) (:ARROWHEAD2 :MODIFY (:FAST-REDRAW-P NIL ) (:DRAW-FUNCTION :COPY ) ) ))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 4)))
          (:FILLING-STYLE NIL)
          (:POINTS (302 181 284 160 ))
          (:FRAC-X1 302)
          (:FRAC-Y1 181)
          (:FRAC-X2 284)
          (:FRAC-Y2 160)
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:TEMP-POINTS (302 181 -18 -21 ))))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:TEMP-POINTS (206 188 -142 -60 ))
      (:FRAC-Y2 128)
      (:FRAC-X2 64)
      (:FRAC-Y1 188)
      (:FRAC-X1 206)
      (:POINTS (206 188 64 128 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:DASH-PATTERN `(10 10 ))
            (:LINE-STYLE :DASH)
            (:LINE-THICKNESS 2)))
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:TEMP-POINTS (295 190 -145 -62 ))
      (:FRAC-Y2 128)
      (:FRAC-X2 150)
      (:FRAC-Y1 190)
      (:FRAC-X1 295)
      (:POINTS (295 190 150 128 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:DASH-PATTERN `(10 10 ))
            (:LINE-STYLE :DASH)
            (:LINE-THICKNESS 2)))
      (:parts (
        (:LINE :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE))
        (:ARROWHEAD2 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:TEMP-POINTS (255 135 -145 -62 ))
      (:FRAC-Y2 73)
      (:FRAC-X2 110)
      (:FRAC-Y1 135)
      (:FRAC-X1 255)
      (:POINTS (255 135 110 73 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:DASH-PATTERN `(10 10 ))
            (:LINE-STYLE :DASH)
            (:LINE-THICKNESS 2)))
      (:parts (
        (:LINE :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE))
        (:ARROWHEAD2 :modify
          (:BEHAVIORS NIL)
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:TEMP-POINTS (146 100 55 -55 ))
      (:FRAC-Y2 45)
      (:FRAC-X2 201)
      (:FRAC-Y1 100)
      (:FRAC-X1 146)
      (:POINTS (146 100 201 45 ))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:DASH-PATTERN `(3 3 ))
            (:LINE-STYLE :DASH)
            (:LINE-THICKNESS 2)))
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)
          (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)))))
    (NIL ,GARNETDRAW::MOVING-AGG
      (:CONSTANT (:KNOWN-AS :COMPONENTS ))
      (:OLD-HEIGHT 40)
      (:OLD-WIDTH 45)
      (:OLD-TOP 10)
      (:OLD-LEFT 197)
      (:parts (
        (NIL ,GARNETDRAW::MOVING-OVAL
          (:CONSTANT (:KNOWN-AS ))
          (:TEMP-BOX (197 10 45 40 ))
          (:FRAC-HEIGHT 40)
          (:FRAC-WIDTH 45)
          (:FRAC-TOP 10)
          (:FRAC-LEFT 197)
          (:BOX (197 10 45 40 ))
          (:DRAW-FUNCTION :COPY)
          (:FILLING-STYLE NIL)
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 1))))
        (NIL ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (FIRST (GVL :BOX)) 213))
          (:TOP ,(o-formula (SECOND (GVL :BOX)) 18))
          (:CONSTANT (:KNOWN-AS ))
          (:TEMP-BOX (213 18 14 25 ))
          (:FRAC-HEIGHT 25)
          (:FRAC-WIDTH 14)
          (:FRAC-TOP 18)
          (:FRAC-LEFT 213)
          (:BOX (213 18 14 25 ))
          (:STRING "D")
          (:TEXT-P T)
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :VERY-LARGE)
                (:FACE :BOLD)))
          (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
                (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                      (:BLUE 0)
                      (:GREEN 0)
                      (:RED 0)))
                (:LINE-THICKNESS 1))))))))))

)

(opal:add-components TOP-AGG INSTANCE)

(opal:update WIN)


