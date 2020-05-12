(setf garnetdraw::*DRAW-AGG*
(create-instance 'SCHEMATA OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 680))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 410))
  (:CONSTANT `(:COMPONENTS ))
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 729)
  (:WINDOW-HEIGHT 355)
  (:PACKAGE-NAME "USER")
  (:WINDOW-TITLE "SCHEMATA FIGURE")
  (:EXPORT-P T)
  (:FUNCTION-FOR-OK NIL)
  (:parts `(
    (NIL ,OPAL:CIRCLE
      (:LEFT 435)
      (:TOP 270)
      (:WIDTH 50)
      (:HEIGHT 50)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:BEHAVIORS NIL)
      (:BOX (435 270 50 50 ))
      (:GROW-P T))
    (NIL ,OPAL:CIRCLE
      (:LEFT 97)
      (:TOP 270)
      (:WIDTH 50)
      (:HEIGHT 50)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-RECTANGLE")
      (:BEHAVIORS NIL)
      (:BOX (97 270 50 50 ))
      (:GROW-P T))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 15)
      (:TOP 288)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (15 288 3 3 ))
      (:STRING "RECTANGLE-1"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 497)
      (:TOP 266)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (497 266 190 56 ))
      (:STRING ":is-a BOX-OBJECT
:x 34
:y (+ (gvl :left-obj :y) 15)
:left-obj RECTANGLE-1"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 160)
      (:TOP 273)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (160 273 109 42 ))
      (:STRING ":is-a BOX-OBJECT
:x 10
:y 20"))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 350)
      (:TOP 287)
      (:VISIBLE T)
      (:CONSTANT (:KNOWN-AS T ))
      (:GILT-REF "TYPE-TEXT")
      (:BEHAVIORS NIL)
      (:BOX (350 287 76 14 ))
      (:STRING "RECTANGLE-2"))
    (NIL ,GARNETDRAW::MOVING-AGG
      (:LEFT 131)
      (:TOP 22)
      (:WIDTH 471)
      (:HEIGHT 50)
      (:CONSTANT (:KNOWN-AS :COMPONENTS ))
      (:OLD-LEFT 131)
      (:OLD-TOP 22)
      (:OLD-WIDTH 471)
      (:OLD-HEIGHT 50)
      (:BOX (130 22 471 50 ))
      (:parts (
        (NIL ,OPAL:CIRCLE
          (:LEFT 274)
          (:TOP 22)
          (:WIDTH 50)
          (:HEIGHT 50)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-RECTANGLE")
          (:BEHAVIORS NIL)
          (:BOX (274 22 50 50 ))
          (:GROW-P T)
          (:FRAC-LEFT 274)
          (:FRAC-TOP 22)
          (:FRAC-WIDTH 50)
          (:FRAC-HEIGHT 50)
          (:TEMP-BOX (274 22 50 50 )))
        (NIL ,OPAL:MULTI-TEXT
          (:LEFT 332)
          (:TOP 26)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-TEXT")
          (:BEHAVIORS NIL)
          (:BOX (332 26 270 42 ))
          (:STRING ":color :blue
:update-demon 'Graphical-Object-Changed
:is-a-inv BOX-OBJECT")
          (:FRAC-LEFT 332)
          (:FRAC-TOP 26)
          (:FRAC-WIDTH 270)
          (:FRAC-HEIGHT 42)
          (:TEMP-BOX (332 26 270 42 )))
        (NIL ,OPAL:MULTI-TEXT
          (:LEFT 131)
          (:TOP 40)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-TEXT")
          (:BEHAVIORS NIL)
          (:BOX (131 40 133 14 ))
          (:STRING "MY-GRAPHICAL-OBJECT")
          (:FRAC-LEFT 131)
          (:FRAC-TOP 40)
          (:FRAC-WIDTH 133)
          (:FRAC-HEIGHT 14)
          (:TEMP-BOX (131 40 133 14 ))))))
    (NIL ,GARNETDRAW::MOVING-AGG
      (:LEFT 193)
      (:TOP 150)
      (:WIDTH 367)
      (:HEIGHT 50)
      (:CONSTANT (:KNOWN-AS :COMPONENTS ))
      (:OLD-LEFT 193)
      (:OLD-TOP 150)
      (:OLD-WIDTH 367)
      (:OLD-HEIGHT 50)
      (:BOX (194 150 367 50 ))
      (:parts (
        (NIL ,OPAL:CIRCLE
          (:LEFT 274)
          (:TOP 150)
          (:WIDTH 50)
          (:HEIGHT 50)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-RECTANGLE")
          (:BEHAVIORS NIL)
          (:BOX (274 150 50 50 ))
          (:GROW-P T)
          (:FRAC-LEFT 273)
          (:FRAC-TOP 150)
          (:FRAC-WIDTH 50)
          (:FRAC-HEIGHT 50)
          (:TEMP-BOX (273 150 50 50 )))
        (NIL ,OPAL:MULTI-TEXT
          (:LEFT 333)
          (:TOP 154)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-TEXT")
          (:BEHAVIORS NIL)
          (:BOX (333 154 227 42 ))
          (:STRING ":is-a GRAPHICAL-OBJECT
:thickness 1
:is-a-inv RECTANGLE-1 RECTANGLE-2")
          (:FRAC-LEFT 333)
          (:FRAC-TOP 154)
          (:FRAC-WIDTH 227)
          (:FRAC-HEIGHT 42)
          (:TEMP-BOX (333 154 227 42 )))
        (NIL ,OPAL:MULTI-TEXT
          (:LEFT 193)
          (:TOP 168)
          (:VISIBLE T)
          (:CONSTANT (:KNOWN-AS T ))
          (:GILT-REF "TYPE-TEXT")
          (:BEHAVIORS NIL)
          (:BOX (193 168 70 14 ))
          (:STRING "BOX-OBJECT")
          (:FRAC-LEFT 193)
          (:FRAC-TOP 168)
          (:FRAC-WIDTH 70)
          (:FRAC-HEIGHT 14)
          (:TEMP-BOX (193 168 70 14 ))))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (142 264 268 196 ))
      (:FRAC-X1 142)
      (:FRAC-Y1 264)
      (:FRAC-X2 268)
      (:FRAC-Y2 196)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (436 266 325 201 ))
      (:FRAC-X1 436)
      (:FRAC-Y1 266)
      (:FRAC-X2 325)
      (:FRAC-Y2 201)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (299 139 299 78 ))
      (:FRAC-X1 299)
      (:FRAC-Y1 139)
      (:FRAC-X2 299)
      (:FRAC-Y2 78)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (423 68 323 144 ))
      (:FRAC-X1 423)
      (:FRAC-Y1 68)
      (:FRAC-X2 323)
      (:FRAC-Y2 144)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,OPAL:ARC
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 135))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 300))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 450))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 50))
      (:CONSTANT (:KNOWN-AS ))
      (:FRAC-HEIGHT 50)
      (:FRAC-WIDTH 450)
      (:FRAC-TOP 300)
      (:FRAC-LEFT 135)
      (:BOX (135 300 450 50 ))
      (:ANGLE1 3.141592653589793d0)
      (:ANGLE2 3.141592653589793d0))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:CONSTANT (:KNOWN-AS :COMPONENTS :ARROWHEAD2 :ARROWHEAD1 :LINE ))
      (:FRAC-Y2 322)
      (:FRAC-X2 131)
      (:FRAC-Y1 330)
      (:FRAC-X1 139)
      (:POINTS (139 330 131 322 ))
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
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (429 196 204 261 ))
      (:FRAC-X1 429)
      (:FRAC-Y1 196)
      (:FRAC-X2 204)
      (:FRAC-Y2 261)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY)))))
    (NIL ,GARNETDRAW::MOVING-ARROWLINE
      (:LINE-STYLE ,(create-instance nil OPAL:LINE-STYLE
            (:FOREGROUND-COLOR (create-instance nil OPAL:COLOR
                  (:BLUE 0)
                  (:GREEN 0)
                  (:RED 0)))))
      (:FILLING-STYLE ,(opal:halftone 100))
      (:POINTS (507 196 471 262 ))
      (:FRAC-X1 507)
      (:FRAC-Y1 196)
      (:FRAC-X2 471)
      (:FRAC-Y2 262)
      (:parts (
        (:LINE :modify
          (:DRAW-FUNCTION :COPY))
        (:ARROWHEAD1 :modify
          (:DRAW-FUNCTION :COPY)
          (:RADIUS 5))
        (:ARROWHEAD2 :modify
          (:DRAW-FUNCTION :COPY))))))))

)
