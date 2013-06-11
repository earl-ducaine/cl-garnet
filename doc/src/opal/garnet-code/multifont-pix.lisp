(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 800)(:top 10)(:width 230)(:height 220)
   (:background-color opal:motif-gray))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))
(opal:update WIN)

(defvar italic (opal:get-standard-font :fixed :italic :medium))
(defvar bold (opal:get-standard-font :fixed :bold :medium))
(create-instance 'MY-CIRCLE opal:circle)

(create-instance 'T1 opal:multifont-text
  (:left 20) (:top 20)
  (:initial-text `("An example string" "with multiple lines")))

(create-instance 'T2 opal:multifont-text
  (:left 20) (:top 60)
  (:initial-text `((("An example string" . ,ITALIC))
		   (("with multiple lines" . ,ITALIC)))))

(create-instance 'T3 opal:multifont-text
  (:left 20) (:top 100)
  (:initial-text `(("Here " ("is" . ,ITALIC) " my " ("example" . ,BOLD) " string."))))

(create-instance 'T4 opal:multifont-text
  (:left 20) (:top 130)
  (:initial-text `(("Here is a circle:" ,MY-CIRCLE))))

(create-instance 'T5 opal:multifont-text
  (:left 20) (:top 160)
  (:initial-text `(("Here is " ("yellow" ,BOLD ,opal:yellow) " and "
		    ("red" ,BOLD ,opal:red) " text"))))

(create-instance 'T6 opal:multifont-text
  (:left 20) (:top 190)
  (:initial-text `(("The " #+comment(:mark NIL) "(parentheses)" #+comment(:mark T) " are marked")))
  (:show-marks T))

(opal:add-components TOP-AGG T1 T2 T3 T4 T5 T6)
(opal:update WIN)
