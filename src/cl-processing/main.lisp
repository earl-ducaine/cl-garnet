(in-package :kr)

(defun create-processing-window ()
  ;; Create a small window at the upper left corner of the screen
  (create-instance 'win inter:interactor-window
    (:left 10)
    (:top 10)
    (:width 200)
    (:height 50))

  ;; create an aggregate for the window
  (s-value win :aggregate (create-instance 'agg opal:aggregate))

  ;; create the string
  (create-instance 'hello opal:text
    (:left 10)
    (:top 20)
    (:background-color opal:black)
    (:foreground-color opal:black)
    (:string "hello world"))

  ;; add the string to the aggregate
  (opal:add-component agg hello) 
  ;; Cause the window and string to be displayed
  (opal:update win) 
  )






(defun modify-processing-window ()
  ;; Opal also strives to make it easy to change the picture.  To change
  ;; the x position of the rectangle only requires setting the value of
  ;; the :left slot;  Opal handles the refresh:

  ;; change the position
  (s-value HELLO :left 50)  

  ;; cause the change to be visible
  (opal:update WIN)

  (create-instance 'opal:line-style opal:graphic-quality
		  (:background-color opal:black)
		  (:forground-color opal:black))


  
  )
