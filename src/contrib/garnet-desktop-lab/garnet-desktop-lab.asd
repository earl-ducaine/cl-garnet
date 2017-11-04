
(asdf:defsystem :garnet-desktop-lab
  ;; sb-posix means that currently only sbcl can load Garnet.
  :depends-on (:xoanon.gui.garnet)
  :components
  ((:file "package")
   (:file "garnet-desktop-lab")
   (:file "xomax")
   (:file "app-launcher")))
