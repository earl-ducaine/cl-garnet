;;;; package.lisp

(defpackage #:clx-freetype2-renderer
  (:nicknames #:clx-ft2)
  (:use #:cl)
  (:export
   :screen-dpi
   :set-face-size
   :font
   :font-family
   :font-style
   :font-size
   :font-underline
   :font-strikethrough
   :font-overline
   :font-equal
   :get-font-families
   :get-font-styles
   :cache-fonts
   :draw-glyphs
   :font-ascent
   :font-descent
   :font-height
   :close-font
   :open-font
   :*font-cache*
   :text-width))

