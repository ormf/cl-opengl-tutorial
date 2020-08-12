(in-package :cl-opengl-tutorial)

(defclass my-intro-window (glut:window)
  ()
  (:default-initargs :width 400 :height 300
                     :title "My Window Title"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((win my-intro-window))
  (gl:shade-model :smooth)        ; enables smooth shading
  (gl:clear-color 0 0 0 0)        ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:enable :depth-test)         ; enable depth testing
  (gl:depth-func :lequal)         ; okay to write pixel if its depth
                                  ; is less-than-or-equal to the
                                  ; depth currently written
                                  ; really nice perspective correction
  (gl:hint :perspective-correction-hint :nicest)
)

(defmethod glut:display ((win my-intro-window))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (glut:swap-buffers))

(defmethod glut:reshape ((win my-intro-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity)              ; reset the matrix
)

(defun intro ()
  (glut:display-window (make-instance 'my-intro-window)))

;;; (intro)

