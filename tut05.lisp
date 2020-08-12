;;; 
;;; tut05.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2020 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-opengl-tutorial)

(defclass rotation-state ()
  ((pyramid-angle :initarg :pyramid-angle :reader pyramid-angle)
   (cube-angle :initarg :cube-angle :reader cube-angle))
  (:default-initargs :pyramid-angle 0.0
                     :cube-angle 0.0))

(defclass my-tut05-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (rotation-state :initarg :rotation-state :accessor rotation-state))
  (:default-initargs :width 400 :height 300
                     :title "tut05: solid shapes"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
                     :rotation-state (make-instance 'rotation-state)
                     :tick-interval (round 1000 60)))  ; milliseconds per tick

(defmethod glut:close ((win my-tut05-window)))

(defmethod glut:tick ((win my-tut05-window))
                                ; retrieve the current rotation
  (let* ((cur (rotation-state win))
                                ; retrieve the current angles
         (tri (pyramid-angle cur))
         (cube (cube-angle cur)))

    (setf (rotation-state win)  ; replace the rotation state
          (make-instance 'rotation-state
                         :pyramid-angle (+ tri 0.2)
                         :cube-angle (+ cube 0.15))))

  (glut:post-redisplay))        ; tell GLUT to redraw

(defmethod glut:display-window :before ((win my-tut05-window))
  (gl:shade-model :smooth)        ; enables smooth shading
  (gl:clear-color 0 0 0 0)        ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:enable :depth-test)         ; enable depth testing
  (gl:depth-func :lequal)         ; okay to write pixel if its depth
                                  ; is less-than-or-equal to the
                                  ; depth currently written
                                  ; really nice perspective correction
  (gl:hint :perspective-correction-hint :nicest)

  (when (fullscreen-p win)        ; check to see if fullscreen needed
    (glut:full-screen))           ; if so, then tell GLUT
)

(defmethod glut:display ((win my-tut05-window))
                                  ; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)              ; reset the modelview matrix
  (let* ((cur (rotation-state win))
         (pyramid-angle (pyramid-angle cur))
         (cube-angle (cube-angle cur)))
    (gl:with-pushed-matrix
      (gl:translate -1.5 0.0 -6.0)          ; position pyramid
      (gl:rotate pyramid-angle 0.0 1.0 0.0) ;;; rotate pyramid
      (gl:with-primitives :triangles    ; start drawing the pyramid
        (gl:color 1.0 0.0 0.0)          ; set the color to red
        (gl:vertex 0.0 1.0 0.0)         ; top vertex (front)
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex -1.0 -1.0 1.0)       ; bottom-left vertex (front)
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex 1.0 -1.0 1.0)        ; bottom-right vertex (front)
        (gl:color 1.0 0.0 0.0)          ; set the color to red
        (gl:vertex 0.0 1.0 0.0)         ; top vertex (right)
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex 1.0 -1.0 1.0)        ; bottom-left vertex (right)
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex 1.0 -1.0 -1.0)       ; bottom-left vertex (right)
        (gl:color 1.0 0.0 0.0)          ; set the color to red
        (gl:vertex 0.0 1.0 0.0)         ; top vertex (back)
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex 1.0 -1.0 -1.0)       ; bottom-left vertex (back)
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex -1.0 -1.0 -1.0)      ; bottom-left vertex (back)
        (gl:color 1.0 0.0 0.0)          ; set the color to red
        (gl:vertex 0.0 1.0 0.0)         ; top vertex (left)
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex -1.0 -1.0 -1.0)      ; bottom-left vertex (left)
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex -1.0 -1.0 1.0)       ; bottom-left vertex (left)
        )
      (gl:with-primitives :quads        ; bottom of the pyramid
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex 1.0 -1.0 1.0)        ; front-right corner
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex 1.0 -1.0 -1.0)       ; back-right corner
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex -1.0 -1.0 -1.0)      ; back-left corner
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex -1.0 -1.0 1.0))      ; front-left corner
      )
    (gl:with-pushed-matrix
      (gl:translate 1.5 0.0 -6.0)   ; translate right and into the screen
      (gl:rotate cube-angle 1.0 0.0 0.0)
      (gl:color 0.5 0.5 1.0)            ; set the color to light blue
      (gl:with-primitives :quads        ; start drawing cube
        (gl:color 0.0 1.0 0.0)          ; set the color to green
        (gl:vertex  1.0  1.0 -1.0)      ; right top back
        (gl:vertex -1.0  1.0 -1.0)      ; left top back
        (gl:vertex -1.0  1.0  1.0)      ; left top front
        (gl:vertex  1.0  1.0  1.0)      ; right top front
        (gl:color 1.0 0.5 0.0)          ; set the color to orange
        (gl:vertex  1.0 -1.0  1.0)      ; right bottom front
        (gl:vertex -1.0 -1.0  1.0)      ; left bottom front
        (gl:vertex -1.0 -1.0 -1.0)      ; left bottom back
        (gl:vertex  1.0 -1.0 -1.0)      ; right bottom back
        (gl:color 1.0 0.0 0.0)          ; set the color to red
        (gl:vertex  1.0  1.0  1.0)      ; right top front
        (gl:vertex -1.0  1.0  1.0)      ; left top front
        (gl:vertex -1.0 -1.0  1.0)      ; left bottom front
        (gl:vertex  1.0 -1.0  1.0)      ; right bottom front
        (gl:color 1.0 1.0 0.0)          ; set the color to yellow
        (gl:vertex  1.0 -1.0 -1.0)      ; right bottom back
        (gl:vertex -1.0 -1.0 -1.0)      ; left bottom back
        (gl:vertex -1.0  1.0 -1.0)      ; left top back
        (gl:vertex  1.0  1.0 -1.0)      ; right top back
        (gl:color 0.0 0.0 1.0)          ; set the color to blue
        (gl:vertex -1.0  1.0  1.0)      ; left top front
        (gl:vertex -1.0  1.0 -1.0)      ; left top back
        (gl:vertex -1.0 -1.0 -1.0)      ; left bottom back
        (gl:vertex -1.0 -1.0  1.0)      ; left bottom front
        (gl:color 1.0 0.0 1.0)          ; set the color to magenta
        (gl:vertex  1.0  1.0 -1.0)      ; right top back
        (gl:vertex  1.0  1.0  1.0)      ; right top front
        (gl:vertex  1.0 -1.0  1.0)      ; right bottom front
        (gl:vertex  1.0 -1.0 -1.0)      ; right bottom back
        )))
  (glut:swap-buffers)             ; swap the buffer onto the screen
  )

(defmethod glut:reshape ((win my-tut05-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity)              ; reset the matrix

   
)
(defmethod glut:keyboard ((win my-tut05-window) key xx yy)
  (declare (ignore xx yy))
  (case key
;;;    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                      ; when we get an 'f'
                                    ; save whether we're in fullscreen
         (let ((full (fullscreen-p win)))
           (glut:close win)         ; close the current window
           (glut:display-window     ; open a new window with fullscreen toggled
               (make-instance 'my-tut05-window
                              :fullscreen (not full)))))
  ))
(defmethod glut:keyboard-up ((win my-tut05-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)
    
  ))

(defun tut05 ()
  (glut:display-window (make-instance 'my-tut05-window)))

;;; (tut05)

