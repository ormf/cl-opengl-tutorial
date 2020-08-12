;;; 
;;; tut03.lisp
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

(defclass my-tut03-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p))
  (:default-initargs :width 400 :height 300
                     :title "tut03: color"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil))

(defmethod glut:display-window :before ((win my-tut03-window))
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
    (glut:full-screen)))          ; if so, then tell GLUT

(defmethod glut:close ((win my-tut03-window)))

(defmethod glut:display ((win my-tut03-window))
                                  ; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)              ; reset the modelview matrix
  (gl:translate -1.5 0.0 -6.0)    ; translate left and into the screen
  (gl:with-primitives :triangles  ; start drawing triangles
    (gl:color 1.0 0.0 0.0)        ; set the color to red
    (gl:vertex 0.0 1.0 0.0)       ; top vertex
    (gl:color 0.0 1.0 0.0)        ; set the color to green
    (gl:vertex -1.0 -1.0 0.0)     ; bottom-left vertex
    (gl:color 0.0 0.0 1.0)        ; set the color to blue
    (gl:vertex 1.0 -1.0 0.0))      ; bottom-right vertex
  (gl:translate 3.0 0.0 0.0)      ; translate right
  (gl:color 0.5 0.5 1.0)          ; set the color to light blue
  (gl:with-primitives :quads      ; start drawing quadrilaterals
    (gl:vertex -1.0  1.0  0.0)    ; top-left vertex
    (gl:vertex  1.0  1.0  0.0)    ; top-right vertex
    (gl:vertex  1.0 -1.0  0.0)    ; bottom-right vertex
    (gl:vertex -1.0 -1.0  0.0))   ; bottom-left vertex          ; do anything specific to this tutorial
  (glut:swap-buffers))            ; swap the buffer onto the screen

(defmethod glut:reshape ((win my-tut03-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity))              ; reset the matrix

(defmethod glut:keyboard ((win my-tut03-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                      ; when we get an 'f'
                                    ; save whether we're in fullscreen
         (let ((full (fullscreen-p win)))
           (glut:close win)         ; close the current window
           (glut:display-window     ; open a new window with fullscreen toggled
               (make-instance 'my-tut03-window
                              :fullscreen (not full)))))))

(defmethod glut:keyboard-up ((win my-tut03-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)))

(defun tut03 ()
  (glut:display-window (make-instance 'my-tut03-window)))

;;; (tut03)

