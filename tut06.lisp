;;; 
;;; tut06.lisp
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

(defun load-png ( filename &optional (texture-id (car (gl:gen-textures 1))
                                                 texture-id-p) )
  (flet ((load-and-decode (filename)
           (with-open-file (in filename
                               :element-type '(unsigned-byte 8))
             (png:decode in))))
    (handler-case
        (let ((png (load-and-decode filename)))
          (assert png)          ; make sure we got the png
          (gl:bind-texture :texture-2d texture-id)
          (let ((ww (png:image-width png))
                (hh (png:image-height png))
                (cc (png:image-channels png)))
            (let ((data (make-array (list (* ww hh cc))
                                    :element-type (array-element-type png)
                                    :displaced-to png)))
              (let ((level-of-detail 0)
                    (internal-format (ecase (png:image-bit-depth png)
                                       (8  (ecase cc
                                             (1 :luminance8)
                                             (2 :luminance8-alpha8)
                                             (3 :rgb8)
                                             (4 :rgba8)))
                                       (16 (ecase cc
                                             (1 :luminance16)
                                             (2 :luminance16-alpha16)
                                             (3 :rgb16)
                                             (4 :rgba16)))))
                    (border 0)
                    (format (ecase cc
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba)))
                    (data-type (ecase (png:image-bit-depth png)
                                 (8  :unsigned-byte)
                                 (16 :unsigned-short))))
                (gl:tex-image-2d :texture-2d
                                 level-of-detail
                                 internal-format
                                 ww
                                 hh
                                 border
                                 format
                                 data-type
                                 data))
              (gl:tex-parameter :texture-2d :texture-min-filter :linear)
              (gl:tex-parameter :texture-2d :texture-mag-filter :linear)))
          texture-id)           ; return the texture-id on success

        (error ()
               (unless texture-id-p
                 (gl:delete-textures (list texture-id)))
               nil))))

(defclass rotation-state ()
  ((x-angle :initarg :x-angle :reader x-angle)
   (y-angle :initarg :y-angle :reader y-angle)
   (z-angle :initarg :z-angle :reader z-angle))
  (:default-initargs :x-angle 0.0
                     :y-angle 0.0
                     :z-angle 0.0))
(declaim (inline cube-face))
(defun cube-face (left up forw)
  (gl:tex-coord 0.0 1.0)        ; bottom-left
  (gl:vertex (+ (- (elt left 0)) (- (elt up 0)) (elt forw 0))
             (+ (- (elt left 1)) (- (elt up 1)) (elt forw 1))
             (+ (- (elt left 2)) (- (elt up 2)) (elt forw 2)))

  (gl:tex-coord 1.0 1.0)        ; bottom-right
  (gl:vertex (+ (+ (elt left 0)) (- (elt up 0)) (elt forw 0))
             (+ (+ (elt left 1)) (- (elt up 1)) (elt forw 1))
             (+ (+ (elt left 2)) (- (elt up 2)) (elt forw 2)))

  (gl:tex-coord 1.0 0.0)        ; top-right
  (gl:vertex (+ (+ (elt left 0)) (+ (elt up 0)) (elt forw 0))
             (+ (+ (elt left 1)) (+ (elt up 1)) (elt forw 1))
             (+ (+ (elt left 2)) (+ (elt up 2)) (elt forw 2)))

  (gl:tex-coord 0.0 0.0)        ; top-left
  (gl:vertex (+ (- (elt left 0)) (+ (elt up 0)) (elt forw 0))
             (+ (- (elt left 1)) (+ (elt up 1)) (elt forw 1))
             (+ (- (elt left 2)) (+ (elt up 2)) (elt forw 2))))

(defclass my-tut06-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (texture-id :initform nil :accessor texture-id)
   (rotation-state :initarg :rotation-state :accessor rotation-state)
  )
  (:default-initargs :width 400 :height 300
                     :title "tut06: UV-textured objects"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
                     :rotation-state (make-instance 'rotation-state)
                     :tick-interval (round 1000 60)  ; milliseconds per tick
   ))

(defmethod glut:close ((win my-tut06-window)))

(defmethod glut:tick ((win my-tut06-window))
                                ; retrieve the current rotation
  (let* ((cur (rotation-state win))
                                ; retrieve the current angles
         (x-angle (x-angle cur))
         (y-angle (y-angle cur))
         (z-angle (z-angle cur)))

    (setf (rotation-state win)  ; replace the rotation state
          (make-instance 'rotation-state
                         :x-angle (+ x-angle 0.3)
                         :y-angle (+ y-angle 0.2)
                         :z-angle (+ z-angle 0.4))))

  (glut:post-redisplay))        ; tell GLUT to redraw

(defmethod glut:display-window :before ((win my-tut06-window))
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

  (unless (texture-id win)     ; load texture if needed
    (setf (texture-id win)
          (load-png #P"./images/cube-texture.png")))
  (when (texture-id win)       ; enable texturing if we have one
    (gl:enable :texture-2d)))

(defmethod glut:display ((win my-tut06-window))
                                  ; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)              ; reset the modelview matrix
  (let* ((cur (rotation-state win))
         (x-angle (x-angle cur))
         (y-angle (y-angle cur))
         (z-angle (z-angle cur)))

    (gl:translate 0.0 0.0 -5.0)   ; move and rotate
    (gl:rotate x-angle 1.0 0.0 0.0)
    (gl:rotate y-angle 0.0 1.0 0.0)
    (gl:rotate z-angle 0.0 0.0 1.0)

    (when (texture-id win)          ; bind the texture if we have it
      (gl:bind-texture :texture-2d (texture-id win)))
    (gl:with-primitives :quads
      ;; front face
      (gl:tex-coord 0.0 1.0) (gl:vertex -1.0 -1.0  1.0)
      (gl:tex-coord 1.0 1.0) (gl:vertex  1.0 -1.0  1.0)
      (gl:tex-coord 1.0 0.0) (gl:vertex  1.0  1.0  1.0)
      (gl:tex-coord 0.0 0.0) (gl:vertex -1.0  1.0  1.0)
      ;; back face
      (cube-face #(1.0 0.0 0.0)  #(0.0 -1.0  0.0) #(0.0 0.0 -1.0))
      ;; top face
      (cube-face #(1.0 0.0 0.0)  #(0.0  0.0 -1.0) #(0.0 1.0 0.0))
      ;; bottom face
      (cube-face #(1.0 0.0 0.0)  #(0.0  0.0  1.0) #(0.0 -1.0 0.0))
      ;; right face
      (cube-face #(0.0 0.0 -1.0) #(0.0  1.0  0.0) #(1.0 0.0 0.0))
      ;; left face
      (cube-face #(0.0 0.0  1.0) #(0.0  1.0  0.0) #(-1.0 0.0 0.0))))       ; draw the cube          ; do anything specific to this tutorial
  (glut:swap-buffers)             ; swap the buffer onto the screen
  )

(defmethod glut:reshape ((win my-tut06-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity)              ; reset the matrix
)

(defmethod glut:keyboard ((win my-tut06-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                  ; when we get an 'f'
                                ; save whether we're in fullscreen
       (let ((full (fullscreen-p win)))
         (glut:close win)       ; close the current window
         (glut:display-window   ; open a new window with fullscreen toggled
             (make-instance 'my-tut06-window
                            :fullscreen (not full)))))))

(defmethod glut:keyboard-up ((win my-tut06-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)))

(defun tut06 ()
  (glut:display-window (make-instance 'my-tut06-window)))

;;; (tut06)
