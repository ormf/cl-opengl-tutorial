;;; 
;;; 02-circles.lisp
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

(defconstant +float-size+ 4)

;;; triangle.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects

(defclass circle-window (glut:window)
  ((circle-vbo :accessor circle-vbo)
   (arrowhead-vbo :accessor arrowhead-vbo)
   (arrowstem-vbo :accessor arrowstem-vbo)
   (offsets :accessor offset-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (circle-vao :accessor circle-vao)
   (arrow-vao :accessor arrow-vao)
   (circle-program :accessor circle-program)
   (arrow-program :accessor arrow-program)
   (angle :accessor angle :initform 0.0)) 
  (:default-initargs :width 400 :height 400 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "02-circles.lisp"))

(defparameter *circle-vertex-shader-program*
  "
#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec4 aTransform;

void
main()
{
    gl_Position = vec4((aTransform.z*aPos)+aTransform.xy, 0.0, 1.0);
}
")

(defparameter *arrow-vertex-shader-program*
  "
#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec4 aTransform;

mat2 rotate2d(float _angle){
    return mat2(cos(_angle),-sin(_angle),
                sin(_angle),cos(_angle));
}

void
main()
{
    gl_Position = vec4((aTransform.z*aPos*rotate2d(aTransform.w))+aTransform.xy,
                        0.0, 1.0);
}
")

(defparameter *fragment-shader-program*
  "#version 330 core

out vec4 fColor;

void main()
{
    fColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}
")

(defun get-circle-verts (num radius)
  (apply #'vector ;;; circle around #(0 0)
         (loop for (pt1 pt2) on (loop for x below (1+ num)
                                      for theta = (mod (* 2 pi (/ x num)) num)
                                      collect `(,(float (* radius (cos theta)) 1.0)
                                                ,(float (* radius (sin theta)) 1.0)))
               while pt2
               append (append pt1 pt2))))



;;; Initialization 

;;; First, we create buffers for our vertex and index
;;; data. Then, we create the vertex array object that we actually use
;;; for rendering directly. Finally, we load the shader objects.
(defmethod glut:display-window :before ((w circle-window))
  ;; An array buffer can be used to store verex position, colors,
  ;; normals, or other data. We need to allocate an GL array, copy the
  ;; data to the array, and tell OpenGL that the buffers data comes
  ;; from this GL array. Like most OpenGL state objects, we bind the
  ;; buffer before we can make changes to its state.
  (unless (gl::features-present-p (>= :glsl-version 3.3))
    (glut:destroy-current-window)
    (return-from glut:display-window nil))
  (let ((buffers (gl:gen-buffers 2)))
    (setf (circle-vbo w) (elt buffers 0))
    (setf (offset-buffer w) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (circle-vbo w))
  (let* ((verts (get-circle-verts 64 0.09))
	 (arr (gl:alloc-gl-array :float (length verts))))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  (gl:bind-buffer :array-buffer (offset-buffer w))
  (let* ((offsets (loop for i below 512 append (list (- (* 0.2 (mod i 10)) 0.9)
                                                     (- (mod (* 0.2 (floor i 10)) 10) 0.9)
                                                     (+ 0.1 (random 0.7))
                                                     0.0)))
	 (arr (gl:alloc-gl-array :float (length offsets))))
    (loop
      for offset in offsets for i from 0
      do (setf (gl:glaref arr i) offset))
    (gl:buffer-data :array-buffer :dynamic-draw arr)
    (gl:free-gl-array arr))

  
  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (circle-vao w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (circle-vao w))

  ;; To associate our CIRCLE-VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (circle-vbo w))
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer))

  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer 0)

  (gl:bind-buffer :array-buffer (offset-buffer w))
  (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer 0)

;;  (cl-opengl-bindings:vertex-attrib-divisor 0 1)
  (cl-opengl-bindings:vertex-attrib-divisor 1 1)

  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))
    (setf (vertex-shader w) vs)
    (setf (fragment-shader w) fs)
    (gl:shader-source vs *arrow-vertex-shader-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *fragment-shader-program*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    ;; (print (gl:get-shader-info-log vs))
    ;; (print (gl:get-shader-info-log fs))

    (setf (circle-program w) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (circle-program w) vs)
    (gl:attach-shader (circle-program w) fs)
    ;; Don't forget to link the circle-program after attaching the
    ;; shaders. This step actually puts the attached shadersxs together
    ;; to form the circle-program.
    (gl:link-program (circle-program w))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (when (slot-boundp w 'vs)
      (gl:delete-shader (vertex-shader w)))
    (when (slot-boundp w 'fs)
      (gl:delete-shader (fragment-shader w)))
    (gl:use-program (circle-program w))))


(defmethod glut:idle ((w circle-window))
  (glut:post-redisplay))

(defmethod glut:display ((w circle-window))
  (update-swank)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-slots (angle circle-program circle-vao offsets) w
    (continuable
      (incf angle 0.001)
      (gl:clear-color 0 0 0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      ;; Since we never use any other program object, this is unnecessary
      ;; in this program. Typically, though, you'll have multiple program
      ;; objects, so you'll need to 'use' each one to activate it.
      (gl:line-width 2)
      (gl:bind-buffer :array-buffer offsets)          
      (gl:with-mapped-buffer (p1 :array-buffer :read-write)
        (loop
          for i below (* 4 512) by 4
          do (setf (cffi:mem-aref p1 :float (+ i 3)) (* angle (if (> i 256) (- i 512) i)))))
      (gl:bind-buffer :array-buffer 0)
      (gl:use-program circle-program)
      (gl:bind-vertex-array circle-vao)
      
      ;; This call actually does the rendering. The vertex data comes from
      ;; the currently-bound VAO. If the input array is null, the indices
      ;; will be taken from the element array buffer bound in the current
      ;; VAO.
;;;  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)
      (gl:draw-arrays-instanced :lines 0 64 512)))
  (glut:swap-buffers))

(defmethod glut:reshape ((w circle-window) width height)
  (gl:viewport 0 0 (min width height) (min width height))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let ((right (max (float (/ width height)) 1.0))
	(top (max (float (/ height width)) 1.0)))
    (glu:ortho-2d (- right) right (- top) top))
  
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w circle-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w circle-window))
  ;; Note: It doesn't matter whether we delete the program or the
  ;; linked shaders first. If a shader is linked to a program, the
  ;; shader isn't destroyed until after the program is
  ;; destroyed. Similarly, if the program is destroyed, the shaders
  ;; are detached.
  ;; (when (slot-boundp w 'vs)
  ;;  (gl:delete-shader (vertex-shader w)))
  ;; (when (slot-boundp w 'fs)
  ;;   (gl:delete-shader (fragment-shader w)))
  (when (slot-boundp w 'circle-program)
   (gl:delete-program (circle-program w))))

(defun 02-circles ()
  (let ((w (make-instance 'circle-window)))
    (unwind-protect
         (continuable
           (glut:display-window w))
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))

;;; (02-circles)
;;; (gl:named-buffer-storage)

