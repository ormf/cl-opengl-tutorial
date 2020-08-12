;;; 
;;; 01-triangles.lisp
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

;;; triangles.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects

(defclass triangles-window (glut:window)
  ((vbuff :accessor vertex-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0)) 
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "01-triangles.lisp"))

(defparameter *triangles-vertex-program*
  "
#version 330

layout( location = 0 ) in vec4 vPosition;

void
main()
{
    gl_Position = vPosition;
}
")

(defparameter *triangles-fragment-program*
  "#version 330

out vec4 fColor;

void main()
{
    fColor = vec4(0.129, 0.337, 0.651, 1.0);
}
")

;;; Initialization 

;;; First, we create buffers for our vertex and index
;;; data. Then, we create the vertex array object that we actually use
;;; for rendering directly. Finally, we load the shader objects.
(defmethod glut:display-window :before ((w triangles-window))
  ;; An array buffer can be used to store verex position, colors,
  ;; normals, or other data. We need to allocate an GL array, copy the
  ;; data to the array, and tell OpenGL that the buffers data comes
  ;; from this GL array. Like most OpenGL state objects, we bind the
  ;; buffer before we can make changes to its state.
  (unless (gl::features-present-p (>= :glsl-version 3.3))
    (glut:destroy-current-window)
    (return-from glut:display-window nil))
  (let ((buffers (gl:gen-buffers 1)))
    (setf (vertex-buffer w) (elt buffers 0)))
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  (let* ((verts #(-0.9 -0.9
                  0.85 -0.9
                  -0.9 0.85
                  0.9 -0.85
                  0.9 0.9
                  -0.85 0.9))
	 (arr (gl:alloc-gl-array :float (length verts))))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array w))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))

  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  
  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))
    (setf (vertex-shader w) vs)
    (setf (fragment-shader w) fs)
    (gl:shader-source vs *triangles-vertex-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *triangles-fragment-program*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    ;; (print (gl:get-shader-info-log vs))
    ;; (print (gl:get-shader-info-log fs))

    (setf (program w) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program w) vs)
    (gl:attach-shader (program w) fs)
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shadersxs together
    ;; to form the program.
    (gl:link-program (program w))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program w))))

(defmethod glut:display ((w triangles-window))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; Since we never use any other program object, this is unnecessary
  ;; in this program. Typically, though, you'll have multiple program
  ;; objects, so you'll need to 'use' each one to activate it.
  (gl:use-program (program w))
  (gl:bind-vertex-array (vertex-array w))
  
  ;; This call actually does the rendering. The vertex data comes from
  ;; the currently-bound VAO. If the input array is null, the indices
  ;; will be taken from the element array buffer bound in the current
  ;; VAO.
;;;  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)
  (gl:draw-arrays :triangles 0 12)
  (glut:swap-buffers))

(defmethod glut:reshape ((w triangles-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let ((right (max (float (/ width height)) 1.0))
	(top (max (float (/ height width)) 1.0)))
    (glu:ortho-2d (- right) right (- top) top))
  (when (program w)
      (let ((proj-mat (gl:get-float :projection-matrix)))
	(gl:uniform-matrix 
	 (gl:get-uniform-location (program w) "projectionMatrix") 
	 4 
	 (vector proj-mat))))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w triangles-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w triangles-window))
  ;; Note: It doesn't matter whether we delete the program or the
  ;; linked shaders first. If a shader is linked to a program, the
  ;; shader isn't destroyed until after the program is
  ;; destroyed. Similarly, if the program is destroyed, the shaders
  ;; are detached.
  (when (slot-boundp w 'vs)
   (gl:delete-shader (vertex-shader w)))
  (when (slot-boundp w 'fs)
    (gl:delete-shader (fragment-shader w)))
  (when (slot-boundp w 'program)
   (gl:delete-program (program w)))

  (when (slot-boundp w 'vbuff)
    (gl:delete-buffers (list (vertex-buffer w))))
  (when (slot-boundp w 'va)
   (gl:delete-vertex-arrays (list (vertex-array w)))))

(defun 01-triangles ()
  (let ((w (make-instance 'triangles-window)))
    (unwind-protect
         (glut:display-window w)
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))

;;; (01-triangles)
;;; (gl:named-buffer-storage)

