;;; 
;;; scratch.lisp
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

(defmethod draw-object ((o click-particles))
  (declare (optimize speed))
  (flet ((rectangle (x y width height
                       &optional (u1 0.0) (v1 0.0) (u2 1.0) (v2 1.0))
           (let* ((w/2 (/ width 2.0))
                  (h/2 (/ height 2.0))
                  (x1 (- x w/2))
                  (x2 (+ x w/2))
                  (y1 (- y h/2))
                  (y2 (+ y h/2)))
             (gl:tex-coord u1 v2)
             (gl:vertex x1 y1 0.0)
             (gl:tex-coord u2 v2)
             (gl:vertex x2 y1 0.0)
             (gl:tex-coord u2 v1)
             (gl:vertex x2 y2 0.0)
             (gl:tex-coord u1 v1)
             (gl:vertex x1 y2 0.0))))
    (bind-texture :click-particle)
    (gl:with-primitive :quads
      ;; SBCL is smart, so declaring the type of this array takes care
      ;; of this function
      (loop with positions of-type (simple-array single-float (*)) = (positions o)
         for i below (length positions) by 2
         for x = (aref positions i)
         for y = (aref positions (1+ i))
         do (rectangle x y 32 32)))))

(defun draw-object ()
  (declare (optimize speed))
  (let ((vbo (gl:gen-buffer)))
    (flet ((vbo-rectangle (pointer offset x y width height
                           &optional (u1 0.0) (v1 0.0) (u2 1.0) (v2 1.0))
             (let* ((w/2 (/ width 2.0))
                    (h/2 (/ height 2.0))
                    (x1 (- x w/2))
                    (x2 (+ x w/2))
                    (y1 (- y h/2))
                    (y2 (+ y h/2)))
               (macrolet
                   ;; we rewrite the macro here a bit to simplify type
                   ;; inference on the offset values
                   ((store-values (&rest v)
                      `(progn ,@(loop for i in v
                                      for j from 0
                                      collect `(setf (cffi:mem-aref pointer
                                                                    :float (+ offset ,j))
                                                     (float ,i 0.0))))))
                 (store-values u1 v2  x1 y1
                               u2 v2  x2 y1
                               u2 v1  x2 y2
                               u1 v1  x1 y2)))))
      (let ((components-per-vertex 4)
            (size-of-float 4)
            (vertices-per-sprite 4))
        (gl:bind-buffer :array-buffer (vbo o))
        (%gl:buffer-data :array-buffer
                         (* (/ (length (positions o)) 2)
                            vertices-per-sprite
                            components-per-vertex
                            size-of-float)
                         (cffi:null-pointer)
                         :stream-draw)
        (gl:with-mapped-buffer (p :array-buffer :write-only)
          ;; we specify the type for the array here too
          (loop with positions of-type (simple-array single-float (*)) = (positions o)
                with length = (length positions)
                for i below length by 2
                ;; and a type for offset
                for offset fixnum from 0 by (* components-per-vertex vertices-per-sprite)
                for x = (aref positions i)
                for y = (aref positions (1+ i))
                do (vbo-rectangle p offset x y 32 32)))
        ;; draw the object
        (%gl:tex-coord-pointer 2 :float
                               (* components-per-vertex size-of-float)
                               (cffi:null-pointer))
        (%gl:vertex-pointer 2 :float
                            (* components-per-vertex size-of-float)
                            (cffi:make-pointer (* 2 size-of-float)))
        (gl:enable-client-state :texture-coord-array)
        (gl:enable-client-state :vertex-array)
        (%gl:draw-arrays :quads 0 (* vertices-per-sprite
                                     (floor (length (positions o)) 2)))
        (gl:disable-client-state :vertex-array)
        (gl:disable-client-state :texture-coord-array))))
  (gl:bind-buffer :array-buffer 0))



(gl:with-mapped-buffer (p :array-buffer :read-write)
            (setf (boid-coords-buffer bs)
                  (%cl:create-buffer
                   *context*
                   '(:write-only :use-host-ptr)
                   (* 2 vertex-size +float4-octets+ maxcount)
                   p
                   (cffi:null-pointer)))
            (unless (zerop count)
              (progn
                (loop repeat count
                      for i from 0 by (* 4 (* 2 vertex-size))
                      do (let ((color (if (zerop i) *first-boid-color* *fg-color*)))
                           (apply #'set-array-vals p (+ i 0) origin)
                           (apply #'set-array-vals p (+ i 8) origin)
                           (apply #'set-array-vals p (+ i 4) color)
                           (apply #'set-array-vals p (+ i 12) color))))
              (ocl:with-mapped-buffer (p (car *command-queues*) vel (* 4 count) :write t)
                (loop repeat count
                      for i from 0 by 4
                      for a = (float (random +twopi+) 1.0)
                      for v = (float (+ 0.1 (random 0.1)) 1.0) ;; 1.0
                      do (set-array-vals p i (* v maxspeed (sin a))(* v maxspeed (cos a)) 0.0 0.0)))
              (ocl:with-mapped-buffer (p (car *command-queues*) life-buffer count :write t)
                (dotimes (i count) (setf (cffi:mem-aref p :float i) (float (if trig
                                                                               (max 0.01 (* (random (float (max 0.01 lifemult))) 8))
                                                                               (max 0.01 (* (+ 0.7 (random 0.2)) maxlife)))
                                                                           1.0))))
              (ocl:with-mapped-buffer (p (car *command-queues*) retrig-buffer (* 4 count) :write t)
                (dotimes (i count) (progn
                                     (setf (cffi:mem-aref p :int i) 0)
                                     (setf (cffi:mem-aref p :int (+ i 1)) -2) ;;; set next trigger-type to no obstacle
                                     (setf (cffi:mem-aref p :int (+ i 2)) 0)
                                     (setf (cffi:mem-aref p :int (+ i 3)) 0))))
              ;;        (new-obstacle win 100 300 20 :bs bs)
              ;;        (new-predator win 100 300 20 :bs bs)
;;; board-offset initialization: write dx,dy,distance,sep and coh vectors 
              (ocl:with-mapped-buffer (dx (car *command-queues*) board-dx max-offs-size :write t)
                (ocl:with-mapped-buffer (dy (car *command-queues*) board-dy max-offs-size :write t)
                  (ocl:with-mapped-buffer (d (car *command-queues*) board-dist max-offs-size :write t)
                    (ocl:with-mapped-buffer (sep (car *command-queues*) board-sep (* 4 max-offs-size) :write t)
                      (ocl:with-mapped-buffer (coh (car *command-queues*) board-coh (* 4  max-offs-size) :write t)
                        (ocl:with-mapped-buffer (obst (car *command-queues*) obstacle-board-buffer max-offs-size :write t)
                          (loop
                            for i from 0 for j from 0 by 4 for elem in *board-offsets*
                            for vcoh = (getf elem :coh-vec) for dist = (getf elem :dist) for vsep = (getf elem :sep-vec)
                            do (progn
                                 ;;                            (format t "~&~a ~a ~a ~a~%" vcoh vsep (getf elem :dx) (getf elem :dy))
                                 (setf (cffi:mem-aref dx :int i) (getf elem :dx)
                                       (cffi:mem-aref dy :int i) (getf elem :dy)
                                       (cffi:mem-aref d :float i) (getf elem :dist)
                                       (cffi:mem-aref obst :int i) 0)
                                 (apply #'set-array-vals sep j vsep)
                                 (apply #'set-array-vals coh j vcoh)))))))))))

(dotimes (i count)
  (setf (cffi:mem-aref p :float i)
        (float
         (if trig
             (max 0.01 (* (random (float (max 0.01 lifemult))) 8))
             (max 0.01 (* (+ 0.7 (random 0.2)) maxlife)))
         1.0)))







(setf (cffi:mem-aref p :float i) (float (if trig
                                                                               (max 0.01 (* (random (float (max 0.01 lifemult))) 8))
                                                                               (max 0.01 (* (+ 0.7 (random 0.2)) maxlife)))
                                                                           1.0))

(gl:enable-client-state :vertex-array)
(%gl:draw-arrays :quads 0 (* vertices-per-sprite
                             (floor (length (positions o)) 2)))
