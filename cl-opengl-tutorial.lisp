;;;; cl-opengl-tutorial.lisp

(in-package #:cl-opengl-tutorial)


#|
getting started

The first thing we need to do, is open a window, and set up some
restarts so that errors don't kill the connection if we don't want it
to. I also add some sbcl specific code to let slime keep responding
even while the main loop is running.  
|#

(defclass arrow-circle-vbo ()
  ((arrowhead-vbo :initform (car (gl:gen-buffers 1)) :accessor arrowhead-vbo)
   (arrowstem-vbo :initform (car (gl:gen-buffers 1)) :accessor arrowstem-vbo)
   (circle-vbo :initform (car (gl:gen-buffers 1)) :accessor circle-vbo)))

#|

(gl:delete-buffers (list (arrowhead-vbo *ac-vbo*)
                         (arrowstem-vbo *ac-vbo*)
                         (circle-vbo *ac-vbo*)))
|#

(defmethod init-fns ((w tutorial-window))
  "setup function before window creation"
  (format t "~&initializing vbos...~%")
  (setup-vbos))

(defclass arrow-circle ()
  ((radius :initform 1 :accessor radius)
   (angle :initform 0 :accessor angle)))

(defparameter *arrow-circles*
  (make-array 512 :element-type 'arrow-circle
              :initial-contents (loop for n below 512 collect (make-instance 'arrow-circle))))

;;; *arrow-circles*

(defparameter *ac-vbo* (make-instance 'arrow-circle-vbo))

;;; (arrowstem-vbo *ac-vbo*)

(defun get-circle-vertex-coords (rad num)
  (let* ((theta (/ (* 2 pi) num))
         (tang-factor (tan theta))
         (rad-factor (cos theta)))
;;    (format t "~a ~a" rad-factor tang-factor)
    (loop
       for x = 0 then (* (+ x1 (* -1 y1 tang-factor)) rad-factor)
       for y = rad then (* (+ y1 (* x1 tang-factor)) rad-factor)
       for x1 = x
       for y1 = y
       for i below num
       collect (vector x y 0.0d0))))

(defparameter *radius* (loop for x below 512 collect (+ 10 (random 20))))

;;; (get-circle-vertex-coords 1 32)

(defparameter *test* (gl:gen-buffer))
(defparameter *test* (gl:delete-buffers (list *test*)))

(defun setup-vbos (&key (num 512))
  (let* ((components-per-vertex 4)
         (size-of-float 4)
         (size (* num 2) ))
    (setf (arrowstem-vbo *ac-vbo*) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
    (%gl:buffer-data :array-buffer
                     (* size components-per-vertex size-of-float)
                     (cffi:null-pointer)
                     :stream-draw)
    (gl:bind-buffer :array-buffer 0)))

(defun dispose-vbos ()
  (format t "disposing vbos...~%")
  (gl:delete-buffers (list (arrowstem-vbo *ac-vbo*))))

(declaim (inline i>freq))
(defun i->freq (i)
  (declare (optimize speed)
           (type (unsigned-byte 32) i))
  (if (> i 256) (- i 512) i))

(defun calc-coords (angle)
  (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
  (gl:with-mapped-buffer (p :array-buffer :write-only)
    ;; we specify the type for the array here too
    (loop
      for i below 512
      with radius = 20
      for x-offs = (float (+ 25 (* (mod i 10) 50)))
      for y-offs = (float (+ 25 (* (floor i 10) 50)))
      for v-idx from i by 8
      ;; and a type for offset
      for x-target = (float (+ x-offs (* radius (cos (* (i->freq i) angle)))))
      for y-target = (float (+ y-offs (* radius (sin (* (i->freq i) angle)))))
      with z = 0.5
      with o = 0.2
          do (progn
               (setf (cffi:mem-aref p :float v-idx) (float (- (/ x-offs 120) 1)))
               (setf (cffi:mem-aref p :float (+ v-idx 1)) (float (/ y-offs 120)))
               (setf (cffi:mem-aref p :float (+ v-idx 2)) 0.0)
               (setf (cffi:mem-aref p :float (+ v-idx 3)) 0.0)
               (setf (cffi:mem-aref p :float (+ v-idx 4)) (float (- (/ x-target 120) 1)))
               (setf (cffi:mem-aref p :float (+ v-idx 5)) (float (/ y-target 120)))
               (setf (cffi:mem-aref p :float (+ v-idx 6)) 0.0)
               (setf (cffi:mem-aref p :float (+ v-idx 7)) 0.0))))
  (gl:bind-buffer :array-buffer 0))

#|

(let ((fft '(1 0.2 3 0.6))
      (angle 0.3))
  (loop
    with x = 0 with y = 0
    for i from 0
    for (l p) on fft by #'cddr
    do (progn
         (setf (aref result i) (incf x (* l (cos (* i angle)))))
         (setf (aref result (1+ i)) (incf y (* l (sin (* i angle))))))))

(multiple-value-setq)
|#

;;; (setup-vbos)
;;; (size 'single-float)

#|
  (gl:with-primitive :lines
    (gl:vertex 0 0 0)
    (gl:vertex 0.9 0 0)
    )
  (gl:with-primitive :triangles
    (gl:color 1 1 1)
    (gl:vertex 0.9 0.04 0)
    (gl:vertex 1 0 0)
    (gl:vertex 0.9 -0.04 0))
|#

(defun draw-arrow (radius angle)
;;  (gl:line-width 2)
;;  (gl:load-identity)
;;  (gl:push-matrix)
  ;; (gl:rotate angle 0 0 1)
  ;; (gl:scale radius radius 0)
  ;; (gl:with-primitive :lines
  ;;   (gl:vertex 0 0 0)
  ;;   (gl:vertex 0.9 0 0)
  ;;   )
  ;; (gl:with-primitive :triangles
  ;;   (gl:color 1 1 1)
  ;;   (gl:vertex 0.9 0.04 0)
  ;;   (gl:vertex 1 0 0)
  ;;   (gl:vertex 0.9 -0.04 0))
  ;; (gl:pop-matrix)
  )

(defun draw-arrow-stems (&optional (count 512))
  (let* ((components-per-vertex 4)
         (vertices-per-sprite (* 2 components-per-vertex))
         (size-of-float 4))
    (gl:line-width 1)
    (gl:color 1 1 0 0)
    (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
    (%gl:vertex-pointer 2 :float
                        (* 1 components-per-vertex size-of-float)
                        (cffi:make-pointer (* 0 size-of-float)))
;;;  (gl:enable-client-state :texture-coord-array)
    (gl:enable-client-state :vertex-array)
    (%gl:draw-arrays :lines 0 (* 2 count))
    (gl:disable-client-state :vertex-array)
    (gl:bind-buffer :array-buffer 0)))

(defparameter *angle* 0)

(defparameter *init* t)

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  (calc-coords (incf *angle* 0.003))
  (draw-arrow-stems 20)
  ;; finish the frame
  (gl:flush))



(defun main-loop ()
  (let ((win (make-instance 'tutorial-window :width 640 :height 480)))
  (unwind-protect
       (setf *win* win)
    (format t "~&displaying window...~%")
    (setf *init* t)
    (glut:display-window win)
    (format t "~&closing window...~%")
    (dispose-vbos))))


;;; (main-loop)

#|


screenshot with triangle Much better.

Note that the way this code draws triangles (using gl:with-primitive, gl:color, gl:vertex, etc., known as "immediate mode") is one of the least efficient ways to draw things on modern hardware, so it should be avoided for anything more complex than a few triangles.

|#


