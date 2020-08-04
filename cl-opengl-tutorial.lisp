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
  ((arrowhead-vbo :initform nil :accessor arrowhead-vbo)
   (arrowstem-vbo :initform nil :accessor arrowstem-vbo)
   (circle-vbo :initform nil :accessor circle-vbo)))

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
    (append
     (loop
       for x = rad then (* (+ x1 (* -1 y1 tang-factor)) rad-factor)
       for y = 0 then (* (+ y1 (* x1 tang-factor)) rad-factor)
       for x1 = x
       for y1 = y
       for i below num
       collect (list (float x 1.0) (float y 1.0)))
     `((,(float rad 1.0) 0)))))

(defparameter *unit-circle-32* (get-circle-vertex-coords 1 32))
(defparameter *unit-circle-16* (get-circle-vertex-coords 1 16))

(defparameter *radius* (loop for x below 512 collect (+ 10 (random 20))))

;;; (get-circle-vertex-coords 1 32)

(defparameter *test* (gl:gen-buffer))
(defparameter *test* (gl:delete-buffers (list *test*)))

(defun setup-vbos (&key (num 512))
  (let* ((components-per-vertex 4)
         (size-of-float 4)
         (vertices-per-sprite 2))
    (setf (arrowstem-vbo *ac-vbo*) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
    (%gl:buffer-data :array-buffer
                     (* num vertices-per-sprite components-per-vertex size-of-float)
                     (cffi:null-pointer)
                     :stream-draw)
    (setf vertices-per-sprite 64)
    (gl:bind-buffer :array-buffer 0)
    (setf (circle-vbo *ac-vbo*) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (circle-vbo *ac-vbo*))
    (%gl:buffer-data :array-buffer
                     (* num vertices-per-sprite components-per-vertex size-of-float)
                     (cffi:null-pointer)
                     :stream-draw)
    (gl:bind-buffer :array-buffer 0)))

(defun dispose-vbos ()
  (format t "disposing vbos...~%")
  (gl:delete-buffers (list (arrowstem-vbo *ac-vbo*)
                           (circle-vbo *ac-vbo*))))

(declaim (inline i>freq))
(defun i->freq (i)
  (declare (optimize speed)
           (type (unsigned-byte 32) i))
  (if (> i 256) (- i 512) i))

(defun calc-coords (angle)
  (declare (optimize speed))
  (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
  (gl:with-mapped-buffer (p :array-buffer :write-only)
    ;; we specify the type for the array here too
    (loop
      for i below 512
      with radius = 20.0
      with num-vertices-per-sprite = 2
      with size-of-vertices = 4
      for x-offs = (float (+ 25 (* (mod i 10) 50)))
      for y-offs = (float (+ 25 (* (floor i 10) 50)))
      for v-idx from i by 8
      ;; and a type for offset
      for x-target = (float (+ x-offs (* radius (cos (* (i->freq i) angle)))))
      for y-target = (float (+ y-offs (* radius (sin (* (i->freq i) angle)))))
      with z = 0.0
      with o = 0.0
          do (progn
               (setf (cffi:mem-aref p :float v-idx) (float x-offs))
               (setf (cffi:mem-aref p :float (+ v-idx 1)) (float y-offs))
               (setf (cffi:mem-aref p :float (+ v-idx 2)) z)
               (setf (cffi:mem-aref p :float (+ v-idx 3)) o)
               (setf (cffi:mem-aref p :float (+ v-idx 4)) (float x-target))
               (setf (cffi:mem-aref p :float (+ v-idx 5)) (float y-target))
               (setf (cffi:mem-aref p :float (+ v-idx 6)) z)
               (setf (cffi:mem-aref p :float (+ v-idx 7)) o))))
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer (circle-vbo *ac-vbo*))
  (gl:with-mapped-buffer (p :array-buffer :write-only)
    ;; we specify the type for the array here too
    (loop
      for i below 512
      with num-vertices-per-sprite = 32
      with size-of-vertices = 4
      with radius = 20
      for x-offs = (float (+ 25 (* (mod i 10) 50)))
      for y-offs = (float (+ 25 (* (floor i 10) 50)))
      for v-start from i by (* size-of-vertices num-vertices-per-sprite)
      ;; and a type for offset
      do (loop
           for (p1 p2) on *unit-circle-16*
           while p2
           with z = 0.0
           with o = 0.0
           for v-idx from v-start by 8
           do (progn
                (setf (cffi:mem-aref p :float v-idx) (+ x-offs (* radius (elt p1 0))))
                (setf (cffi:mem-aref p :float (+ v-idx 1)) (+ y-offs (* radius (elt p1 1))))
                (setf (cffi:mem-aref p :float (+ v-idx 2)) z)
                (setf (cffi:mem-aref p :float (+ v-idx 3)) o)
                (setf (cffi:mem-aref p :float (+ v-idx 4)) (+ x-offs (* radius (elt p2 0))) )
                (setf (cffi:mem-aref p :float (+ v-idx 5)) (+ y-offs (* radius (elt p2 1))))
                (setf (cffi:mem-aref p :float (+ v-idx 6)) z)
                (setf (cffi:mem-aref p :float (+ v-idx 7)) o)))))
  (gl:bind-buffer :array-buffer 0))

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
         (vertices-per-sprite 2)
         (size-of-float 4))
    (gl:line-width 2)
    (gl:color 1 1 1 0)
    (gl:bind-buffer :array-buffer (arrowstem-vbo *ac-vbo*))
    (%gl:vertex-pointer vertices-per-sprite :float
                        (* 1 components-per-vertex size-of-float)
                        (cffi:make-pointer (* 0 size-of-float)))
;;;  (gl:enable-client-state :texture-coord-array)
    (gl:enable-client-state :vertex-array)
    (%gl:draw-arrays :lines 0 (* vertices-per-sprite count))
    (gl:disable-client-state :vertex-array)
    (gl:bind-buffer :array-buffer 0)))

(defun draw-circles (&optional (count 512))
  (let* ((components-per-vertex 4)
         (vertices-per-sprite 64)
         (size-of-float 4))
    (gl:line-width 1)
    (gl:color 0.6 0.6 1 0)
    (gl:bind-buffer :array-buffer (circle-vbo *ac-vbo*))
    (%gl:vertex-pointer 2 :float
                        (* 1 components-per-vertex size-of-float)
                        (cffi:make-pointer (* 0 size-of-float)))
;;;  (gl:enable-client-state :texture-coord-array)
    (gl:enable-client-state :vertex-array)
    (%gl:draw-arrays :lines 0 (* count vertices-per-sprite))
    (gl:disable-client-state :vertex-array)
    (gl:bind-buffer :array-buffer 0)))

(defparameter *angle* 0)

(defparameter *init* t)

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  (calc-coords (incf *angle* 0.003))
  (draw-arrow-stems 512)
  (draw-circles 512)
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


