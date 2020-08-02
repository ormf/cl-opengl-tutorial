;;;; cl-opengl-tutorial.lisp

(in-package #:cl-opengl-tutorial)


#|
getting started

The first thing we need to do, is open a window, and set up some
restarts so that errors don't kill the connection if we don't want it
to. I also add some sbcl specific code to let slime keep responding
even while the main loop is running.  
|#

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0) ;;  hmm, i need to add an option to the syntax
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)   ;;  highlighting to show changed lines...
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0)) ;; (this one changed too)
  ;; finish the frame
  (gl:flush))

(defun main-loop ()
  (let ((win (make-instance 'tutorial-window :width 640 :height 480)))
  (unwind-protect
       (setf *win* win)
    (format t "~&displaying window...~%")
    (glut:display-window win)
    (format t "~&closing window...~%"))))


;;; (main-loop)

#|


screenshot with triangle Much better.

Note that the way this code draws triangles (using gl:with-primitive, gl:color, gl:vertex, etc., known as "immediate mode") is one of the least efficient ways to draw things on modern hardware, so it should be avoided for anything more complex than a few triangles.

|#


