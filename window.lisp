;;; 
;;; window.lisp
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

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Swank.Live: Continue")))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

(defclass tutorial-window (glut:window) 
  ()
  (:default-initargs :width 640 :height 480 :title "OpenGL Tutorial"
                     :mode '(:double :rgb :depth :multisample)))

(defparameter *win* nil) ;;; global accessor to window instance

(defmethod glut:display-window :before ((w tutorial-window))
  "setup function before window creation"
  (format t "~&setting cursor...~%")
;;  (glut:set-cursor :cursor-none)
  ;; (gl:clear-depth 1)
  ;; (gl:shade-model :smooth)
  ;; (gl:clear-color 0 0 0 0)
  ;; (gl:enable :depth-test :multisample)
  ;; (gl:depth-func :lequal)
  ;; (gl:hint :perspective-correction-hint :nicest)
  )

(defun draw ()
  (gl:clear :color-buffer-bit)
  (gl:flush))

(defmethod glut:close ((window tutorial-window))
  "cleanup on window close"
  (format t "window closed, cleaning up...~%"))

(defmethod glut:display ((window tutorial-window))
  "central display routine."
  (with-slots (width height) window
;;     (gl:load-identity)
;;     (gl:enable :blend)
;;     (gl:blend-func :src-alpha :one)
;;     (gl:point-size 1)
;;     (gl:line-width 1)
;;     (gl:disable :depth-test)
;;     (gl:depth-func :lequal)
;;     (gl:matrix-mode :projection)
;;     (gl:load-identity)
;;     ;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) -1 1)
;; ;;;    (gl:ortho 0 width 0 height -1 1)
;;     (gl:scale 1 1 1)
;; ;;;    (gl:translate (* (- 1 gl-scale) gl-width) (* (- 1 gl-scale) gl-height) 0.0)
;;     (gl:matrix-mode :modelview)
    (update-swank)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (continuable
      (draw))
    (glut:swap-buffers)
    (gl:finish)))

(defmethod glut:idle ((window tutorial-window))
  (glut:post-redisplay))

(defmethod glut:mouse ((window tutorial-window) button state x y)
  "react to mouse events."
  (continuable
    (case button
      (:left-button
       (when (eql state :down)))
      (:wheel-down (when (and (equal (list :active-ctrl) (glut:get-modifiers))
                              (eql state :down))))
      (:wheel-up (when (and (equal (list :active-ctrl) (glut:get-modifiers))
                            (eql state :down)))))    ))

(defmethod glut:reshape ((window tutorial-window) width height)
  "react to window resize."
    (setf (glut:width window) width
          (glut:height window) height)
  (glut:reshape-window width height)
;;;  (set-viewport window)
  )

(defmethod glut:keyboard ((window tutorial-window) key x y)
  "react to keyboard events."
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

