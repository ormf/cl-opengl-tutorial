;;;; cl-opengl-tutorial.asd

(asdf:defsystem #:cl-opengl-tutorial
  :description "Describe cl-opengl-tutorial here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:cl-opengl ;;; #:safe-queue
               #:cl-glut #:cl-glu
               #:png)
  :serial t
  :components ((:file "package")
               (:file "window")
               (:file "cl-opengl-tutorial")
               (:file "intro")
               (:file "tut02")
               (:file "tut03")
               (:file "tut04")
               (:file "tut05")
               (:file "tut06")
               (:module "redbook"
                :serial t
                :components
                ((:file "01-triangles")))))
