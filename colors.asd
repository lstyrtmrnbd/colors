;;;; colors.asd

(asdf:defsystem #:colors
  :description "colors"
  :author "t"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:3d-vectors
               #:png
               #:trivial-project-pathname)
  :components ((:file "package")
               (:file "colors")))
