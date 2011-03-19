
(asdf:defsystem :spectacle
  :name "spectacle"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (mcclim opticl)
  :serial t
  :components
  ((:file "package")
   (:file "spectacle")))
