;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :spectacle
  (:use #:clim-lisp #:clim #:opticl)
  (:export #:spectacle)
  (:shadowing-import-from #:opticl
                          #:draw-line
                          #:draw-line*
                          #:draw-circle
                          #:draw-circle*
                          #:draw-rectangle
                          #:draw-rectangle*))

