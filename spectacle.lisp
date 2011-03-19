;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.
;;;
;;; A simple viewer for opticl images
;;;
;;; Note: Originally based on Troels' Henriksen's
;;; clim-demo:image-viewer
;;;
(in-package :spectacle)

(defclass spectacle-gadget (value-gadget)
  ((image-transform :accessor image-transform :initarg :image-transform)
   (image-transformed-image :accessor image-transformed-image :initarg :image-transformed-image)
   (image-pattern :accessor image-pattern :initarg :image-pattern))
  (:default-initargs :value nil
    :image-transform nil
    :image-transformed-image nil
    :image-pattern nil))

(defmethod (setf gadget-value) :after (new-value (gadget spectacle-gadget)
                                                 &key &allow-other-keys)
  (handle-repaint gadget (or (pane-viewport-region gadget)
                             (sheet-region gadget))))

(defclass spectacle-pane (spectacle-gadget basic-gadget) ())

(define-application-frame spectacle ()
  ()
  (:menu-bar t)
  (:panes
   (viewer (make-pane 'spectacle-pane :background +black+ :foreground +white+))
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100
               :min-width 200))
  (:layouts
   (default (vertically ()
              (4/5 (labelling (:label "Opticl Image") viewer))
              (1/5 interactor)))))

(defmethod handle-repaint ((pane spectacle-pane) region)
  (declare (ignore region))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)    
    (clim:draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  (with-accessors ((image gadget-value)
                   (transform image-transform)
                   (transformed-image image-transformed-image)
                   (pattern image-pattern))
      pane
    (when image
      (unless pattern
        (setf transformed-image (if transform
                                    (transform-image image transform)
                                    image))
        (setf pattern (rgb-image-to-climi-rgb-pattern transformed-image)))
      (with-image-bounds (image-height image-width) transformed-image
        (change-space-requirements pane :height image-height :width image-width)
        (let ((bounding-rectangle-height (bounding-rectangle-height pane))
              (bounding-rectangle-width (bounding-rectangle-width pane)))
          (handler-case (clim:draw-pattern*
                         pane pattern
                         (/ (- bounding-rectangle-width image-width) 2)
                         (/ (- bounding-rectangle-height image-height) 2))))))))

(defun spectacle (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'spectacle)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Spectacle")
        (run))))

(defun rgb-image-to-climi-rgb-pattern (image)
  (with-image-bounds (y x) image
    (let ((cimg (make-32-bit-gray-image y x)))
      (set-pixels (i j) cimg
        (multiple-value-bind (r g b)
            (pixel image i j)
          (+ (ash r 0) (ash g 8) (ash b 16))))
      (make-instance 'clim-internals::rgb-pattern
                     :image (make-instance 'clim-internals::rgb-image
                                           :height y
                                           :width x
                                           :data cimg)))))

(define-spectacle-command (com-quit :name t :menu t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(define-spectacle-command (com-load-image :name t :menu t)
    ((image-pathname 'pathname
                     :default (user-homedir-pathname) :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'viewer))
        (img (read-image-file image-pathname)))
    (with-accessors ((image gadget-value)
                       (transform image-transform)
                       (pattern image-pattern))
        viewer
      (setf transform nil
            pattern nil
            image img))))

(define-spectacle-command (zoom :name t :menu t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (let ((bounding-rectangle-height (bounding-rectangle-height viewer))
          (bounding-rectangle-width (bounding-rectangle-width viewer)))
      (with-accessors ((image gadget-value)
                       (transform image-transform)
                       (pattern image-pattern))
          viewer
        (setf pattern nil)
        (setf transform 
              (with-image-bounds (oldy oldx) image
                (let ((scale (apply #'min
                                    (append (list (/ bounding-rectangle-height oldy))
                                            (list (/ bounding-rectangle-width oldx))))))
                  (make-affine-transformation :y-scale scale :x-scale scale))))
        (setf image image)))))
