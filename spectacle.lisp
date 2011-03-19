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
  ((transform-parameters :accessor transform-parameters :initarg :transform-parameters)
   (transform :accessor transform :initarg :transform)
   (transformed-image :accessor transformed-image :initarg :transformed-image)
   (image-pattern :accessor image-pattern :initarg :image-pattern))
  (:default-initargs :value nil
    :transform-parameters (list 1d0 1d0 0d0 0d0 0d0 0d0 0d0)
    :transform nil
    :transformed-image nil
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
                   (transform-parameters transform-parameters)
                   (transform transform)
                   (transformed-image transformed-image)
                   (pattern image-pattern))
      pane
    (when image
      (unless transform
        (destructuring-bind (y-scale x-scale y-shift x-shift y-shear x-shear rotate)
            transform-parameters
          (setf transform
                (make-affine-transformation :y-scale y-scale
                                            :x-scale x-scale
                                            :y-shift y-shift
                                            :x-shift x-shift
                                            :y-shear y-shear
                                            :x-shear x-shear
                                            :theta rotate)
                pattern nil)))
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
                       (transform transform)
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
                       (transform-parameters transform-parameters)
                       (transform transform)
                       (pattern image-pattern))
          viewer
        (setf pattern nil)
        (with-image-bounds (oldy oldx) image
          (let ((scale (apply #'min
                              (append (list (/ bounding-rectangle-height oldy))
                                      (list (/ bounding-rectangle-width oldx))))))
            (setf (first transform-parameters) scale
                  (second transform-parameters) scale
                  transform nil)))
        (setf image image)))))

(define-spectacle-command (redraw  :name t :menu t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (handle-repaint viewer (sheet-region viewer))))

(define-spectacle-command (set-angle  :name t :menu t)
    ((degrees 'number :default 0 :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (pattern image-pattern))
        viewer
      (setf (seventh transform-parameters) 
            (mod (* pi degrees (/ 180)) (* 2 pi))
            transform nil
            image image))))
