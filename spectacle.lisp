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
   (lock-x-and-y-scale :accessor lock-x-and-y-scale :initarg :lock-x-and-y-scale)
   (transformed-image :accessor transformed-image :initarg :transformed-image)
   (image-pattern :accessor image-pattern :initarg :image-pattern))
  (:default-initargs :value nil
    :transform-parameters #(1d0 1d0 0d0 0d0 0d0 0d0 0d0)
    :lock-x-and-y-scale t
    :transform nil
    :transformed-image nil
    :image-pattern nil))

(defmethod (setf gadget-value) :after (new-value (gadget spectacle-gadget)
                                                 &key &allow-other-keys)
  (handle-repaint gadget (or (pane-viewport-region gadget)
                             (sheet-region gadget))))

(defclass spectacle-pane (spectacle-gadget basic-gadget) ())

(defun y-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters y-scale-param) scale)
          (let ((x-scale-slider (find-pane-named *application-frame* 'x-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value x-scale-slider) scale
                    (elt transform-parameters x-scale-param) scale)))
          (setf (elt transform-parameters y-scale-param) scale
                transform nil)
          (handle-repaint viewer (sheet-region viewer)))))))

(defun x-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters x-scale-param) scale)
          (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value y-scale-slider) scale
                    (elt transform-parameters y-scale-param) scale)))
          (setf (elt transform-parameters x-scale-param) scale
                transform nil)
          (handle-repaint viewer (sheet-region viewer)))))
    (handle-repaint viewer (sheet-region viewer))))

(defun theta-callback (gadget degrees)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 6)
            (rads (mod (* pi degrees (/ 180)) (* 2 pi))))
        (unless (equal (elt transform-parameters param) rads)
          (setf (elt transform-parameters param) rads
                transform nil))))
    (handle-repaint viewer (sheet-region viewer))))

(defun y-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 4))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil))))
    (handle-repaint viewer (sheet-region viewer))))

(defun x-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 5))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil))))
    (handle-repaint viewer (sheet-region viewer))))


(define-application-frame spectacle ()
  ()
  (:menu-bar t)
  (:panes
   (viewer (make-pane 'spectacle-pane
                      :background +black+
                      :foreground +white+
                      :width 500))
   (lock-scale :toggle-button
               :label "Lock X and Y Scale"
               :value t
               :value-changed-callback
               (lambda (gadget value)
                 (declare (ignore gadget))
                 (let ((viewer (find-pane-named *application-frame* 'viewer)))
                   (with-accessors ((lock-x-and-y-scale lock-x-and-y-scale))
                       viewer
                     (setf lock-x-and-y-scale value)))))
   (y-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'y-scale-callback
            :value-changed-callback 'y-scale-callback)
   (x-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'x-scale-callback
            :value-changed-callback 'x-scale-callback)
   (theta :slider
          :min-value -180
          :max-value 180
          :decimal-places 1
          :value 0
          :show-value-p t
          :orientation :horizontal
          :drag-callback #'theta-callback
          :value-changed-callback 'theta-callback)
   (y-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'y-shear-callback
            :value-changed-callback 'y-shear-callback)
   (reset-button :push-button
                 :label "Reset Parameters"
                 :activate-callback (lambda (gadget)
                                      (declare (ignore gadget))
                                      (reset-it)))
   (x-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'x-shear-callback
            :value-changed-callback 'x-shear-callback)
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100))
  (:layouts
   (default (vertically ()
              (4/5 (horizontally ()
                     (4/5 viewer)
                     (1/5 (vertically ()
                            lock-scale
                            (labelling (:label "Y Scale")
                              y-scale)
                            (labelling (:label "X Scale")
                              x-scale)
                            (labelling (:label "Theta")
                              theta)
                            (labelling (:label "Y Shear")
                              y-shear)
                            (labelling (:label "X Shear")
                              x-shear)
                            reset-button))))
              (1/5 interactor)))))

(defun reset-sliders ()
  (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale))
        (x-scale-slider (find-pane-named *application-frame* 'x-scale))
        (theta-slider (find-pane-named *application-frame* 'theta))
        (y-shear-slider (find-pane-named *application-frame* 'y-shear))
        (x-shear-slider (find-pane-named *application-frame* 'x-shear)))
    (setf (gadget-value y-scale-slider) 1.0d0)
    (setf (gadget-value x-scale-slider) 1.0d0)
    (setf (gadget-value y-shear-slider) 0.0d0)
    (setf (gadget-value x-shear-slider) 0.0d0)
    (setf (gadget-value theta-slider) 0.0d0)))

(defun reset-transform-parameters (transform-parameters)
  (setf (elt transform-parameters 0) 1d0
        (elt transform-parameters 1) 1d0
        (elt transform-parameters 4) 0d0
        (elt transform-parameters 5) 0d0
        (elt transform-parameters 6) 0d0))

(defun opticl-image-to-climi-rgb-pattern (image)
  (etypecase image
    (8-bit-rgb-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-rgb-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (+ (ash r 0) (ash g 8) (ash b 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))
    (8-bit-rgba-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-rgba-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (+ (ash r 0) (ash g 8) (ash b 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))
    (16-bit-rgb-image
     (locally (declare (optimize (speed 3))
                       (type 16-bit-rgb-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (+ (ash (ash r -8) 0) (ash (ash g -8) 8) (ash (ash b -8) 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))
    (16-bit-rgba-image
     (locally (declare (optimize (speed 3))
                       (type 16-bit-rgba-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (+ (ash (ash r -8) 0) (ash (ash g -8) 8) (ash (ash b -8) 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))
    (8-bit-gray-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-gray-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (let ((k (pixel image i j)))
               (+ (ash k 0) (ash k 8) (ash k 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))
    (1-bit-gray-image
     (locally (declare (optimize (speed 3))
                       (type 1-bit-gray-image image))
       (with-image-bounds (y x) image
         (let ((cimg (make-32-bit-gray-image y x)))
           (declare (type 32-bit-gray-image cimg))
           (set-pixels (i j) cimg
             (let ((k (* 255 (pixel image i j))))
               (+ (ash k 0) (ash k 8) (ash k 16))))
           (make-instance 'clim-internals::rgb-pattern
                          :image (make-instance 'clim-internals::rgb-image
                                                :height y
                                                :width x
                                                :data cimg))))))))

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
            (coerce transform-parameters 'list)
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
        (setf pattern (opticl-image-to-climi-rgb-pattern transformed-image)))
      (with-image-bounds (image-height image-width) transformed-image
        (change-space-requirements pane :height image-height :width image-width)
        (let ((bounding-rectangle-height (bounding-rectangle-height pane))
              (bounding-rectangle-width (bounding-rectangle-width pane)))
          (clim:draw-pattern*
           pane pattern
           (/ (- bounding-rectangle-width image-width) 2)
           (/ (- bounding-rectangle-height image-height) 2)))))))

(defun spectacle (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'spectacle)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Spectacle")
        (run))))

(define-spectacle-command (com-quit :name t :menu t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(define-spectacle-command (com-load-image :name t :menu t)
    ((image-pathname 'pathname
                     :default (user-homedir-pathname)
                     :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'viewer))
        (img (read-image-file image-pathname)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (pattern image-pattern))
        viewer
      (reset-sliders)
      (reset-transform-parameters transform-parameters)
      (setf transform nil
            pattern nil
            image img))))

(define-spectacle-command (zoom :name t :menu t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale))
          (x-scale-slider (find-pane-named *application-frame* 'x-scale))
          (bounding-rectangle-height (bounding-rectangle-height viewer))
          (bounding-rectangle-width (bounding-rectangle-width viewer)))
      (with-accessors ((image gadget-value)
                       (transform-parameters transform-parameters)
                       (transform transform)
                       (pattern image-pattern))
          viewer
        (setf pattern nil)
        (when image
          (with-image-bounds (oldy oldx) image
            (let ((scale (apply #'min
                                (append (list (/ bounding-rectangle-height oldy))
                                        (list (/ bounding-rectangle-width oldx))))))
              (setf (gadget-value y-scale-slider) scale
                    (gadget-value x-scale-slider) scale)
              (setf (elt transform-parameters 0) scale
                    (elt transform-parameters 1) scale
                    transform nil)))
          (setf image image))))))

(defun reset-it ()
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (with-accessors ((image gadget-value)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (pattern image-pattern))
        viewer
      (reset-sliders)
      (reset-transform-parameters transform-parameters)
      (setf transform nil
            pattern nil))
    (handle-repaint viewer (sheet-region viewer))))

(define-spectacle-command (reset :name t :menu t)
    ()
  (reset-it))

(define-spectacle-command (redraw :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'viewer)))
    (handle-repaint viewer (sheet-region viewer))))

